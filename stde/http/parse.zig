const std = @import("std");
const testing = std.testing;

pub const HttpParserErrorPartial = error {
    InvalidMethodChar,
    MethodNameTooLong,
    BadVersionNewline,
    BadHttpVersion,
    InvalidHeaderNameChar,
    HeaderNameTooLong,
    NoLineFeedAfterCarriageReturn,
};
pub const HttpParserErrorComplete = error {
    InvalidMethodChar,
    MethodNameTooLong,
    BadVersionNewline,
    BadHttpVersion,
    InvalidHeaderNameChar,
    HeaderNameTooLong,
    NoLineFeedAfterCarriageReturn,
    Incomplete,
};

pub const HttpParserOptions = struct {
    // The type of the first argument to pass to a callback function.
    CallbackContext: type,
    /// Configures a parser to support parsing HTTP in a sequence of partial blocks.
    partialData: bool,
    /// The maximum number of chars to allow for an HTTP Method
    optionalMaxMethod: ?comptime_int,
    /// The maximum number of chars to allow for an HTTP Header name
    optionalMaxHeaderName: ?comptime_int,
};

pub const DefaultMaxMethod = 40;
pub const DefaultMaxHeaderName = 70;

pub const HttpOneShotParser = HttpParserGeneric(.{
    .partialData = false,
    .optionalMaxMethod = DefaultMaxMethod,
    .optionalMaxHeaderName = DefaultMaxHeaderName,
});
pub const HttpStreamParser = HttpParserGeneric(.{
    .partialData = true,
    .optionalMaxMethod = DefaultMaxMethod,
    .optionalMaxHeaderName = DefaultMaxHeaderName,
});

pub const HttpParserEvent = enum {
    method,
    uri,
    headerName,
    headerValue,
};

pub fn HttpParserGeneric(comptime options_: HttpParserOptions) type { return struct {

    pub const options = options_;
    pub const Callback = fn(ctx: options.CallbackContext, event: HttpParserEvent, data: []const u8) void;
    pub const PartialCallback = fn(ctx: options.CallbackContext, event: HttpParserEvent, data: []const u8) void;
    pub const Error = if (options.partialData)
        HttpParserErrorPartial else HttpParserErrorComplete;
    const Self = @This();

    pub const OptionArgs = if (options.partialData) struct {
        partialCallback: PartialCallback,
    } else struct {
    };

    const State = enum {
        method,
        uri,
        versionAndNewline,
        headerInitial,
        headerName,
        headerSkipWhitespace,
        headerValue,
        lastNewline,
        done,
    };
    callbackContext: options.CallbackContext,
    callback: Callback,
    optionArgs: OptionArgs,
    state: State,
    stateData: union {
        offset32: u32,
    },

    pub fn init(callbackContext: options.CallbackContext, callback: Callback, optionArgs: OptionArgs) Self {
        return Self {
            .callbackContext = callbackContext,
            .callback = callback,
            .optionArgs = optionArgs,
            .state = State.method,
            .stateData = undefined
        };
    }

    pub fn parse(self: *Self, data: []const u8) !usize {
        var remaining = data;
        loopBlock: { while (remaining.len > 0) {
            const method = switch (self.state) {
                .method => parseMethod,
                .uri => parseUri,
                .versionAndNewline => parseVersionAndNewline,
                .headerInitial => parseHeaderInitial,
                .headerName => parseHeaderName,
                .headerSkipWhitespace => parseHeaderWhitespace,
                .headerValue => parseHeaderValue,
                .lastNewline => parseLastNewline,
                .done => break :loopBlock,
            };
            //std.debug.warn("parsing '{}'\n", .{remaining});
            const parsedLen = try method(self, remaining);
            std.debug.assert(parsedLen > 0);
            remaining = remaining[parsedLen..];
        }}
        return @ptrToInt(remaining.ptr) - @ptrToInt(data.ptr);
    }
    fn parseMethod(self: *Self, data: []const u8) Error!usize {
        for (data) |c, i| {
            if (c == ' ') {
                self.callback(self.callbackContext, .method, data[0..i]);
                self.state = .uri;
                return i + 1;
            }
            if (!validTokenChar(c))
                return Error.InvalidMethodChar;
            // TODO: i is NOT all the data if partial is supported
            if (options.optionalMaxMethod) |maxMethod| {
                if (i >= maxMethod)
                    return Error.MethodNameTooLong;
            }
        }
        if (comptime options.partialData) {
            self.optionArgs.partialCallback(self.callbackContext, .method, data);
            return data.len;
        }
        return Error.Incomplete;
    }
    /// grammar here: https://tools.ietf.org/html/rfc3986#section-3
    ///     URI       = [ scheme ":" hier-part ] [ "?" query ] [ "#" fragment ]
    ///     hier-part = "//" authority path-abempty
    ///               / path-absolute
    ///               / path-rootless
    ///               / path-empty
    fn parseUri(self: *Self, data: []const u8) Error!usize {
        for (data) |c, i| {
            if (c == ' ') {
                self.callback(self.callbackContext, .uri, data[0..i]);
                self.state = .versionAndNewline;
                self.stateData = .{ .offset32 = 0 };
                return i + 1;
            }
            // TODO: check if there is a maximum URI and/or if the URI char is valid
        }
        if (comptime options.partialData) {
            self.optionArgs.partialCallback(self.callbackContext, .uri, data);
            return data.len;
        }
        return Error.Incomplete;
    }
    const HTTP_VERSION_AND_NEWLINE = "HTTP/1.1\r\n";
    fn parseVersionAndNewline(self: *Self, data: []const u8) Error!usize {
        const needed = HTTP_VERSION_AND_NEWLINE.len - self.stateData.offset32;
        if (data.len < needed) {
            if (!memcmp(u8, HTTP_VERSION_AND_NEWLINE[self.stateData.offset32..].ptr, data.ptr, data.len)) {
                return error.BadVersionNewline;
            }
            self.stateData.offset32 += @intCast(u32, data.len);
            return data.len;
        }

        if (!std.mem.eql(u8, HTTP_VERSION_AND_NEWLINE[self.stateData.offset32..], data[0..needed]))
            return error.BadVersionNewline;
        self.state = .headerInitial;
        return needed;
    }
    fn parseHeaderInitial(self: *Self, data: []const u8) Error!usize {
        if (data[0] == '\r') {
            self.state = .lastNewline;
            return @as(usize, 1);
        }
        self.state = .headerName;
        return self.parseHeaderName(data); // need to call directly because we haven't consumed any data
    }
    fn parseHeaderName(self: *Self, data: []const u8) Error!usize {
        for (data) |c, i| {
            if (c == ':') {
                self.callback(self.callbackContext, .headerName, data[0..i]);
                self.state = .headerSkipWhitespace;
                return i + 1;
            }
            if (!validTokenChar(c))
                return Error.InvalidHeaderNameChar;
            // TODO: i is NOT all the data if partial is supported
            if (options.optionalMaxHeaderName) |maxHeaderName| {
                if (i >= maxHeaderName)
                    return Error.HeaderNameTooLong;
            }
        }
        if (comptime options.partialData) {
            self.optionArgs.partialCallback(self.callbackContext, .headerName, data);
            return data.len;
        }
        return Error.Incomplete;
    }
    fn parseHeaderWhitespace(self: *Self, data: []const u8) Error!usize {
        for (data) |c, i| {
            if (c != ' ' and c != '\t') {
                // self.headerValueState = noNewline???
                self.state = .headerValue;
                return i;
            }
            // TODO: limit the amount of whitespace?? Prevent DOS
        }
        return data.len;
    }
    fn parseHeaderValue(self: *Self, data: []const u8) Error!usize {
        {var i : usize= 0; while (i + 1 < data.len) : (i += 1) {
            if ('\r' == data[i] and '\n' == data[i+1]) {
                self.callback(self.callbackContext, .headerValue, data[0..i]);
                self.state = .headerInitial;
                return i + 2;
            }
            // TODO: limit the length
        }}
        if (comptime options.partialData) {
            self.optionArgs.partialCallback(self.callbackContext, .headerValue, data);
            return data.len;
        }
        return Error.Incomplete;
    }
    fn parseLastNewline(self: *Self, data: []const u8) Error!usize {
        if (data[0] != '\n')
            return error.NoLineFeedAfterCarriageReturn;
        self.state = .done;
        return 1;
    }
};}


const NoContext = struct { };
fn testCallback(context: NoContext, event: HttpParserEvent, data: []const u8) void {
    std.debug.warn("test got data '{}'\n", .{data});
}
fn testPartialCallback(context: NoContext, event: HttpParserEvent, data: []const u8) void {
    std.debug.warn("test got partial data '{}'\n", .{data});
}
fn initDefaultHttpParser(comptime HttpParser: type) HttpParser {
    if (HttpParser.options.partialData)
        return HttpParser.init(.{}, testCallback, .{.partialCallback = testPartialCallback});
    return HttpParser.init(.{}, testCallback, .{});
}
test "HttpParser" {
    inline for ([2]bool {false, true}) |partialData| {
        try testParser(HttpParserGeneric(HttpParserOptions {
            .CallbackContext = NoContext,
            .partialData = partialData,
            .optionalMaxMethod = DefaultMaxMethod,
            .optionalMaxHeaderName = DefaultMaxHeaderName,
        }));
    }
}

fn chunkedParse(parser: anytype, data: []const u8, chunkSize: usize) !usize {
     var remaning = data;
     while (true) {
        const nextLen = std.math.min(chunkSize, remaning.len);
        const parsed = try parser.parse(remaning[0..nextLen]);
        remaning = remaning[parsed..];
        if (remaning.len == 0)
            return data.len;
    }
}

const ChunkSizes = struct {
    max: usize,
    current: usize,
    pub fn init(max: usize) ChunkSizes {
        return ChunkSizes { .max = max, .current = 0, };
    }
    pub fn next(self: *ChunkSizes) ?usize {
        if (self.current == self.max) return null;
        self.current += 1;
        return self.current;
    }
};
pub fn chunkSizes(comptime T: type, len: usize) ChunkSizes {
    return if (comptime T.options.partialData) ChunkSizes.init(len)
        else .{ .max = len, .current = len - 1 };
}

fn testParser(comptime HttpParser: type) !void {
    {var c : u8 = 0; while (c < 0x7f) : (c += 1) {
        if (validTokenChar(c) or c == ' ')
            continue;
        {
            var parser = initDefaultHttpParser(HttpParser);
            var buf = [_]u8 {c,' '};
            {var i = chunkSizes(HttpParser, buf.len); while (i.next()) |chunkSize| {
                testing.expectError(HttpParser.Error.InvalidMethodChar, chunkedParse(&parser, &buf, chunkSize));
            }}
        }
        {
            var parser = initDefaultHttpParser(HttpParser);
            var buf = [_]u8 {'G','E','T',c};
            {var i = chunkSizes(HttpParser, buf.len); while (i.next()) |chunkSize| {
                testing.expectError(HttpParser.Error.InvalidMethodChar, chunkedParse(&parser, &buf, chunkSize));
            }}
        }
        if (c != ':' and c != '\r') {
            var parser = initDefaultHttpParser(HttpParser);
            var buf = [_]u8 {'G','E','T',' ','/',' ','H','T','T','P','/','1','.','1','\r','\n',c};
            {var i = chunkSizes(HttpParser, buf.len); while (i.next()) |chunkSize| {
                testing.expectError(HttpParser.Error.InvalidHeaderNameChar, chunkedParse(&parser, &buf, chunkSize));
            }}
        }
    }}
    if (HttpParser.options.optionalMaxMethod) |maxMethod| {
        var parser = initDefaultHttpParser(HttpParser);
        var buf: [maxMethod + 1]u8 = undefined;
        std.mem.set(u8, &buf, 'A');
        testing.expectError(HttpParser.Error.MethodNameTooLong, parser.parse(&buf));
    }
    for ([_][]const u8 {
        "GET / HTTP/1.1\r\r",
        "GET / HTTP/1.0\r\n",
        "GET / !TTP/1.1\r\n",
    }) |buf| {
        var parser = initDefaultHttpParser(HttpParser);
        testing.expectError(HttpParser.Error.BadVersionNewline, parser.parse(buf));
    }
    _ = try initDefaultHttpParser(HttpParser).parse("GET / HTTP/1.1\r\n\r\n");
    _ = try initDefaultHttpParser(HttpParser).parse("GET / HTTP/1.1\r\nName: Value\r\n\r\n");
    _ = try initDefaultHttpParser(HttpParser).parse("GET / HTTP/1.1\r\nName: Value\r\nAnother: value\r\n\r\n");
}

const CharFlags = struct {
    pub const none      = 0;
    pub const ctl       = 1 << 0;
    pub const separator = 1 << 1;
    pub const ctl_separator = ctl | separator;
};
const charFlagTable = [127]u8 {
    CharFlags.ctl,            // '\0'
    CharFlags.ctl,            // '\x01'
    CharFlags.ctl,            // '\x02'
    CharFlags.ctl,            // '\x03'
    CharFlags.ctl,            // '\x04'
    CharFlags.ctl,            // '\x05'
    CharFlags.ctl,            // '\x06'
    CharFlags.ctl,            // '\x07'
    CharFlags.ctl,            // '\x08'
    CharFlags.ctl_separator,  // '\t'
    CharFlags.ctl,            // '\n'
    CharFlags.ctl,            // '\x0B'
    CharFlags.ctl,            // '\x0C'
    CharFlags.ctl,            // '\r'
    CharFlags.ctl,            // '\x0E'
    CharFlags.ctl,            // '\x0F'
    CharFlags.ctl,            // '\x11'
    CharFlags.ctl,            // '\x12'
    CharFlags.ctl,            // '\x13'
    CharFlags.ctl,            // '\x14'
    CharFlags.ctl,            // '\x15'
    CharFlags.ctl,            // '\x16'
    CharFlags.ctl,            // '\x17'
    CharFlags.ctl,            // '\x18'
    CharFlags.ctl,            // '\x19'
    CharFlags.ctl,            // '\x1A'
    CharFlags.ctl,            // '\x1B'
    CharFlags.ctl,            // '\x1C'
    CharFlags.ctl,            // '\x1D'
    CharFlags.ctl,            // '\x1E'
    CharFlags.ctl,            // '\x1F'
    CharFlags.separator,      // ' '
    CharFlags.none,           // '!'
    CharFlags.separator,      // '"'
    CharFlags.none,           // '#'
    CharFlags.none,           // '$'
    CharFlags.none,           // '%'
    CharFlags.none,           // '&'
    CharFlags.none,           // '\''
    CharFlags.separator,      // '('
    CharFlags.separator,      // ')'
    CharFlags.none,           // '*'
    CharFlags.none,           // '+'
    CharFlags.separator,      // ','
    CharFlags.none,           // '-'
    CharFlags.none,           // '.'
    CharFlags.separator,      // '/'
    CharFlags.none,           // '0'
    CharFlags.none,           // '1'
    CharFlags.none,           // '2'
    CharFlags.none,           // '3'
    CharFlags.none,           // '4'
    CharFlags.none,           // '5'
    CharFlags.none,           // '6'
    CharFlags.none,           // '7'
    CharFlags.none,           // '8'
    CharFlags.none,           // '9'
    CharFlags.separator,      // ':'
    CharFlags.separator,      // ';'
    CharFlags.separator,      // '<'
    CharFlags.separator,      // '='
    CharFlags.separator,      // '>'
    CharFlags.separator,      // '?'
    CharFlags.separator,      // '@'
    CharFlags.none,           // 'A'
    CharFlags.none,           // 'B'
    CharFlags.none,           // 'C'
    CharFlags.none,           // 'D'
    CharFlags.none,           // 'E'
    CharFlags.none,           // 'F'
    CharFlags.none,           // 'G'
    CharFlags.none,           // 'H'
    CharFlags.none,           // 'I'
    CharFlags.none,           // 'J'
    CharFlags.none,           // 'K'
    CharFlags.none,           // 'L'
    CharFlags.none,           // 'M'
    CharFlags.none,           // 'N'
    CharFlags.none,           // 'O'
    CharFlags.none,           // 'P'
    CharFlags.none,           // 'Q'
    CharFlags.none,           // 'R'
    CharFlags.none,           // 'S'
    CharFlags.none,           // 'T'
    CharFlags.none,           // 'U'
    CharFlags.none,           // 'V'
    CharFlags.none,           // 'W'
    CharFlags.none,           // 'X'
    CharFlags.none,           // 'Y'
    CharFlags.none,           // 'Z'
    CharFlags.separator,      // '['
    CharFlags.separator,      // '\\'
    CharFlags.separator,      // ']'
    CharFlags.none,           // '^'
    CharFlags.none,           // '_'
    CharFlags.none,           // '`'
    CharFlags.none,           // 'a'
    CharFlags.none,           // 'b'
    CharFlags.none,           // 'c'
    CharFlags.none,           // 'd'
    CharFlags.none,           // 'e'
    CharFlags.none,           // 'f'
    CharFlags.none,           // 'g'
    CharFlags.none,           // 'h'
    CharFlags.none,           // 'i'
    CharFlags.none,           // 'j'
    CharFlags.none,           // 'k'
    CharFlags.none,           // 'l'
    CharFlags.none,           // 'm'
    CharFlags.none,           // 'n'
    CharFlags.none,           // 'o'
    CharFlags.none,           // 'p'
    CharFlags.none,           // 'q'
    CharFlags.none,           // 'r'
    CharFlags.none,           // 's'
    CharFlags.none,           // 't'
    CharFlags.none,           // 'u'
    CharFlags.none,           // 'v'
    CharFlags.none,           // 'w'
    CharFlags.none,           // 'x'
    CharFlags.none,           // 'y'
    CharFlags.none,           // 'z'
    CharFlags.separator,      // '{'
    CharFlags.none,           // '|'
    CharFlags.separator,      // '}'
    CharFlags.none,           // '~'
    CharFlags.none,           // '\x7F'
};
fn getCharFlags(c: u8) u8 {
    return if (c < charFlagTable.len) charFlagTable[c] else CharFlags.ctl;
}
fn validTokenChar(c: u8) bool {
    return 0 == (getCharFlags(c) & CharFlags.ctl_separator);
}

// TODO: use something from standard lib
fn memcmp(comptime T: type, a: [*]const T, b: [*]const T, len: usize) bool {
    var i : usize = 0;
    while (i < len) : (i += 1) {
        if (a[i] != b[i])
            return false;
    }
    return true;
}
