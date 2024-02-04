const std = @import("std");
const Allocator = std.mem.Allocator;

pub const Scanner = struct {
    alloc: Allocator,
    start: usize = 0,
    cur: usize = 0,
    line: u64 = 1,
    txt: [:0]const u8,

    const Self = @This();
    pub fn init(alloc: Allocator, src: [:0]const u8) Self {
        return .{
            .alloc = alloc,
            .txt = src,
        };
    }

    pub fn ntok(s: *Self) Token {
        s.skipws();
        s.start = s.cur;
        if (s.cc() == 0) {
            return s.makeToken(.EOF);
        }
        const c = s.adv();
        return switch (c) {
            '(' => s.makeToken(.LPAR),
            ')' => s.makeToken(.RPAR),
            '{' => s.makeToken(.LBRACE),
            '}' => s.makeToken(.RBRACE),
            ';' => s.makeToken(.SEMICOL),
            ',' => s.makeToken(.COMMA),
            '.' => s.makeToken(.DOT),
            '-' => s.makeToken(.MINUS),
            '+' => s.makeToken(.PLUS),
            '/' => s.makeToken(.SLASH),
            '*' => s.makeToken(.STAR),
            '!' => if (s.mt('=')) s.makeToken(.BANG_EQL) else s.makeToken(.BANG),
            '=' => if (s.mt('=')) s.makeToken(.EQL_EQL) else s.makeToken(.EQL),
            '<' => if (s.mt('=')) s.makeToken(.LESS_EQL) else s.makeToken(.LESS),
            '>' => if (s.mt('=')) s.makeToken(.GTR_EQL) else s.makeToken(.GTR),
            '"' => s.string(),
            '0'...'9' => s.number(),
            'a'...'z', 'A'...'Z', '_' => s.ident(),

            else => s.makeErrorToken("unexpected character."),
        };
    }

    fn ident(s: *Self) Token {
        while (switch (s.cc()) {
            'a'...'z', 'A'...'Z', '0'...'9', '_' => true,
            else => false,
        }) {
            _ = s.adv();
        }
        const val = s.txt[s.start..s.cur];
        return .{ .tp = TokenType.identType(val), .val = val, .line = s.line };
    }
    fn identType(s: *Self) TokenType {
        _ = s;
    }

    // scan int
    fn sci(s: *Self) void {
        while (switch (s.cc()) {
            '0'...'9' => true,
            else => false,
        }) {
            _ = s.adv();
        }
    }

    fn number(s: *Self) Token {
        s.sci();
        if (s.cc() == '.' and ('0' <= s.pk() and s.pk() <= '9')) {
            _ = s.adv(); // consume .
            s.sci();
        }

        return .{
            .tp = .STRING,
            .val = s.txt[s.start..s.cur],
            .line = s.line,
        };
    }

    fn string(s: *Self) Token {
        const startLine = s.line;
        while (s.cc() != '"') {
            if (s.cc() == '\n') s.line += 1;
            _ = s.adv();
        }

        if (s.cur == s.txt.len) return s.makeErrorToken("Unterminated string");
        _ = s.adv();
        return .{
            .tp = .STRING,
            .val = s.txt[s.start + 1 .. s.cur - 1],
            .line = startLine,
        };
    }

    fn skipws(s: *Self) void {
        while (true) {
            switch (s.cc()) {
                ' ', '\r', '\t' => {
                    _ = s.adv();
                },
                '/' => {
                    if (s.pk() == '/') {
                        while (s.cc() != '\n') {
                            _ = s.adv();
                        }
                    }
                    break;
                },
                '\n' => {
                    s.line += 1;
                    _ = s.adv();
                },
                else => break,
            }
        }
    }

    fn makeErrorToken(s: *Self, msg: []const u8) Token {
        return .{
            .tp = .ERROR,
            .val = msg,
            .line = s.line,
        };
    }

    fn makeToken(s: *Self, t: TokenType) Token {
        return .{
            .tp = t,
            .val = null,
            .line = s.line,
        };
    }

    fn adv(s: *Self) u8 {
        const v = s.cc();
        s.cur += 1;
        return v;
    }

    fn mt(s: *Self, c: u8) bool {
        if (s.cc() != c) return false;
        _ = s.adv();
        return true;
    }

    inline fn cc(s: *Self) u8 {
        return s.txt[s.cur];
    }

    inline fn pk(s: *Self) u8 {
        return s.txt[s.cur + 1];
    }
};

pub const Token = struct {
    tp: TokenType,
    val: ?[]const u8,
    line: u64,
};

pub const TokenType = enum(u8) {
    //keywords,
    AND,
    CLASS,
    ELSE,
    FALSE,
    FOR,
    FUN,
    IF,
    NIL,
    OR,
    PRINT,
    RETURN,
    SUPER,
    THIS,
    TRUE,
    VAR,
    WHILE,

    // single char
    //LBRAK,
    //RBRAK,
    LBRACE,
    RBRACE,
    LPAR,
    RPAR,
    SEMICOL,
    COMMA,
    DOT,
    MINUS,
    PLUS,
    SLASH,
    STAR,
    //1 or 2 char
    BANG,
    BANG_EQL,
    EQL,
    EQL_EQL,
    GTR,
    GTR_EQL,
    LESS,
    LESS_EQL,
    //lit
    IDENT,
    NUM,
    STRING,

    ERROR,
    EOF,

    const keyWordCount = 16;

    pub fn identType(s: []const u8) TokenType {
        inline for (0..keyWordCount) |i| {
            const kw: TokenType = @enumFromInt(i);
            if (std.mem.eql(u8, &toLower(@tagName(kw)), s)) return kw;
        } else {
            return .IDENT;
        }
    }
    fn toLower(comptime s: [:0]const u8) [s.len:0]u8 {
        var sl: [s.len:0]u8 = undefined;
        for (s, 0..) |c, i| {
            sl[i] = std.ascii.toLower(c);
        }
        return sl;
    }
};

test Scanner {
    var sc = Scanner.init(std.testing.allocator, "false");
    try std.testing.expectEqual(TokenType.FALSE, sc.ntok().tp);
}
