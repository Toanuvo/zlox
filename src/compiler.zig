const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");

pub const Compiler = struct {
    cur: lx.Token = undefined,
    prev: lx.Token = undefined,
    scnr: *lx.Scanner = undefined,
    hadError: bool = false,
    panicMode: bool = false,
    alloc: Allocator,
    src: [:0]const u8,
    chunk: *lx.Chunk,
    vm: *lx.VM,

    const Self = @This();
    pub fn init(alloc: Allocator, vm: *lx.VM, src: [:0]const u8, c: *lx.Chunk) Self {
        return .{
            .chunk = c,
            .src = src,
            .alloc = alloc,
            .vm = vm,
        };
    }

    pub fn compile(s: *Self) !bool {
        var sc = lx.Scanner.init(s.alloc, s.src);
        s.scnr = &sc;
        s.advance();

        while (!s.mt(.EOF)) {
            try s.decl();
        }

        try s.consume(.EOF, "expect end of expr");
        try s.emitOp(.RETURN);
        return !s.hadError;
    }

    pub fn decl(s: *Self) !void {
        if (s.mt(.VAR)) {
            try s.varDecl();
        } else {
            try s.stmt();
        }
        if (s.panicMode) s.sync();
    }

    pub fn varDecl(s: *Self) !void {
        const global = try s.parseVar("expect variable name");

        if (s.mt(.EQL)) {
            try s.expr();
        } else {
            try s.emitOp(.NIL);
        }

        try s.consume(.SEMICOL, "expect ; after variable decl");
        try s.defineVar(global);
    }

    pub fn defineVar(s: *Self, global: usize) !void {
        try s.emitOp(.DEF_GLOB);
        try s.emitByte(global);
    }

    pub fn parseVar(s: *Self, errmsg: [:0]const u8) !usize {
        try s.consume(.IDENT, errmsg);
        return try s.identConst(s.prev);
    }

    pub fn identConst(s: *Self, name: lx.Token) !usize {
        const str = try lx.String.allocString(s.vm, s.alloc, name.val.?);
        return s.makeConst(.{ .Obj = &str.obj });
    }

    pub fn stmt(s: *Self) !void {
        if (s.mt(.PRINT)) {
            try s.printStmt();
        } else {
            try s.exprStmt();
        }
    }

    pub fn exprStmt(s: *Self) !void {
        try s.expr();
        try s.consume(.SEMICOL, "expect ; after expression");
        try s.emitOp(.POP);
    }

    pub fn printStmt(s: *Self) !void {
        try s.expr();
        try s.consume(.SEMICOL, "expect ; after print");
        try s.emitOp(.PRINT);
    }

    pub fn mt(s: *Self, t: lx.TokenType) bool {
        if (s.cur.tp != t) return false;
        s.advance();
        return true;
    }

    pub fn expr(s: *Self) !void {
        try s.parsePrec(.ASSIGNMENT);
    }

    pub fn parsePrec(s: *Self, prec: Precedence) !void {
        s.advance();
        const prefix = rules.get(s.prev.tp).prefix;
        const canAssign = @intFromEnum(prec) <= @intFromEnum(Precedence.ASSIGNMENT);
        if (prefix) |r| {
            try r(s, canAssign);
        } else {
            try s.displayErr(&s.prev, "expect expression");
        }

        while (@intFromEnum(prec) <= @intFromEnum(rules.get(s.cur.tp).prec)) {
            s.advance();
            const infix = rules.get(s.prev.tp).infix.?;
            try infix(s, canAssign);
        }
        if (canAssign and s.mt(.EQL)) try s.displayErr(&s.prev, "invalid aSSIGNMENT target");
    }

    pub fn binary(s: *Self, _: bool) !void {
        const optp = s.prev.tp;
        var rule = rules.get(optp);
        try s.parsePrec(rule.prec.next());
        switch (optp) {
            .PLUS => try s.emitOp(.ADD),
            .MINUS => try s.emitOp(.SUB),
            .STAR => try s.emitOp(.MUL),
            .SLASH => try s.emitOp(.DIV),
            .EQL_EQL => try s.emitOp(.EQL),
            .GTR => try s.emitOp(.GTR),
            .LESS => try s.emitOp(.LESS),
            .BANG_EQL => try s.emitOps(&.{ .EQL, .NOT }),
            .GTR_EQL => try s.emitOps(&.{ .LESS, .NOT }),
            .LESS_EQL => try s.emitOps(&.{ .GTR, .NOT }),
            else => unreachable,
        }
    }

    pub fn unary(s: *Self, _: bool) !void {
        const op = s.prev.tp;
        try s.parsePrec(.UNARY);
        switch (op) {
            .MINUS => try s.emitOp(.NEGATE),
            .BANG => try s.emitOp(.NOT),
            else => unreachable,
        }
    }

    pub fn grouping(s: *Self, _: bool) !void {
        try s.expr();
        try s.consume(.RPAR, "expect ')' after expr");
    }

    pub fn namedVar(s: *Self, name: lx.Token, canAssign: bool) !void {
        const arg = try s.identConst(name);
        if (canAssign and s.mt(.EQL)) {
            try s.expr();
            try s.emitOp(.SET_GLOB);
            try s.emitByte(arg);
        } else {
            try s.emitOp(.GET_GLOB);
            try s.emitByte(arg);
        }
    }

    pub fn variable(s: *Self, canAssign: bool) !void {
        try s.namedVar(s.prev, canAssign);
    }

    pub fn number(s: *Self, _: bool) !void {
        const v = try std.fmt.parseFloat(std.meta.FieldType(lx.Value, .Num), s.prev.val.?);
        try s.emitConst(.{ .Num = v });
    }

    pub fn literal(s: *Self, _: bool) !void {
        switch (s.prev.tp) {
            .TRUE => try s.emitOp(.TRUE),
            .FALSE => try s.emitOp(.FALSE),
            .NIL => try s.emitOp(.NIL),
            else => unreachable,
        }
    }

    pub fn string(s: *Self, _: bool) !void {
        var str = try lx.String.allocString(s.vm, s.alloc, s.prev.val.?);
        try s.emitConst(.{ .Obj = &str.obj });
    }

    pub fn emitConst(s: *Self, v: lx.Value) !void {
        try s.emitOp(.CONST);
        try s.emitByte(try s.makeConst(v));
    }

    pub fn makeConst(s: *Self, v: lx.Value) !usize {
        return try s.curChunk().addConst(v);
    }

    pub fn emitOp(s: *Self, b: lx.OpCode) !void {
        try s.curChunk().writeChunk(b, s.prev.line);
    }

    pub fn emitOps(s: *Self, comptime ops: []const lx.OpCode) !void {
        for (ops) |v| {
            try s.curChunk().writeChunk(v, s.prev.line);
        }
    }

    pub fn emitByte(s: *Self, b: anytype) !void {
        try s.curChunk().writeChunk(b, s.prev.line);
    }

    pub fn curChunk(s: *Self) *lx.Chunk {
        return s.chunk;
    }

    pub fn consume(s: *Self, tp: lx.TokenType, msg: [:0]const u8) !void {
        if (s.cur.tp == tp) {
            s.advance();
        } else {
            try s.displayErr(&s.cur, msg);
        }
    }

    pub fn advance(s: *Self) void {
        s.prev = s.cur;
        while (true) {
            s.cur = s.scnr.ntok();
            if (s.cur.tp != lx.TokenType.ERROR) break;
            //s.displayErr()
        }
    }

    fn sync(s: *Self) void {
        s.panicMode = false;
        while (s.cur.tp != .EOF) {
            if (s.prev.tp == .SEMICOL) return;
            switch (s.cur.tp) {
                .CLASS,
                .FUN,
                .VAR,
                .FOR,
                .IF,
                .WHILE,
                .PRINT,
                .RETURN,
                => return,
                else => {},
            }
            s.advance();
        }
    }

    // todo use zig errors
    pub fn displayErr(s: *Self, tok: *lx.Token, msg: [:0]const u8) !void {
        if (s.panicMode) return;
        s.panicMode = true;
        var stderr = std.io.getStdErr().writer();
        try stderr.print("[line {d}] Error", .{tok.line});

        if (tok.tp == lx.TokenType.EOF) {
            try stderr.print(" at end", .{});
        } else if (tok.tp == lx.TokenType.ERROR) {} else if (tok.val) |v| {
            try stderr.print(" at '{s}'", .{v});
        } else {
            try stderr.print(" at '{s}'", .{@tagName(tok.tp)});
        }
        try stderr.print(": {s}\n", .{msg});

        s.hadError = true;
    }

    const ParseFn = *const fn (s: *Self, canAssign: bool) anyerror!void;
    const ParseRule = struct {
        prefix: ?ParseFn,
        infix: ?ParseFn,
        prec: Precedence,

        pub fn of(pre: ?ParseFn, in: ?ParseFn, prec: Precedence) ParseRule {
            return .{
                .prefix = pre,
                .infix = in,
                .prec = prec,
            };
        }
    };

    usingnamespace @import("scanner.zig");
    const rules = blk: {
        var rls = std.EnumArray(lx.TokenType, ParseRule).initUndefined();
        rls.set(.LPAR, ParseRule.of(&Self.grouping, null, .NONE));
        rls.set(.MINUS, ParseRule.of(&Self.unary, &Self.binary, .TERM));
        rls.set(.PLUS, ParseRule.of(null, &Self.binary, .TERM));
        rls.set(.SLASH, ParseRule.of(null, &Self.binary, .FACTOR));
        rls.set(.STAR, ParseRule.of(null, &Self.binary, .FACTOR));
        rls.set(.NUM, ParseRule.of(&Self.number, null, .NONE));
        rls.set(.TRUE, ParseRule.of(&Self.literal, null, .NONE));
        rls.set(.FALSE, ParseRule.of(&Self.literal, null, .NONE));
        rls.set(.NIL, ParseRule.of(&Self.literal, null, .NONE));
        rls.set(.BANG, ParseRule.of(&Self.unary, null, .NONE));
        rls.set(.BANG_EQL, ParseRule.of(null, &Self.binary, .EQUALITY));
        rls.set(.EQL_EQL, ParseRule.of(null, &Self.binary, .EQUALITY));
        rls.set(.GTR, ParseRule.of(null, &Self.binary, .COMPARISON));
        rls.set(.GTR_EQL, ParseRule.of(null, &Self.binary, .COMPARISON));
        rls.set(.LESS, ParseRule.of(null, &Self.binary, .COMPARISON));
        rls.set(.LESS_EQL, ParseRule.of(null, &Self.binary, .COMPARISON));
        rls.set(.STRING, ParseRule.of(&Self.string, null, .NONE));
        rls.set(.IDENT, ParseRule.of(&Self.variable, null, .NONE));
        break :blk rls;
    };
};

const Precedence = enum(u8) {
    NONE,
    ASSIGNMENT, // =
    OR, // or
    AND, // and
    EQUALITY, // == !=
    COMPARISON, // < > <= >=
    TERM, // + -
    FACTOR, // * /
    UNARY, // ! -
    CALL, // . ()
    PRIMARY,

    const Self = @This();
    pub fn next(s: Self) Self {
        if (s == .PRIMARY) {
            return s;
        } else {
            return @enumFromInt(1 + @intFromEnum(s));
        }
    }
};

test Compiler {}
