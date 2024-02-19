const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");
const maxInt = std.math.maxInt;

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
    current: ScopeCompiler,

    const Self = @This();
    pub fn init(alloc: Allocator, vm: *lx.VM, src: [:0]const u8, c: *lx.Chunk) Self {
        return .{
            .chunk = c,
            .src = src,
            .alloc = alloc,
            .vm = vm,
            .current = .{},
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

    pub fn decl(s: *Self) anyerror!void {
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
        if (s.current.scopeDepth > 0) {
            s.markInit();
            return;
        }
        try s.emitOp(.DEF_GLOB);
        try s.emitByte(global);
    }

    pub fn markInit(s: *Self) void {
        s.current.locals[s.current.localCount].depth = s.current.scopeDepth;
    }

    pub fn parseVar(s: *Self, errmsg: [:0]const u8) !usize {
        try s.consume(.IDENT, errmsg);
        try s.declareVar();
        if (s.current.scopeDepth > 0) return 0;
        return try s.identConst(s.prev);
    }

    pub fn declareVar(s: *Self) !void {
        if (s.current.scopeDepth == 0) return;

        const name = s.prev;

        for (s.current.locals[s.current.localCount..]) |local| {
            if (local.depth != null and local.depth.? < s.current.scopeDepth) { // local.depth != -1
                break;
            }

            if (std.meta.eql(name, local.name)) {
                try s.displayErr(&local.name, "already a variable with this name in this scope");
            }
        }
        try s.addLocal(name);
    }

    pub fn addLocal(s: *Self, name: lx.Token) !void {
        if (s.current.localCount == 0) {
            try s.displayErr(&name, "too many local variables in function");
        }
        s.current.localCount -= 1;
        const local = &s.current.locals[s.current.localCount];
        local.* = .{
            .name = name,
            .depth = null,
        };
    }

    pub fn identConst(s: *Self, name: lx.Token) !usize {
        const str = try lx.String.allocString(s.vm, s.alloc, name.val.?);
        return s.makeConst(.{ .Obj = &str.obj });
    }

    pub fn stmt(s: *Self) !void {
        if (s.mt(.PRINT)) {
            try s.printStmt();
        } else if (s.mt(.LBRACE)) {
            try s.beginScope();
            try s.block();
            try s.endScope();
        } else {
            try s.exprStmt();
        }
    }

    pub fn beginScope(s: *Self) !void {
        s.current.scopeDepth += 1;
    }

    pub fn endScope(s: *Self) !void {
        s.current.scopeDepth -= 1;

        for (s.current.locals[s.current.localCount..]) |local| {
            if (local.depth) |depth| {
                if (depth > s.current.scopeDepth) {
                    break;
                }
            }
            try s.emitOp(.POP);
            s.current.localCount += 1;
        }
    }

    pub fn block(s: *Self) !void {
        while (s.cur.tp != .RBRACE and s.cur.tp != .EOF) {
            try s.decl();
        }

        try s.consume(.RBRACE, "expect }} after block");
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
        const Variable = struct {
            arg: usize,
            setOp: lx.OpCode,
            getOp: lx.OpCode,
        };

        const varb: Variable = if (try s.resolveLocal(name)) |loc| b: {
            break :b .{
                .arg = loc,
                .setOp = .SET_LOCAL,
                .getOp = .GET_LOCAL,
            };
        } else b: {
            const arg = try s.identConst(name);
            break :b .{
                .arg = arg,
                .setOp = .SET_GLOB,
                .getOp = .GET_GLOB,
            };
        };

        //std.debug.print("VAR {any}\n", .{varb});

        if (canAssign and s.mt(.EQL)) {
            try s.expr();
            try s.emitOp(varb.setOp);
            try s.emitByte(varb.arg);
        } else {
            try s.emitOp(varb.getOp);
            try s.emitByte(varb.arg);
        }
    }

    pub fn resolveLocal(s: *Self, name: lx.Token) !?usize {
        for (s.current.locals[s.current.localCount..], s.current.localCount..) |local, i| {
            if (local.depth != null and local.depth.? < s.current.scopeDepth) { // local.depth != -1
                break;
            }

            if (name.tp == local.name.tp and std.mem.eql(u8, name.val.?, local.name.val.?)) {
                if (local.depth == null) try s.displayErr(&name, "cant read local variable from its own initializer");

                return (maxInt(u8) - i) - 1;
            }
        }
        return null;
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
            //std.debug.print("tok: {any}\n", .{s.cur});
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
    pub fn displayErr(s: *Self, tok: *const lx.Token, msg: [:0]const u8) !void {
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

const ScopeCompiler = struct {
    // array grows down
    locals: [maxInt(u8)]Local = [_]Local{undefined} ** maxInt(u8),
    localCount: usize = maxInt(u8), // points to the last local or off the top/end of the array
    scopeDepth: usize = 0,
};

const Local = struct {
    name: lx.Token,
    depth: ?usize,
};

test Compiler {}
