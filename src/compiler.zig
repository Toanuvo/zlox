const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");
const maxInt = std.math.maxInt;
const mem = std.mem;
const Endianess = @import("builtin").cpu.arch.endian();

pub const Parser = struct {
    finished: bool = false,
    cur: lx.Token = undefined,
    prev: lx.Token = undefined,
    scnr: *lx.Scanner = undefined,
    hadError: bool = false,
    panicMode: bool = false,
    alloc: Allocator,
    src: [:0]const u8,
    vm: *lx.VM,
    current: ?*Compiler = null,

    const Self = @This();
    pub fn init(alloc: Allocator, vm: *lx.VM, src: [:0]const u8) !Self {
        return .{
            //.chunk = c,
            .src = src,
            .alloc = alloc,
            .vm = vm,
        };
    }

    pub fn compile(s: *Self) !*lx.Func {
        var c: Compiler = undefined;
        try Compiler.init(s, &c, .SCRIPT);
        var sc = lx.Scanner.init(s.alloc, s.src);
        s.scnr = &sc;
        s.advance();

        while (!s.mt(.EOF)) {
            try s.decl();
        }

        //try s.consume(.EOF, "expect end of expr");
        return s.endCompiler();
        //return !s.hadError; // todo deal with error
    }

    pub fn decl(s: *Self) anyerror!void {
        if (s.mt(.FUN)) {
            try s.funDecl();
        } else if (s.mt(.VAR)) {
            try s.varDecl();
        } else {
            try s.stmt();
        }
        if (s.panicMode) s.sync();
    }

    pub fn funDecl(s: *Self) !void {
        const glob = try s.parseVar("expect function name");
        markInit(s.current.?);
        try s.function(.FUNC);
        try s.defineVar(glob);
    }

    pub fn function(s: *Self, fnType: lx.FuncType) !void {
        var compiler: Compiler = undefined;
        try Compiler.init(s, &compiler, fnType);
        try s.beginScope();

        try s.consume(.LPAR, "expect (  after fn name");
        if (s.cur.tp != .RPAR) {
            while (true) {
                s.current.?.function.arity += 1;
                const cnst = try s.parseVar("expect parameter name");
                try s.defineVar(cnst);

                if (!s.mt(.COMMA)) break;
            }
        }
        try s.consume(.RPAR, "expect )  after parameters");
        try s.consume(.LBRACE, "expect {  before fn body");
        try s.block();

        const func = try s.endCompiler();
        try s.emitOp(.CLOSURE);
        try s.emitByte(try s.makeConst(.{ .Obj = &func.obj }));

        for (compiler.upvalues[0..compiler.function.upValueCount]) |uv| {
            try s.emitByte(uv.isLocal);
            try s.emitByte(uv.idx);
        }
    }

    pub fn call(s: *Self, _: bool) !void {
        const count: usize = try s.argList();
        try s.emitOp(.CALL);
        try s.emitByte(count);
    }

    pub fn argList(s: *Self) !u8 {
        var count: u8 = 0;
        if (s.cur.tp != .RPAR) {
            while (true) {
                try s.expr();
                count += 1;
                if (!s.mt(.COMMA)) break;
            }
        }
        try s.consume(.RPAR, "expect ) after arg list");
        return count;
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
        if (s.current.?.scopeDepth > 0) {
            markInit(s.current.?);
            return;
        }
        try s.emitOp(.DEF_GLOB);
        try s.emitByte(global);
    }

    pub fn markInit(comp: *Compiler) void {
        if (comp.scopeDepth == 0) return;
        comp.locals[comp.localCount].depth = comp.scopeDepth;
    }

    pub fn parseVar(s: *Self, errmsg: [:0]const u8) !usize {
        try s.consume(.IDENT, errmsg);
        try s.declareVar();
        if (s.current.?.scopeDepth > 0) return 0;
        return try s.identConst(s.prev);
    }

    pub fn declareVar(s: *Self) !void {
        if (s.current.?.scopeDepth == 0) return;

        const name = s.prev;

        for (s.current.?.locals[s.current.?.localCount..]) |local| {
            if (local.depth != null and local.depth.? < s.current.?.scopeDepth) { // local.depth != -1
                break;
            }

            if (std.meta.eql(name, local.name)) {
                try s.displayErr(&local.name, "already a variable with this name in this scope");
            }
        }
        addLocal(s.current.?, name);
    }

    pub fn addLocal(comp: *Compiler, name: lx.Token) void {
        //if (comp.localCount == 0) {
        //try s.displayErr(&name, "too many local variables in function");
        //}
        comp.localCount -= 1; // too many local variables in function
        const local = &comp.locals[comp.localCount];
        local.* = .{
            .name = name,
            .depth = null,
        };
    }

    pub fn identConst(s: *Self, name: lx.Token) !usize {
        const str = try lx.String.allocString(s.vm, s.alloc, name.val.?);
        return s.makeConst(.{ .Obj = &str.obj });
    }

    pub fn stmt(s: *Self) anyerror!void {
        if (s.mt(.PRINT)) {
            try s.printStmt();
        } else if (s.mt(.IF)) {
            try s.ifStmt();
        } else if (s.mt(.RETURN)) {
            try s.returnStmt();
        } else if (s.mt(.WHILE)) {
            try s.whileStmt();
        } else if (s.mt(.FOR)) {
            try s.forStmt();
        } else if (s.mt(.LBRACE)) {
            try s.beginScope();
            try s.block();
            try s.endScope();
        } else {
            try s.exprStmt();
        }
    }

    pub fn ifStmt(s: *Self) !void {
        try s.consume(.LPAR, "expect ( after if");
        try s.expr();
        try s.consume(.RPAR, "expect ) after condition");

        const thenJump = try s.emitJump(.JMP_IF_FALSE);
        try s.emitOp(.POP);
        try s.stmt();
        const elseJump = try s.emitJump(.JMP);
        try s.patchJMP(thenJump);
        try s.emitOp(.POP);
        if (s.mt(.ELSE)) try s.stmt();
        try s.patchJMP(elseJump);
    }

    pub fn returnStmt(s: *Self) !void {
        if (s.current.?.funcType == .SCRIPT) try s.displayErr(&s.prev, "cant return from top level code");
        if (s.mt(.SEMICOL)) {
            try s.emitReturn();
        } else {
            try s.expr();
            try s.consume(.SEMICOL, "expect ; after return value");
            try s.emitOp(.RETURN);
        }
    }

    pub fn whileStmt(s: *Self) !void {
        const loopStart = s.curChunk().cap;
        try s.consume(.LPAR, "expect ( after while");
        try s.expr();
        try s.consume(.RPAR, "expect ) after condition");

        const exitJump = try s.emitJump(.JMP_IF_FALSE);
        try s.emitOp(.POP);
        try s.stmt();
        try s.emitLoop(loopStart);
        try s.patchJMP(exitJump);
        try s.emitOp(.POP);
    }

    pub fn forStmt(s: *Self) !void {
        try s.beginScope();
        try s.consume(.LPAR, "expect ( after for");
        if (s.mt(.SEMICOL)) {
            //no initializer
        } else if (s.mt(.VAR)) {
            try s.varDecl(); // consumes trailing semi
        } else {
            try s.exprStmt(); // consumes trailing semi
        }
        var loopStart = s.curChunk().cap;
        const exitJump = if (!s.mt(.SEMICOL)) b: {
            try s.expr();
            try s.consume(.SEMICOL, "expect ; after loop condition");

            const v = try s.emitJump(.JMP_IF_FALSE);
            try s.emitOp(.POP);
            break :b v;
        } else null;

        if (!s.mt(.RPAR)) {
            const bodyJump = try s.emitJump(.JMP);
            const incrStart = s.curChunk().cap;
            try s.expr();
            try s.emitOp(.POP);

            try s.consume(.RPAR, "expect ) after for clauses");
            try s.emitLoop(loopStart);
            loopStart = incrStart;
            try s.patchJMP(bodyJump);
        }

        try s.stmt();
        try s.emitLoop(loopStart);

        if (exitJump) |jmp| {
            try s.patchJMP(jmp);
            try s.emitOp(.POP);
        }

        try s.endScope();
    }

    pub fn emitLoop(s: *Self, start: usize) !void {
        try s.emitOp(.LOOP);

        if (std.math.cast(u16, s.curChunk().cap - start + 2)) |jmp| {
            try s.emitInt(u16, jmp, start);
        } else {
            try s.displayErr(&s.prev, "too long jump");
        }
    }

    pub fn emitJump(s: *Self, op: lx.OpCode) !usize {
        try s.emitOp(op);
        try s.emitInt(u16, 0, null);

        return s.curChunk().cap - 2;
    }

    pub fn patchJMP(s: *Self, loc: usize) !void {
        // dont need -2 because of how emitInt / read is implemented?
        if (std.math.cast(u16, (s.curChunk().cap - loc) - 2)) |jmp| {
            try s.emitInt(u16, jmp, loc);

            //const ch = s.curChunk();
            //const buf: *[2]u8 = @ptrCast(ch.code[loc .. loc + 2].ptr);
            //const i = std.mem.readInt(u16, buf, Endianess);
            //std.debug.print("exp: {any}, act: {any}\n", .{ jmp, i });
        } else {
            try s.displayErr(&s.prev, "too long jump");
        }
    }

    pub fn beginScope(s: *Self) !void {
        s.current.?.scopeDepth += 1;
    }

    pub fn endScope(s: *Self) !void {
        s.current.?.scopeDepth -= 1;

        for (s.current.?.locals[s.current.?.localCount..]) |local| {
            if (local.depth) |depth| {
                if (depth <= s.current.?.scopeDepth) {
                    break;
                }
            }

            try s.emitOp(.POP);
            s.current.?.localCount += 1;
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

        const varb: Variable = if (try resolveLocal(s.current.?, name)) |loc| b: {
            break :b .{
                .arg = loc,
                .setOp = .SET_LOCAL,
                .getOp = .GET_LOCAL,
            };
        } else if (try resolveUpvalue(s.current.?, name)) |up| b: {
            break :b .{
                .arg = up,
                .setOp = .SET_UPVAL,
                .getOp = .GET_UPVAL,
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

    pub fn resolveLocal(comp: *Compiler, name: lx.Token) !?usize {
        std.debug.print("func: {?any}\n", .{comp.function.name});
        std.debug.print("resolveLocal: {?s}\n", .{name.val});
        std.debug.print("locals: {any}\n", .{comp.locals[comp.localCount..].len});
        for (comp.locals[comp.localCount..], comp.localCount..) |local, i| {
            if (local.depth != null and local.depth.? < comp.scopeDepth) { // local.depth != -1
                break;
            }

            if (name.tp == local.name.tp and std.mem.eql(u8, name.val.?, local.name.val.?)) {
                if (local.depth == null) @panic("cant read local variable from its own initializer");
                //if (local.depth == null) try s.displayErr(&name, "cant read local variable from its own initializer");
                std.debug.print("idx: {any}\n", .{(maxInt(u8) - i) - 1});
                return (maxInt(u8) - i) - 1;
            }
        }
        return null;
    }

    pub fn resolveUpvalue(comp: *Compiler, name: lx.Token) !?usize {
        std.debug.print("resolve: {?s}\n", .{name.val});
        return if (comp.enclosing) |enclosing|
            if (try resolveLocal(enclosing, name)) |loc|
                addUpValue(comp, loc, true)
            else if (try resolveUpvalue(enclosing, name)) |uv|
                addUpValue(comp, uv, false)
            else
                null
        else
            null;
    }

    pub fn addUpValue(comp: *Compiler, idx: usize, isLocal: bool) usize {
        const nextSlot = comp.function.upValueCount;
        for (comp.upvalues[0..nextSlot], 0..) |uv, i| {
            if (uv.idx == idx and uv.isLocal == isLocal) {
                return i;
            }
        }

        const slot = &comp.upvalues[nextSlot];
        slot.* = .{
            .isLocal = isLocal,
            .idx = idx,
        };

        comp.function.upValueCount += 1;
        return nextSlot;
    }

    pub fn @"and"(s: *Self, _: bool) !void {
        const endJump = try s.emitJump(.JMP_IF_FALSE);
        try s.emitOp(.POP);
        try s.parsePrec(.AND);
        try s.patchJMP(endJump);
    }

    pub fn @"or"(s: *Self, _: bool) !void {
        const elseJump = try s.emitJump(.JMP_IF_FALSE);
        const endJump = try s.emitJump(.JMP);

        try s.patchJMP(elseJump);
        try s.emitOp(.POP);

        try s.parsePrec(.OR);
        try s.patchJMP(endJump);
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
        //std.debug.print("val: {any}\n", .{v});
        try s.emitOp(.CONST);
        try s.emitByte(try s.makeConst(v));
        //const j = s.curChunk().code[s.curChunk().cap - 1];
        //std.debug.print("{any}, {any}\n", .{ i, j });
    }

    pub fn makeConst(s: *Self, v: lx.Value) !usize {
        return try s.curChunk().addConst(v);
    }

    pub fn emitOp(s: *Self, b: lx.OpCode) !void {
        //std.debug.print("{*}: {any}\n", .{ s.curChunk(), b });
        try s.curChunk().writeChunk(b, s.prev.line);
    }

    pub fn emitOps(s: *Self, comptime ops: []const lx.OpCode) !void {
        for (ops) |v| {
            try s.curChunk().writeChunk(v, s.prev.line);
        }
    }

    pub fn emitInt(s: *Self, comptime T: type, v: T, offset: ?usize) !void {
        try s.curChunk().writeInt(T, v, s.prev.line, offset);
    }

    pub fn emitByte(s: *Self, b: anytype) !void {
        try s.curChunk().writeChunk(b, s.prev.line);
    }

    pub fn emitReturn(s: *Self) !void {
        try s.emitOp(.NIL);
        try s.emitOp(.RETURN);
    }

    pub fn curChunk(s: *Self) *lx.Chunk {
        return s.current.?.function.chunk;
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

    const rules = blk: {
        var rls = std.EnumArray(lx.TokenType, ParseRule).initUndefined();
        rls.set(.LPAR, ParseRule.of(&Self.grouping, &Self.call, .CALL));
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
        rls.set(.AND, ParseRule.of(null, &Self.@"and", .AND));
        rls.set(.OR, ParseRule.of(null, &Self.@"or", .OR));
        break :blk rls;
    };

    pub fn endCompiler(s: *Self) !*lx.Func {
        if (s.finished) @panic("double end compiler");
        try s.emitReturn();
        const func = s.current.?.function;
        s.current = s.current.?.enclosing; // this should return exit the parser
        return func;
    }

    const Compiler = struct {
        enclosing: ?*Compiler,
        function: *lx.Func,
        funcType: FuncType,
        // array grows down
        upvalues: [maxInt(u8)]Upvalue = [_]Upvalue{undefined} ** maxInt(u8),
        locals: [maxInt(u8)]Local = [_]Local{undefined} ** maxInt(u8),
        localCount: usize = maxInt(u8), // points to the last local or off the top/end of the array
        scopeDepth: usize = 0,

        pub fn init(s: *Self, c: *Compiler, tp: FuncType) !void {
            c.* = .{
                .enclosing = s.current,
                .funcType = tp,
                .function = try lx.Func.init(s.alloc, s.vm),
            };

            s.current = c;
            if (c.funcType != .SCRIPT) {
                c.function.name = try lx.String.allocString(s.vm, s.alloc, s.prev.val.?);
            }

            c.localCount -= 1;
            c.locals[c.localCount] = .{
                .name = .{
                    .tp = .ERROR,
                    .val = "",
                    .line = maxInt(u64),
                },
                .depth = 0,
            };
        }
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

const Local = struct {
    name: lx.Token,
    depth: ?usize,
};

pub const FuncType = enum {
    FUNC,
    SCRIPT,
};

const Upvalue = struct {
    idx: usize,
    isLocal: bool,
};

test Parser {
    std.debug.print("{any}\n", .{@sizeOf(u16)});
}
