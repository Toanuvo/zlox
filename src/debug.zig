const std = @import("std");
const Chunk = @import("chunk.zig").Chunk;
const OpCode = @import("vm.zig").OpCode;
const Endianess = @import("builtin").cpu.arch.endian();
const lx = @import("zlox.zig");

//pub fn dissChunk(c: *const Chunk, stream: anytype, name: []const u8) !void {
//try stream.print("== {s} ==\n", .{name});
//var offset: usize = 0;
//while (offset < c.cap) {
//offset = try dissInstr(c, stream, offset);
//}
//}

pub fn dissInstr(s: *const Chunk, stream: anytype, offset: usize) !usize {
    try stream.print("{d:0>4} ", .{offset});
    if (offset > 0 and s.lines[offset] == s.lines[offset - 1]) {
        try stream.print("   | ", .{});
    } else {
        try stream.print("{d:0>4} ", .{s.lines[offset]});
    }

    const instr = s.code[offset];
    if (instr < 256) {
        const op: OpCode = @enumFromInt(instr);
        return switch (op) {
            .RETURN,
            .ADD,
            .SUB,
            .MUL,
            .DIV,
            .NEGATE,
            .NIL,
            .TRUE,
            .FALSE,
            .POP,
            .EQL,
            .GTR,
            .LESS,
            .NOT,
            .PRINT,
            => simpleInstr(s, @tagName(op), stream, offset),

            .JMP,
            .JMP_IF_FALSE,
            => jumpInstr(s, @tagName(op), stream, offset, 0),

            .LOOP => jumpInstr(s, @tagName(op), stream, offset, '-'),

            .CALL,
            .GET_UPVAL,
            .SET_UPVAL,
            .GET_LOCAL,
            .SET_LOCAL,
            => byteInstr(s, @tagName(op), stream, offset),

            .DEF_GLOB,
            .GET_GLOB,
            .SET_GLOB,
            .CONST,
            => constInstr(s, @tagName(op), stream, offset),

            .CLOSURE => b: {
                var off = offset + 1;
                const c = s.code[off];
                off += 1;
                try stream.print("{s: <16} {d:0>4}\n", .{ "CLOSURE", c });
                try stream.print("{}\n", .{s.vals.items[c]});

                const func = s.vals.items[c].Obj.as(lx.Func);

                for (0..func.upValueCount) |_| {
                    const isLocal = s.code[off] == 1;
                    off += 1;
                    const i = s.code[off];
                    off += 1;
                    try stream.print("{d:0>4}   |               {s} {d}\n", .{ off - 2, if (isLocal) "local" else "upval", i });
                }

                break :b off;
            },
        };
    }
}

fn jumpInstr(s: *const Chunk, name: []const u8, stream: anytype, offset: usize, sign: u8) !usize {
    const arr = s.code[offset + 1 ..][0..@sizeOf(u16)];
    const jmp = std.mem.readInt(u16, arr, Endianess);
    try stream.print("{s: <16} {d:0>4} -> {c}{d}\n", .{ name, offset, sign, offset + 3 + jmp });
    return offset + 1 + @sizeOf(u16);
}
fn byteInstr(s: *const Chunk, name: []const u8, stream: anytype, offset: usize) !usize {
    const slot = s.code[offset + 1];
    try stream.print("{s: <16} {d:0>4}\n", .{ name, slot });
    return offset + 2;
}

fn constInstr(s: *const Chunk, name: []const u8, stream: anytype, offset: usize) !usize {
    const idx = s.code[offset + 1];
    try stream.print("{s: <16} {d:0>4} '{e}'\n", .{ name, idx, s.vals.items[idx] });
    return offset + 2;
}

fn simpleInstr(s: *const Chunk, name: []const u8, stream: anytype, offset: usize) !usize {
    _ = s;
    try stream.print("{s}\n", .{name});
    return offset + 1;
}
