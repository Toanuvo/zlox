const std = @import("std");
const debug = @import("debug.zig");
const lx = @import("zlox.zig");

pub fn main() !void {
    const args = std.os.argv;
    if (args.len == 1) {
        try repl();
    } else if (args.len == 2) {
        try runFile(std.mem.span(args[1]));
    } else {
        unreachable;
    }

    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});
}

pub fn runFile(fileName: [:0]const u8) !void {
    _ = fileName;
}

pub fn repl() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    const alloc = gpa.allocator();
    _ = alloc;
    defer _ = gpa.deinit();

    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("zlox\n", .{});

    const stdin = std.io.getStdIn().reader();
    _ = stdin;

    while (true) {
        try bw.flush();
        try stdout.print("> ", .{});
        break;
    }
    try bw.flush();
}

test "simple test" {
    const alloc = std.testing.allocator;
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    var vm = lx.VM.init(alloc);
    defer _ = &vm.deinit();

    var c = lx.Chunk.init(alloc);
    defer c.deinit();

    const cst = try c.addConst(1.2);
    try c.writeChunk(lx.OpCode.CONST, 5);
    try c.writeChunk(cst, 5);

    const a1 = try c.addConst(3.4);
    try c.writeChunk(lx.OpCode.CONST, 5);
    try c.writeChunk(a1, 5);

    try c.writeChunk(lx.OpCode.ADD, 5);

    const a2 = try c.addConst(5.6);
    try c.writeChunk(lx.OpCode.CONST, 5);
    try c.writeChunk(a2, 5);

    try c.writeChunk(lx.OpCode.DIV, 5);
    try c.writeChunk(lx.OpCode.NEGATE, 5);

    try c.writeChunk(lx.OpCode.RETURN, 5);
    try debug.dissChunk(&c, stdout, "test");
    vm.interpret(&c);
    try vm.run();
    try bw.flush(); // don't forget to flush!

}
