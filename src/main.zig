const std = @import("std");
const debug = @import("debug.zig");
const lx = @import("zlox.zig");
const Allocator = std.mem.Allocator;
const fs = std.fs;

pub fn main() !void {
    std.debug.print("\n", .{});
    var gpa = std.heap.GeneralPurposeAllocator(.{ .safety = true }){};
    defer _ = gpa.detectLeaks();

    const alloc = gpa.allocator();
    const gc = try alloc.create(lx.GC);
    defer alloc.destroy(gc);
    const vm = try alloc.create(lx.VM);
    defer alloc.destroy(vm);

    lx.GC.init(gc, alloc);
    try lx.VM.init(vm, gc, std.io.getStdOut().writer().any());
    defer vm.deinit();
    gc.vm = vm; // todo refactor
    defer gc.deinit();

    const args = std.os.argv;
    if (args.len == 1) {
        try repl(alloc, vm);
    } else if (args.len == 2) {
        try runFile(alloc, std.mem.span(args[1]), vm);
    } else {
        unreachable;
    }
}

pub fn runFile(alloc: Allocator, fileName: [:0]const u8, vm: *lx.VM) !void {
    const stat = try fs.cwd().statFile(fileName);
    const buf = try alloc.allocSentinel(u8, stat.size, 0);
    defer alloc.free(buf);

    const code = try fs.cwd().readFile(fileName, buf);
    if (code.len != buf.len) @panic("read error");

    try vm.interpret(buf);
}

pub fn repl(alloc: Allocator, vm: *lx.VM) !void {
    _ = alloc; // autofix
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();
    try stdout.print("zlox\n", .{});

    const stdin = std.io.getStdIn().reader();

    var buf = [_:0]u8{0} ** 1024;

    while (true) {
        try bw.flush();
        try stdout.print("> ", .{});
        const line = try stdin.readUntilDelimiterOrEof(&buf, '\n');

        if (line) |b| {
            buf[b.len + 1] = 0;
            try vm.interpret(buf[0..b.len :0]);
        } else {
            break;
        }
    }
    try bw.flush();
}
