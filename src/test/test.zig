const std = @import("std");
const lx = @import("../zlox.zig");
const mem = std.mem;
const Allocator = std.mem.Allocator;
const AnyWriter = std.io.AnyWriter;

pub fn setup(alloc: Allocator, writer: AnyWriter) !*lx.VM {
    const gc = try alloc.create(lx.GC);
    errdefer alloc.destroy(gc);
    const vm = try alloc.create(lx.VM);
    errdefer alloc.destroy(vm);

    lx.GC.init(gc, alloc);
    try lx.VM.init(vm, gc, writer);
    gc.vm = vm; // todo refactor

    return vm;
}

pub fn cleanup(alloc: Allocator, vm: *lx.VM) void {
    const gc = vm.gc;
    vm.deinit();
    alloc.destroy(vm);
    gc.deinit();
    alloc.destroy(gc);
}

const tests = struct {
    pub usingnamespace @import("assignment.zig");
};
test "test all" {
    //std.testing.refAllDeclsRecursive(@import("assignment.zig"));
}
