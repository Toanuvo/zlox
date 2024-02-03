const std = @import("std");
const mem = std.mem;

pub fn realloc(alloc: mem.Allocator, old: anytype, newSize: usize) !@TypeOf(old) {
    return try alloc.realloc(old, newSize);
}
