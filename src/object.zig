const std = @import("std");
const Allocator = std.mem.Allocator;
const lx = @import("zlox.zig");

// must have some size
pub const Obj = struct {
    tp: Type,
    next: ?*Self = null,

    const Self = @This();

    pub inline fn as(s: *Obj, comptime t: type) *t {
        return @fieldParentPtr(t, "obj", s);
    }

    // this function cannot exist because switch prongs cannot return multipple types
    //pub inline fn asT(s: *Obj, t: Type) type {
    //return switch (t.to_struct()) {
    //inline else => |tp| @fieldParentPtr(tp, "obj", s),
    //};
    //}

    pub fn is(s: *Obj, comptime t: type) bool {
        return Type.from_struct(t) == s.tp;
    }
};

pub const Type = enum {
    String,

    pub fn from_struct(comptime t: type) Type {
        return switch (t) {
            String => .String,
            else => unreachable,
        };
    }

    pub fn as_type(t: Type) type {
        return switch (t) {
            .String => String,
            //else => unreachable,
        };
    }
};

const ctx = struct {
    pub fn hash(_: @This(), k: String) u64 {
        return k.hv;
    }

    pub fn eql(_: @This(), a: String, b: String) bool {
        return a.hv == b.hv;
    }
};

pub const StrMap = std.HashMap(String, void, ctx, 75);

pub const String = struct {
    obj: Obj,
    chars: [:0]u8,
    hv: u64,

    const Self = @This();

    pub fn hash(s: []const u8) u64 {
        var h: u64 = 2166136261;
        for (s) |c| {
            h ^= c;
            h +%= 16777619;
        }
        return h;
    }

    pub fn allocString(vm: *lx.VM, alloc: Allocator, str: []const u8) !*String {
        const h = hash(str);
        const hvOnly = String{
            .hv = h,
            .chars = undefined,
            .obj = undefined,
        };

        if (vm.strings.getKeyPtr(hvOnly)) |s| {
            return s;
        } else {
            var s = try alloc.create(String);
            s.obj = .{ .tp = Type.from_struct(Self) };
            s.chars = try alloc.dupeZ(u8, str);
            s.hv = h;

            const p = if (vm.objects) |prev| &prev.next else &vm.objects;
            p.* = &s.obj;

            try vm.strings.putNoClobber(s.*, {});
            return s;
        }
    }

    pub fn takeString(alloc: Allocator, str: [:0]u8) !*String {
        var s = try alloc.create(String);
        s.obj = .{ .tp = Type.from_struct(Self) };
        s.chars = str;
        return s;
    }

    pub fn destroy(s: *Self, alloc: Allocator) void {
        alloc.free(s.chars);
        alloc.destroy(s);
    }
};

const ObfContainer = @This();

test "object" {}
