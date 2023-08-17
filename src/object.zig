const std = @import("std");
const Value = @import("./value.zig").Value;
const Vm = @import("./vm.zig").Vm;

const Allocator = std.mem.Allocator;

pub const ObjectType = enum { String };

pub const Object = struct {
    objectType: ObjectType,

    pub fn create(allocator: *Allocator, comptime T: type, objectType: ObjectType) *T {
        const ptrT = allocator.create(T) catch @panic("Error creating Obj\n");
        ptrT.object = Object{ .objectType = objectType };
        return ptrT;
    }

    pub inline fn asString(self: *Object) *String {
        return @fieldParentPtr(String, "object", self);
    }

    pub inline fn isA(value: Value, objectType: ObjectType) bool {
        return value == .object and value.object.objectType == objectType;
    }

    pub const String = struct {
        const Self = @This();

        object: Object,
        length: usize,
        chars: []const u8,

        pub fn copy(allocator: *Allocator, chars: []const u8) *String {
            const heap = allocator.alloc(u8, chars.len) catch @panic("Error copying String\n");
            std.mem.copy(u8, heap, chars);
            return allocate(allocator, heap);
        }

        fn allocate(allocator: *Allocator, bytes: []const u8) *String {
            const str = Object.create(allocator, Self, .String);
            str.chars = bytes;
            return str;
        }
    };

};
