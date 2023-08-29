const std = @import("std");
const Value = @import("./value.zig").Value;
const Vm = @import("./vm.zig").Vm;

const Allocator = std.mem.Allocator;

pub const ObjectType = enum { String };

pub const Object = struct {
    objectType: ObjectType,
    next: ?*Object,

    fn create(vm: *Vm, comptime T: type, objectType: ObjectType) *T {
        const ptrT = vm.allocator.create(T) catch @panic("Error creating Obj\n");
        ptrT.object = Object{ .objectType = objectType, .next = vm.objects };
        vm.objects = &ptrT.object;
        return ptrT;
    }

    pub fn destroy(self: *Object, vm: *Vm) void {
        switch (self.objectType) {
            .String => self.asString().destroy(vm),
        }
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
        hash: u32,

        pub fn copy(vm: *Vm, chars: []const u8) *String {
            const hash = hashBytes(chars);

            const maybe_cached = vm.strings.findString(chars, hash);
            if (maybe_cached) |cached| return cached;

            const heap = vm.allocator.alloc(u8, chars.len) catch {
                @panic("Error copying String\n");
            };
            
            std.mem.copy(u8, heap, chars);
            
            return allocate(vm, heap, hash);
        }

        pub fn take(vm: *Vm, chars: []const u8) *String {
            const hash = hashBytes(chars);

            const maybe_cached = vm.strings.findString(chars, hash);
            if (maybe_cached) |cached| {
                vm.allocator.free(chars);
                return cached;
            }

            return allocate(vm, chars, hash);
        }

        fn allocate(vm: *Vm, bytes: []const u8, hash: u32) *String {
            const str = Object.create(vm, Self, .String);
            
            str.chars = bytes;
            str.hash = hash;

            _ = vm.strings.set(str, Value.NilValue());
            
            return str;
        }

        fn destroy(self: *String, vm: *Vm) void {
            vm.allocator.free(self.chars);
            vm.allocator.destroy(self);
        }
    };
};

fn hashBytes(bytes: []const u8) u32 {
    var hash: u32 = 2166136261;

    for (bytes) |byte| {
        hash ^= byte;
        hash *%= 16777619;
    }

    return hash;
}

test "generate hash for bytes" {
    const expect = std.testing.expect;

    try expect(hashBytes("foo") == 2851307223);
    try expect(hashBytes("bar") == 1991736602);
}

test "create a String" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var vm = Vm.init(allocator);
    defer vm.deinit();

    const foo = Object.String.copy(&vm, "foo");
    // try expect(str.length == 3);
    try expect(std.mem.eql(u8, foo.chars, "foo"));
    try expect(foo.hash == 2851307223);

    const str = Object.String.copy(&vm, "ąźć");
    // try expect(str.length == 3);
    try expect(std.mem.eql(u8, str.chars, "ąźć"));
    try expect(str.hash == 3075109956);
}
