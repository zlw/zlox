const std = @import("std");
const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;
const Vm = @import("./vm.zig").Vm;

const Allocator = std.mem.Allocator;

pub const ObjectType = enum { String, Function };

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
            .Function => self.asFunction().destroy(vm),
        }
    }

    pub inline fn asString(self: *Object) *String {
        return @fieldParentPtr(String, "object", self);
    }

    pub inline fn asFunction(self: *Object) *Function {
        return @fieldParentPtr(Function, "object", self);
    }

    pub inline fn isA(value: Value, objectType: ObjectType) bool {
        return value == .object and value.object.objectType == objectType;
    }

    pub const String = struct {
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
            const str = Object.create(vm, String, .String);
            
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

    pub const Function = struct {
        object: Object,
        arity: usize,
        chunk: Chunk,
        name: ?*String,

        pub fn create(vm: *Vm) *Function {
            const function = Object.create(vm, Function, .Function);

            function.arity = 0;
            function.name = null;
            function.chunk = Chunk.init(vm.allocator);

            return function;
        }

        fn destroy(self: *Function, vm: *Vm) void {
            self.chunk.deinit();
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

test "create a Function" {
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var vm = Vm.init(allocator);
    defer vm.deinit();

    const func = Object.Function.create(&vm);

    try expect(func.name == null);
    try expect(func.arity == 0);
}
