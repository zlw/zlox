const std = @import("std");
const Value = @import("./value.zig").Value;
const Chunk = @import("./chunk.zig").Chunk;
const Vm = @import("./vm.zig").Vm;
const Table = @import("./table.zig").Table;

const Allocator = std.mem.Allocator;

const debug_garbage_collection = @import("./debug.zig").debug_garbage_collection;

pub const ObjectType = enum { String, Function, NativeFunction, Closure, Upvalue, Class, Instance };

pub const Object = struct {
    objectType: ObjectType,
    isMarked: bool = false,
    next: ?*Object,

    fn create(vm: *Vm, comptime T: type, objectType: ObjectType) *T {
        const ptrT = vm.allocator.create(T) catch @panic("Error creating Obj\n");
        ptrT.object = Object{ .objectType = objectType, .next = vm.objects };
        vm.objects = &ptrT.object;

        if (comptime debug_garbage_collection) {
            std.log.debug("GC: {any} allocate {d} bytes for {s}", .{ @intFromPtr(&ptrT.object), @sizeOf(T), @typeName(T) });
        }

        return ptrT;
    }

    pub fn destroy(self: *Object, vm: *Vm) void {
        if (comptime debug_garbage_collection) {
            std.log.debug("GC: {any} free type {any}", .{ @intFromPtr(self), self.objectType });
        }

        switch (self.objectType) {
            .String => self.asString().destroy(vm),
            .Function => self.asFunction().destroy(vm),
            .NativeFunction => self.asNativeFunction().destroy(vm),
            .Closure => self.asClosure().destroy(vm),
            .Upvalue => self.asUpvalue().destroy(vm),
            .Class => self.asClass().destroy(vm),
            .Instance => self.asInstance().destroy(vm),
        }
    }

    pub inline fn asString(self: *Object) *String {
        return @fieldParentPtr(String, "object", self);
    }

    pub inline fn asFunction(self: *Object) *Function {
        return @fieldParentPtr(Function, "object", self);
    }

    pub inline fn asNativeFunction(self: *Object) *NativeFunction {
        return @fieldParentPtr(NativeFunction, "object", self);
    }

    pub inline fn asClosure(self: *Object) *Closure {
        return @fieldParentPtr(Closure, "object", self);
    }

    pub inline fn asUpvalue(self: *Object) *Upvalue {
        return @fieldParentPtr(Upvalue, "object", self);
    }

    pub inline fn asClass(self: *Object) *Class {
        return @fieldParentPtr(Class, "object", self);
    }

    pub inline fn asInstance(self: *Object) *Instance {
        return @fieldParentPtr(Instance, "object", self);
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

            vm.push(Value.ObjectValue(&str.object));

            _ = vm.strings.set(str, Value.NilValue());

            _ = vm.pop();

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
        upvalueCount: u8,
        chunk: Chunk,
        name: ?*String,

        pub fn create(vm: *Vm) *Function {
            const function = Object.create(vm, Function, .Function);

            function.arity = 0;
            function.upvalueCount = 0;
            function.name = null;
            function.chunk = Chunk.init(vm.allocator, vm);

            return function;
        }

        fn destroy(self: *Function, vm: *Vm) void {
            self.chunk.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const NativeFunction = struct {
        object: Object,
        function: Fn,

        pub const Fn = *const fn (args: Value) Value;

        pub fn create(vm: *Vm, function: Fn) *NativeFunction {
            const native = Object.create(vm, NativeFunction, .NativeFunction);

            native.function = function;

            return native;
        }

        pub fn destroy(self: *NativeFunction, vm: *Vm) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Closure = struct {
        object: Object,
        function: *Object.Function,
        upvalues: []*Upvalue,
        upvalueCount: u8,

        pub fn create(vm: *Vm, function: *Object.Function) *Closure {
            const closure = Object.create(vm, Closure, .Closure);

            closure.function = function;
            closure.upvalues = vm.allocator.alloc(*Upvalue, function.upvalueCount) catch @panic("Error creating Closure Upvalues");
            closure.upvalueCount = function.upvalueCount;

            return closure;
        }

        pub fn destroy(self: *Closure, vm: *Vm) void {
            vm.allocator.free(self.upvalues);
            vm.allocator.destroy(self);
        }
    };

    pub const Upvalue = struct {
        object: Object,
        location: *Value,
        closed: Value = Value.nil,
        next: ?*Upvalue = null,

        pub fn create(vm: *Vm, location: *Value) *Upvalue {
            const upvalue = Object.create(vm, Upvalue, .Upvalue);

            upvalue.location = location;

            return upvalue;
        }

        pub fn destroy(self: *Upvalue, vm: *Vm) void {
            vm.allocator.destroy(self);
        }
    };

    pub const Class = struct {
        object: Object,
        name: *String,
        methods: Table,

        pub fn create(vm: *Vm, name: *String) *Class {
            const class = Object.create(vm, Class, .Class);

            class.name = name;
            class.methods = Table.init(vm.allocator);

            return class;
        }

        pub fn destroy(self: *Class, vm: *Vm) void {
            self.methods.deinit();
            vm.allocator.destroy(self);
        }
    };

    pub const Instance = struct {
        object: Object,
        class: *Class,
        fields: Table,

        pub fn create(vm: *Vm, class: *Class) *Instance {
            const instance = Object.create(vm, Instance, .Instance);

            instance.class = class;
            instance.fields = Table.init(vm.allocator);

            return instance;
        }

        pub fn destroy(self: *Instance, vm: *Vm) void {
            self.fields.deinit();
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
