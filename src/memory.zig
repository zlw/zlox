const std = @import("std");
const Allocator = std.mem.Allocator;

const Vm = @import("./vm.zig").Vm;
const Chunk = @import("./chunk.zig").Chunk;
const Value = @import("./value.zig").Value;
const Object = @import("./object.zig").Object;
const Table = @import("./table.zig").Table;
const Compiler = @import("./compiler.zig").Compiler;
const DynamicArray = @import("./dynamic_array.zig").DynamicArray;
const printValue = @import("./value.zig").printValue;

const debug_gc = @import("./debug.zig").debug_garbage_collection;
const stress_gc = false;

const GC_HEAP_GROW_FACTOR = 2;
// Investigate why smaller heap breaks ./test/limit/loop_too_large.lox
const GC_DEFAULT_NEXT_GC = (1024 * 1024) * 2;

const GrayObjects = DynamicArray(*Object);

pub const GCAllocator = struct {
    parent_allocator: Allocator,
    vm: *Vm,
    grayObjects: GrayObjects,
    bytesAllocated: usize = 0,
    nextGC: usize = GC_DEFAULT_NEXT_GC,

    const Self = @This();

    pub fn init(parent_allocator: Allocator, vm: *Vm) Self {
        return .{ .parent_allocator = parent_allocator, .vm = vm, .grayObjects = GrayObjects.init(parent_allocator) };
    }

    pub fn deinit(self: *Self) void {
        self.grayObjects.deinit();
        self.freeObjects();
    }

    pub fn allocator(self: *Self) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{ .alloc = alloc, .resize = resize, .free = free },
        };
    }

    // begin: Allocator interface

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));

        if ((self.bytesAllocated > self.nextGC)) {
            try self.collectGarbage();
        }

        if (self.parent_allocator.rawAlloc(len, ptr_align, ret_addr)) |out| {
            self.bytesAllocated += len;
            return out;
        } else {
            return null;
        }
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));

        // if (new_len > buf.len) {
        //     if ((self.bytesAllocated + (new_len - buf.len) > self.nextGC) or stress_gc) {
        //         try self.collectGarbage();
        //     }
        // }

        if ((self.bytesAllocated > self.nextGC)) {
            try self.collectGarbage();
        }

       if (self.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr)) {
            if (new_len > buf.len) {
                self.bytesAllocated += new_len - buf.len;
            } else {
                self.bytesAllocated -= buf.len - new_len;
            }
            return true;
        } else {
            return false;
        }
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));

        self.parent_allocator.rawFree(buf, buf_align, ret_addr);
        self.bytesAllocated -= buf.len;
    }

    // end: Allocator interface

    fn collectGarbage(self: *Self) !void {
        const before = self.bytesAllocated;

        if (debug_gc) {
            std.log.debug("GC: begin", .{});
        }

        self.markRoots();
        self.traceReferences();
        self.tableRemoveWhite(&self.vm.strings);
        self.sweep();

        self.nextGC = self.bytesAllocated * GC_HEAP_GROW_FACTOR;

        if (debug_gc) {
            std.log.debug("GC: end", .{});
            std.log.debug("GC: collected {d} bytes (from {d} to {d}) next at {d}\n", .{ before - self.bytesAllocated, before, self.bytesAllocated, self.nextGC });
        }
    }

    fn markRoots(self: *Self) void {
        var i: usize = undefined;

        // mark Object in Values sitting on the stack
        i = 0;
        while (i < self.vm.stack_top) : (i += 1) {
            self.markValue(&self.vm.stack[i]);
        }

        // mark Object.Closure in CallFrames
        i = 0;
        while (i < self.vm.framesCount) : (i += 1) {
            self.markObject(&self.vm.frames[i].closure.object);
        }

        // mark Upvalues
        var upvalue = self.vm.openUpvalues;
        while (upvalue != null) : (upvalue = upvalue.?.next) {
            self.markObject(&upvalue.?.object);
        }

        // mark Object.String (keys) and Value (values) in global variables
        self.markTable(&self.vm.globals);

        self.markCompilerRoots();

        self.markObject(&self.vm.initString.?.object);
    }

    fn markValue(self: *Self, value: *Value) void {
        switch (value.*) {
            .object => self.markObject(value.object),
            .nil, .boolean, .number => return,
        }
    }

    fn markObject(self: *Self, object: *Object) void {
        if (object.isMarked) return;

        if (comptime debug_gc) {
            std.log.debug("GC: {} mark", .{object});
            printValue(Value.ObjectValue(object));
        }

        object.isMarked = true;
        self.grayObjects.appendItem(object);
    }

    fn markTable(self: *Self, table: *Table) void {
        var i: usize = 0;
        while (i < table.capacity) : (i += 1) {
            const entry = &table.entries[i];

            self.markObject(&entry.key.?.object);
            self.markValue(&entry.val);
        }
    }

    fn markArray(self: *Self, array: *Chunk.ValueArray) void {
        for (array.items) |*item| {
            self.markValue(item);
        }
    }

    fn markCompilerRoots(self: *Self) void {
        var compiler: ?*Compiler = self.vm.parser.?.compiler;

        while (compiler) |_| : (compiler = compiler.?.enclosing) {
            self.markObject(&compiler.?.function.object);
        }
    }

    fn traceReferences(self: *Self) void {
        for (self.grayObjects.items) |object| {
            self.blackenObject(object);
        }
    }

    fn blackenObject(self: *Self, object: *Object) void {
        if (comptime debug_gc) {
            std.log.debug("GC: {any} blacken", .{object});
        }

        switch (object.objectType) {
            .Upvalue => self.markValue(&object.asUpvalue().closed),
            .Function => {
                const function = object.asFunction();
                self.markObject(&function.name.?.object);
                self.markArray(&function.chunk.constants);
            },
            .Closure => {
                const closure = object.asClosure();
                self.markObject(&closure.function.object);
                var i: usize = 0;
                while (i < closure.upvalueCount) : (i += 1) {
                    const upvalue = &closure.upvalues[i];
                    self.markObject(&upvalue.*.object);
                }
            },
            .Class => {
                const class = object.asClass();
                self.markObject(&class.name.object);
                self.markTable(&class.methods);
            },
            .Instance => {
                const instance = object.asInstance();
                self.markObject(&instance.class.object);
                self.markTable(&instance.fields);
            },
            .BoundMethod => {
                const bound = object.asBoundMethod();
                self.markValue(&bound.receiver);
                self.markObject(&bound.method.object);
            },
            .NativeFunction, .String => return,
        }
    }

    fn tableRemoveWhite(self: *Self, table: *Table) void {
        _ = self;
        var i: usize = 0;
        while (i < table.capacity) : (i += 1) {
            const entry = &table.entries[i];
            if (entry.key != null and !entry.key.?.object.isMarked) {
                _ = table.delete(entry.key.?);
            }
        }
    }

    fn sweep(self: *Self) void {
        var previous: ?*Object = null;
        var object: ?*Object = self.vm.objects;

        while (object != null) {
            if (object.?.isMarked) {
                object.?.isMarked = false;
                previous = object;
                object = object.?.next;
            } else {
                const unreached: *Object = object.?;
                object = object.?.next;

                if (previous != null) {
                    previous.?.next = object;
                } else {
                    self.vm.objects = object;
                }

                unreached.destroy(self.vm);
            }
        }
    }

    pub fn freeObjects(self: *Self) void {
        var obj = self.vm.objects;
        var total_objects: u64 = 0;

        while (obj) |object| {
            if (comptime debug_gc) {
                total_objects += 1;
            }
            const next = object.next;
            object.destroy(self.vm);
            obj = next;
        }

        if (comptime debug_gc) {
            std.log.debug("GC: Objects freed: {d}", .{total_objects});
        }
    }
};
