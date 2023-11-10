const std = @import("std");
const Allocator = std.mem.Allocator;

const String = @import("./object.zig").Object.String;
const Value = @import("./value.zig").Value;
const growCapacity = @import("./dynamic_array.zig").growCapacity;

const max_load = 0.75;

const Obj = @import("./object.zig").Object;

const Entry = struct {
    key: ?*String = null,
    val: Value = Value.nil,
};

pub const Table = struct {
    const Self = @This();

    count: usize = 0,
    capacity: usize = 0,
    entries: []Entry = &[_]Entry{},
    allocator: Allocator,

    pub fn init(allocator: Allocator) Self {
        return Self{ .allocator = allocator };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.free(self.entries);
        self.count = 0;
        self.capacity = 0;
    }

    pub fn set(self: *Self, key: *String, val: Value) bool {
        const new_size = @as(f64, @floatFromInt(self.count + 1));
        const needed_capacity = @as(f64, @floatFromInt(self.entries.len)) * max_load;
        if (new_size > needed_capacity) {
            const new_capacity = growCapacity(self.capacity);
            self.adjustCapacity(new_capacity);
        }

        const entry = findEntry(self.entries, key);
        const isNew = entry.key == null;

        if (isNew and entry.val.isNil()) {
            self.count += 1;
        }

        entry.key = key;
        entry.val = val;

        return isNew;
    }

    pub fn get(self: *Self, key: *String) ?*Value {
        if (self.count == 0) return null;

        const entry = findEntry(self.entries, key);

        if (entry.key == null) return null;

        return &entry.val;
    }

    pub fn delete(self: *Self, key: *String) bool {
        if (self.count == 0) return false;

        const entry = findEntry(self.entries, key);
        if (entry.key == null) return false;

        entry.key = null;
        entry.val = Value.BooleanValue(true);

        return true;
    }

    pub fn addAll(from: *Self, to: *Table) void {
        for (from.entries) |entry| {
            if (entry.key) |key| {
                _ = to.set(key, entry.val);
            }
        }
    }

    pub fn findString(self: *Self, chars: []const u8, hash: u32) ?*String {
        if (self.count == 0) return null;

        var index = hash % self.capacity;
        while (true) {
            const entry = &self.entries[index];
            if (entry.key == null) {
                // Stop if we find an empty non-tombstone entry.
                if (entry.val.isNil()) return null;
            } else if (entry.key.?.chars.len == chars.len
                   and entry.key.?.hash == hash
                   and std.mem.eql(u8, entry.key.?.chars, chars)) {
                return entry.key;
            }

            index += 1;
            index %= self.capacity;
        }
    }

    fn findEntry(entries: []Entry, key: *String) *Entry {
        const capacity = entries.len;
        var index = key.hash % capacity;
        var tombstone: ?*Entry = null;

        while (true) {
            const entry = &entries[index];

            if (entry.key == null) {
                if (entry.val.isNil()) {
                    // not a tombstone
                    return tombstone orelse entry;
                } else {
                    tombstone = entry;
                    return entry;
                }
            } else if (entry.key == key) {
                return entry;
            }

            index += 1;
            index %= capacity;
        }
    }

    fn adjustCapacity(self: *Self, new_capacity: usize) void {
        const new_entries = self.allocator.alloc(Entry, new_capacity) catch @panic("Error creating Entries\n");

        for (new_entries) |*e| {
            e.* = Entry{};
        }

        self.count = 0;
        for (self.entries) |entry| {
            if (entry.key) |key| {
                const dest: *Entry = findEntry(new_entries, key);
                dest.key = entry.key;
                dest.val = entry.val;
                self.count += 1;
            }
        }

        self.allocator.free(self.entries);

        self.capacity = new_capacity;
        self.entries = new_entries;
    }
};

test "create a Table" {
    const Vm = @import("./vm.zig").Vm;
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var vm = Vm.init(allocator);
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    const key = String.copy(&vm, "a");
    const val = Value.NumberValue(1.0);

    try expect(table.set(key, val) == true);
    try expect(table.get(key).?.number == val.number);
}

test "create a Table and add many antries until we adjustCapacity" {
    const Vm = @import("./vm.zig").Vm;
    const expect = std.testing.expect;
    const allocator = std.testing.allocator;

    var vm = Vm.init(allocator);
    defer vm.deinit();

    var table = Table.init(allocator);
    defer table.deinit();

    // capacity initialised at 0
    try expect(table.capacity == 0);

    _ = table.set(String.copy(&vm, "a"), Value.NumberValue(1.0));

    // after first entry we reached capacity limit, so capacity increased
    try expect(table.capacity == 8);

    _ = table.set(String.copy(&vm, "b"), Value.NumberValue(2.0));
    _ = table.set(String.copy(&vm, "c"), Value.NumberValue(3.0));
    _ = table.set(String.copy(&vm, "d"), Value.NumberValue(4.0));
    _ = table.set(String.copy(&vm, "e"), Value.NumberValue(5.0));
    _ = table.set(String.copy(&vm, "f"), Value.NumberValue(6.0));
    _ = table.set(String.copy(&vm, "g"), Value.NumberValue(7.0));

    // after 6 entries, we reached 0.75 of capacity (8) so capacity doubled
    try expect(table.capacity == 16);

    _ = table.set(String.copy(&vm, "h"), Value.NumberValue(8.0));
    _ = table.set(String.copy(&vm, "i"), Value.NumberValue(9.0));
    _ = table.set(String.copy(&vm, "j"), Value.NumberValue(1.0));
    _ = table.set(String.copy(&vm, "k"), Value.NumberValue(2.0));
    _ = table.set(String.copy(&vm, "l"), Value.NumberValue(3.0));
    _ = table.set(String.copy(&vm, "m"), Value.NumberValue(4.0));

    // after 12 entries, we reached 0.75 of capacity (16) so capacity doubled
    try expect(table.capacity == 32);
}
