const std = @import("std");
const Allocator = std.mem.Allocator;

const debug_gc = @import("./debug.zig").debug_garbage_collection;
const debug_stress_gc = true;

pub const GCAllocator = struct {
    parent_allocator: Allocator,

    const Self = @This();

    pub fn init(parent_allocator: Allocator) Self {
        return .{ .parent_allocator = parent_allocator };
    }

    pub fn allocator(self: *Self) Allocator {
        return .{
            .ptr = self,
            .vtable = &.{
                .alloc = alloc,
                .resize = resize,
                .free = free,
            },
        };
    }

    fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self: *Self = @ptrCast(@alignCast(ctx));

        return self.parent_allocator.rawAlloc(len, ptr_align, ret_addr);
    }

    fn resize(ctx: *anyopaque, buf: []u8, log2_buf_align: u8, new_len: usize, ret_addr: usize) bool {
        const self: *Self = @ptrCast(@alignCast(ctx));

        if (new_len > buf.len) {
            if (debug_stress_gc) {
                try self.collectGarbage();
            }
        }

        return self.parent_allocator.rawResize(buf, log2_buf_align, new_len, ret_addr);
    }

    fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self: *Self = @ptrCast(@alignCast(ctx));

        return self.parent_allocator.rawFree(buf, buf_align, ret_addr);
    }

    fn collectGarbage(self: *Self) !void {
        _ = self;
        if (debug_gc) {
            std.log.debug("GC: begin", .{});
        }

        if (debug_gc) {
            std.log.debug("GC: end", .{});
        }
    }
};
