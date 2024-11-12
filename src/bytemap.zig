const std = @import("std");
const testing = std.testing;

pub const SearchIndex = struct {
    value: usize,
    exists: bool,
};

pub fn SortedByteMap(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const Entry = struct {
            key: u8,
            value: T,
        };

        entries: std.ArrayList(Entry),

        pub fn init(allocator: std.mem.Allocator) Self {
            return .{
                .entries = std.ArrayList(Entry).init(allocator),
            };
        }

        pub fn deinit(self: Self) void {
            self.entries.deinit();
        }

        pub fn count(self: Self) usize {
            return self.entries.items.len;
        }

        pub fn at(self: Self, index: usize) T {
            return self.entries.items[index].value;
        }

        pub fn get(self: *Self, key: u8) ?T {
            const index = self.search(key);
            if (index.exists) {
                return self.entries.items[index.value].value;
            }
            return null;
        }

        pub fn getPtr(self: *Self, key: u8) ?*T {
            const index = self.search(key);
            if (index.exists) {
                return &self.entries.items[index.value].value;
            }
            return null;
        }

        pub fn put(self: *Self, key: u8, value: T) !?T {
            const index = self.search(key);
            if (index.exists) {
                const prev = self.entries.items[index.value];
                self.entries.items[index.value].value = value;
                return prev.value;
            }
            try self.entries.insert(index.value, .{ .key = key, .value = value });
            return null;
        }

        pub fn remove(self: *Self, key: u8) ?T {
            const index = self.search(key);
            if (index.exists) {
                return self.entries.orderedRemove(index.value).value;
            }
            return null;
        }

        pub fn search(self: Self, key: u8) SearchIndex {
            const items = self.entries.items;
            var low: usize = 0;
            var high: usize = items.len;
            while (low < high) {
                // Avoid overflowing in the midpoint calculation
                const mid = low + (high - low) / 2;
                switch (std.math.order(key, items[mid].key)) {
                    .eq => return .{ .value = mid, .exists = true },
                    .gt => low = mid + 1,
                    .lt => high = mid,
                }
            }
            return .{ .value = low, .exists = false };
        }
    };
}

test "SortedByteMap.get: 1" {
    var map = SortedByteMap(i64).init(testing.allocator);
    defer map.deinit();
    try map.entries.append(.{ .key = '0', .value = 123 });
    const value = map.get('0');
    try testing.expectEqual(123, value);
}

test "SortedByteMap.put: 1" {
    var map = SortedByteMap(i64).init(testing.allocator);
    defer map.deinit();
    try map.entries.append(.{ .key = '0', .value = 123 });
    _ = try map.put('0', 42);
    const value = map.get('0');
    try testing.expectEqual(42, value);
}

test "SortedByteMap.put: 2" {
    var map = SortedByteMap(i64).init(testing.allocator);
    defer map.deinit();
    _ = try map.put('0', 0);
    _ = try map.put('2', 2);
    _ = try map.put('1', 1);

    const Entry = SortedByteMap(i64).Entry;
    try testing.expectEqualDeep(map.entries.items, &[_]Entry{
        .{ .key = '0', .value = 0 },
        .{ .key = '1', .value = 1 },
        .{ .key = '2', .value = 2 },
    });
}

test "SortedByteMap.remove: 1" {
    var map = SortedByteMap(i64).init(testing.allocator);
    defer map.deinit();

    _ = try map.put('8', 8);
    _ = try map.put('1', 1);
    _ = map.remove('7');
    _ = try map.put('4', 4);
    _ = try map.put('9', 9);
    _ = map.remove('8');

    const Entry = SortedByteMap(i64).Entry;
    try testing.expectEqualDeep(map.entries.items, &[_]Entry{
        .{ .key = '1', .value = 1 },
        .{ .key = '4', .value = 4 },
        .{ .key = '9', .value = 9 },
    });
}
