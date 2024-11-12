const std = @import("std");
const testing = std.testing;
const SortedByteMap = @import("bytemap.zig").SortedByteMap;

pub fn RadixTree(comptime T: type) type {
    return struct {
        const ChildrenMap = SortedByteMap(Node);

        const InsertErrors = error{
            NotImplemented,
            OutOfMemory,
        };

        const Node = struct {
            seq: []u8,
            children: ChildrenMap,
            value: ?T = null,

            fn init(allocator: std.mem.Allocator, seq: []const u8, value: ?T) !Node {
                return .{
                    .seq = try allocator.dupe(u8, seq),
                    .children = ChildrenMap.init(allocator),
                    .value = value,
                };
            }

            fn deinit(self: Node, allocator: std.mem.Allocator) void {
                for (self.children.entries.items) |entry| {
                    entry.value.deinit(allocator);
                }
                self.children.deinit();
                allocator.free(self.seq);
            }

            fn insert(self: *Node, allocator: std.mem.Allocator, seq: []const u8, value: T) InsertErrors!void {
                const i = std.mem.indexOfDiff(u8, self.seq, seq) orelse {
                    // case: the new seq is the current node.
                    self.value = value;
                    return;
                };
                // case: the current node is a parent of the new seq.
                if (i == self.seq.len) {
                    try self.insertChild(allocator, seq[i..], value);
                    return;
                }
                // case: the new seq is a parent of the current node.
                if (i == seq.len) {
                    std.mem.copyForwards(u8, self.seq, self.seq[i..]);
                    self.seq = try allocator.realloc(self.seq, self.seq.len - i);
                    var prev = self.*;
                    self.* = try Node.init(allocator, seq, value);
                    errdefer prev.deinit(allocator);
                    _ = try self.children.put(prev.seq[0], prev);
                    return;
                }
                // case: the current node and the new seq share a common parent.
                std.mem.copyForwards(u8, self.seq, self.seq[i..]);
                self.seq = try allocator.realloc(self.seq, self.seq.len - i);
                const prev = self.*;
                var new = try Node.init(allocator, seq[i..], value);
                errdefer new.deinit(allocator);
                self.* = try Node.init(allocator, seq[0..i], null);
                _ = try self.children.put(new.seq[0], new);
                _ = try self.children.put(prev.seq[0], prev);
            }

            fn insertChild(self: *Node, allocator: std.mem.Allocator, seq: []const u8, value: T) InsertErrors!void {
                if (self.children.getPtr(seq[0])) |node| {
                    try node.insert(allocator, seq, value);
                } else {
                    var node = try Node.init(allocator, seq, value);
                    errdefer node.deinit(allocator);
                    _ = try self.children.put(seq[0], node);
                }
            }

            fn lookup(self: *Node, seq: []const u8) ?T {
                if (!std.mem.startsWith(u8, seq, self.seq)) {
                    return null;
                }
                if (self.seq.len == seq.len) {
                    return self.value;
                }
                const sub_seq = seq[self.seq.len..];
                const child = self.children.getPtr(sub_seq[0]);
                if (child) |node| {
                    return node.lookup(sub_seq);
                }
                return null;
            }

            fn empty(self: *Node) bool {
                return self.value == null and self.children.entries.items.len == 0;
            }

            fn remove(self: *Node, allocator: std.mem.Allocator, seq: []const u8) !?T {
                var value: ?T = null;
                if (!std.mem.startsWith(u8, seq, self.seq)) {
                    return null;
                }
                if (self.seq.len == seq.len) {
                    value = self.value;
                    self.value = null;
                } else {
                    const sub_seq = seq[self.seq.len..];
                    if (self.children.getPtr(sub_seq[0])) |node| {
                        value = try node.remove(allocator, sub_seq);
                        if (node.empty()) {
                            node.deinit(allocator);
                            const old = self.children.remove(sub_seq[0]);
                            std.debug.assert(old != null);
                        }
                    }
                }
                try self.compress(allocator);
                return value;
            }

            fn compress(self: *Node, allocator: std.mem.Allocator) !void {
                if (self.children.entries.items.len != 1 or self.value != null) return;
                // detach the only child
                var child = self.children.entries.items[0].value;
                const old = self.children.remove(child.seq[0]);
                std.debug.assert(old != null);
                errdefer child.deinit(allocator);
                // prefix the child's seq with ours
                const child_len = child.seq.len;
                child.seq = try allocator.realloc(child.seq, self.seq.len + child.seq.len);
                std.mem.copyBackwards(u8, child.seq[self.seq.len..], child.seq[0..child_len]);
                std.mem.copyForwards(u8, child.seq, self.seq);
                // replace ourselves with the child
                self.deinit(allocator);
                self.* = child;
            }

            fn write(self: Node, w: std.io.AnyWriter, indent: usize) !void {
                for (0..indent) |_| {
                    try w.writeByte(' ');
                }
                if (self.seq.len == 0) {
                    try w.writeByte('*');
                } else {
                    try w.writeAll(self.seq);
                }
                if (self.value) |value| {
                    try w.print(" - {d}", .{value});
                }
                try w.writeAll("\n");
                for (self.children.entries.items) |entry| {
                    try entry.value.write(w, indent + 1);
                }
            }
        };

        pub const Iterator = struct {
            const IteratorNode = struct {
                node: Node,
                index: usize = 0,
            };

            stack: std.ArrayList(IteratorNode),

            pub fn init(allocator: std.mem.Allocator, root: ?Node) Iterator {
                const it = Iterator{ .stack = std.ArrayList(IteratorNode).init(allocator) };
                if (root) |node| {
                    try it.stack.append(.{ .node = node });
                }
                return it;
            }

            pub fn deinit(self: Iterator) void {
                self.stack.deinit();
            }

            pub fn next() !?T {
                return null;
            }
        };

        allocator: std.mem.Allocator,
        root: ?Node,

        fn init(allocator: std.mem.Allocator) RadixTree(i64) {
            return .{
                .allocator = allocator,
                .root = null,
            };
        }

        fn deinit(self: *RadixTree(i64)) void {
            if (self.root) |*node| {
                node.deinit(self.allocator);
            }
        }

        fn write(self: *RadixTree(i64), w: std.io.AnyWriter) !void {
            if (self.root) |*node| {
                try node.write(w, 0);
            }
        }

        fn insert(self: *RadixTree(i64), seq: []const u8, value: T) InsertErrors!void {
            if (seq.len == 0) {
                return;
            }
            if (self.root) |*node| {
                try node.insert(self.allocator, seq, value);
            } else {
                self.root = try Node.init(self.allocator, seq, value);
            }
        }

        fn remove(self: *RadixTree(i64), seq: []const u8) !?T {
            if (seq.len == 0) {
                return null;
            }
            if (self.root) |*node| {
                const value = try node.remove(self.allocator, seq);
                if (node.empty()) {
                    node.deinit(self.allocator);
                    self.root = null;
                }
                return value;
            }
            return null;
        }

        fn lookup(self: *RadixTree(i64), seq: []const u8) ?T {
            if (self.root) |*node| {
                return node.lookup(seq);
            }
            return null;
        }

        fn iterator(self: *RadixTree(i64)) !Iterator {
            return Iterator.init(self.allocator, self.root);
        }
    };
}

fn expectTreeEqual(tree: *RadixTree(i64), expected: []const u8) !void {
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    try tree.write(output.writer().any());
    try testing.expectEqualStrings(expected, output.items);
}

test "RadixTree(i64).init: 0" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    const expected = "";
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).insert: 1" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("bar", 2);
    const expected =
        \\*
        \\ bar - 2
        \\ foo - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).insert: 2" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("bar", 2);
    const expected =
        \\*
        \\ bar - 2
        \\ foo - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).insert: 3" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foobar", 1);
    try tree.insert("foo", 2);
    const expected =
        \\foo - 2
        \\ bar - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).insert: 4" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foobar", 1);
    try tree.insert("foopoo", 2);
    const expected =
        \\foo
        \\ bar - 1
        \\ poo - 2
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).insert: 5" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foobar", 1);
    try tree.insert("foopoo", 2);
    try tree.insert("f", 3);
    const expected =
        \\f - 3
        \\ oo
        \\  bar - 1
        \\  poo - 2
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).insert: 6" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foobar", 1);
    try tree.insert("f", 2);
    try tree.insert("f", 3);
    const expected =
        \\f - 3
        \\ oobar - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).remove: 1" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    _ = try tree.remove("foo");
    try expectTreeEqual(&tree, "");
}

test "RadixTree(i64).remove: 2" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("bar", 2);
    _ = try tree.remove("foo");
    const expected =
        \\bar - 2
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).remove: 3" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("foobar", 2);
    _ = try tree.remove("foobar");
    const expected =
        \\foo - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).remove: 4" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("f", 2);
    _ = try tree.remove("f");
    const expected =
        \\foo - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree(i64).lookup: 1" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try testing.expectEqual(1, tree.lookup("foo"));
}

test "RadixTree(i64).lookup: 2" {
    var tree = RadixTree(i64).init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("f", 2);
    try testing.expectEqual(1, tree.lookup("foo"));
    try testing.expectEqual(2, tree.lookup("f"));
}

test "RadixTree(i64).fuzz" {
    const global = struct {
        fn testOne(input: []const u8) anyerror!void {
            // we treat first 2 bytes as 8 bit numbers.
            // we use these two numbers to split the rest of the input.
            if (input.len < 2) {
                return;
            }
            const len1: usize = @intCast(input[0]);
            const len2: usize = @intCast(input[1]);
            if (input.len < 2 + len1 + len2) {
                return;
            }
            var tree = RadixTree(i64).init(testing.allocator);
            defer tree.deinit();

            try tree.insert("foo", 0);
            try tree.insert("bar", 0);
            try tree.insert("foobar", 0);
            try tree.insert("f", 0);

            try tree.insert(input[2 .. len1 + 2], 0);
            try tree.insert(input[2 + len1 .. 2 + len1 + len2], 0);
            _ = try tree.remove(input[2 + len1 + len2 ..]);
        }
    };
    try std.testing.fuzz(global.testOne, .{});
}
