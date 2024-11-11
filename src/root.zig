const std = @import("std");
const testing = std.testing;

const RadixTree = struct {
    const ChildrenMap = std.AutoHashMap(u8, Node);

    const InsertErrors = error{
        NotImplemented,
        OutOfMemory,
    };

    const Node = struct {
        seq: []u8,
        children: ChildrenMap,
        value: ?i64 = null,

        fn init(allocator: std.mem.Allocator, seq: []const u8, value: ?i64) !Node {
            return .{
                .seq = try allocator.dupe(u8, seq),
                .children = ChildrenMap.init(allocator),
                .value = value,
            };
        }

        fn deinit(self: *Node, allocator: std.mem.Allocator) void {
            var it = self.children.valueIterator();
            while (it.next()) |node| {
                node.deinit(allocator);
            }
            self.children.deinit();
            allocator.free(self.seq);
        }

        fn insert(self: *Node, allocator: std.mem.Allocator, seq: []const u8, value: i64) InsertErrors!void {
            const index = std.mem.indexOfDiff(u8, self.seq, seq);
            if (index) |i| {
                // case: the current node is a parent of the new seq.
                if (i == self.seq.len) {
                    try self.insertChild(allocator, seq[i..], value);
                    return;
                }
                // case: the new seq is a parent of the current node.
                if (i == seq.len) {
                    var prev = self.*;
                    std.mem.copyForwards(u8, prev.seq, prev.seq[i..]);
                    prev.seq = try allocator.realloc(prev.seq, prev.seq.len - i);
                    self.* = try Node.init(allocator, seq, value);
                    errdefer prev.deinit(allocator);
                    try self.children.put(seq[i - 1], prev);
                    return;
                }
                // case: the current node and the new seq share a common parent.
                var prev = self.*;
                std.mem.copyForwards(u8, prev.seq, prev.seq[i..]);
                prev.seq = try allocator.realloc(prev.seq, prev.seq.len - i);
                var new = try Node.init(allocator, seq[i..], value);
                errdefer new.deinit(allocator);
                self.* = try Node.init(allocator, seq[0..i], null);
                try self.children.put(new.seq[0], new);
                try self.children.put(prev.seq[0], prev);
            } else {
                // case: the new seq is the current node.
                self.value = value;
            }
        }

        fn insertChild(self: *Node, allocator: std.mem.Allocator, seq: []const u8, value: i64) InsertErrors!void {
            if (self.children.getPtr(seq[0])) |node| {
                try node.insert(allocator, seq, value);
            } else {
                var node = try Node.init(allocator, seq, value);
                errdefer node.deinit(allocator);
                try self.children.put(seq[0], node);
            }
        }

        fn remove(self: *Node, allocator: std.mem.Allocator, seq: []const u8) !bool {
            if (!std.mem.startsWith(u8, seq, self.seq)) {
                return false;
            }
            if (self.seq.len == seq.len) {
                switch (self.children.count()) {
                    0 => return true,
                    1 => {
                        var it = self.children.valueIterator();
                        const child = it.next().?.*;
                        _ = self.children.remove(child.seq[0]);
                        self.deinit(allocator);
                        self.* = child;
                        return false;
                    },
                    else => {
                        self.value = null;
                        return false;
                    },
                }
            }
            const sub_seq = seq[self.seq.len..];
            if (self.children.getPtr(sub_seq[0])) |node| {
                const empty = try node.remove(allocator, sub_seq);
                if (empty) {
                    node.deinit(allocator);
                    _ = self.children.remove(sub_seq[0]);
                }
            }
            return false;
        }

        fn write(self: *Node, w: std.io.AnyWriter, indent: usize) !void {
            var it = self.children.valueIterator();
            while (it.next()) |node| {
                for (0..indent) |_| {
                    try w.writeByte(' ');
                }
                try w.writeAll(node.seq);
                if (node.value) |v| {
                    try w.print(" - {d}", .{v});
                }
                try w.writeAll("\n");
                try node.write(w, indent + 1);
            }
        }
    };

    allocator: std.mem.Allocator,
    root: ?Node,

    fn init(allocator: std.mem.Allocator) RadixTree {
        return .{
            .allocator = allocator,
            .root = Node.init(allocator, "", null) catch unreachable,
        };
    }

    fn deinit(self: *RadixTree) void {
        if (self.root) |*node| {
            node.deinit(self.allocator);
        }
    }

    fn write(self: *RadixTree, w: std.io.AnyWriter) !void {
        if (self.root) |*node| {
            try node.write(w, 0);
        }
    }

    fn insert(self: *RadixTree, seq: []const u8, value: i64) InsertErrors!void {
        if (seq.len == 0) {
            return;
        }
        if (self.root) |*node| {
            try node.insert(self.allocator, seq, value);
        } else {
            self.root = try Node.init(self.allocator, seq, value);
        }
    }

    fn remove(self: *RadixTree, seq: []const u8) !void {
        if (seq.len == 0) {
            return;
        }
        if (self.root) |*node| {
            const empty = try node.remove(self.allocator, seq);
            if (empty) {
                node.deinit(self.allocator);
                self.root = null;
            }
        }
    }
};

fn expectTreeEqual(tree: *RadixTree, expected: []const u8) !void {
    var output = std.ArrayList(u8).init(testing.allocator);
    defer output.deinit();
    try tree.write(output.writer().any());
    try testing.expectEqualStrings(expected, output.items);
}

test "RadixTree: 0" {
    var tree = RadixTree.init(testing.allocator);
    defer tree.deinit();
    const expected = "";
    try expectTreeEqual(&tree, expected);
}

test "RadixTree: 1" {
    var tree = RadixTree.init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("bar", 2);
    const expected =
        \\bar - 2
        \\foo - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree: 2" {
    var tree = RadixTree.init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("bar", 2);
    const expected =
        \\bar - 2
        \\foo - 1
        \\
    ;
    try expectTreeEqual(&tree, expected);
}

test "RadixTree: 3" {
    var tree = RadixTree.init(testing.allocator);
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

test "RadixTree: 4" {
    var tree = RadixTree.init(testing.allocator);
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

test "RadixTree: 5" {
    var tree = RadixTree.init(testing.allocator);
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

test "RadixTree: 6" {
    var tree = RadixTree.init(testing.allocator);
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

test "RadixTree: 7" {
    var tree = RadixTree.init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.remove("foo");
    try expectTreeEqual(&tree, "");
}

test "RadixTree: 8" {
    var tree = RadixTree.init(testing.allocator);
    defer tree.deinit();
    try tree.insert("foo", 1);
    try tree.insert("bar", 2);
    try tree.remove("foo");
    const expected =
        \\bar - 2
        \\
    ;
    try expectTreeEqual(&tree, expected);
}
//
// test "RadixTree: 9" {
//     var tree = RadixTree.init(testing.allocator);
//     defer tree.deinit();
//     try tree.insert("foo", 1);
//     try tree.insert("f", 2);
//     try tree.remove("f");
//     const expected =
//         \\foo - 1
//         \\
//     ;
//     try expectTreeEqual(&tree, expected);
// }
