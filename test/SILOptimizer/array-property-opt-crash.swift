// RUN: %target-swift-frontend -parse-as-library -O %s -sil-verify-all -emit-sil -o /dev/null

// Check that ArrayPropertyOpt does not produce illegal SIL.
// https://github.com/swiftlang/swift/issues/89324

enum Op { case a, b }

class Operation: Equatable {
    let value1: Node
    let value2: Node
    let op: Op

    init(value1: Node, value2: Node, op: Op) {
        self.value1 = value1
        self.value2 = value2
        self.op = op
    }

    static func == (lhs: Operation, rhs: Operation) -> Bool {
        guard lhs.op == rhs.op else { return false }
        switch lhs.op {
        case .a:
            return (lhs.value1 == rhs.value1 && lhs.value2 == rhs.value2)
                || (lhs.value1 == rhs.value2 && lhs.value2 == rhs.value1)
        case .b:
            return lhs.value1 == rhs.value1 && lhs.value2 == rhs.value2
        }
    }
}

class Node: Equatable {
    var value: Int
    var origin: Operation?

    init(value: Int, origin: Operation? = nil) {
        self.value = value
        self.origin = origin
    }

    static func == (lhs: Node, rhs: Node) -> Bool {
        if let o1 = lhs.origin, let o2 = rhs.origin {
            return o1 == o2
        }
        return lhs.value == rhs.value
    }
}

class NodeArray: Hashable {
    var values: [Node]
    init(values: [Node]) { self.values = values }
    static func == (lhs: NodeArray, rhs: NodeArray) -> Bool {
        return lhs.values == rhs.values
    }
    func hash(into hasher: inout Hasher) {
        for v in values { hasher.combine(v.value) }
    }
}

