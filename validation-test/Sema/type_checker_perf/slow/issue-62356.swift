// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// This also fails with the default limit.

// https://github.com/swiftlang/swift/issues/62356

func sort4<Value,
           Property1: Comparable,
           Property2: Comparable,
           Property3: Comparable,
           Property4: Comparable>(
    _ a: Value,
    _ b: Value,
    prop1: KeyPath<Value, Property1>,
    comparator1: (Property1, Property1) -> Bool,
    prop2: KeyPath<Value, Property2>,
    comparator2: (Property2, Property2) -> Bool,
    prop3: KeyPath<Value, Property3>,
    comparator3: (Property3, Property3) -> Bool,
    prop4: KeyPath<Value, Property4>,
    comparator4: (Property4, Property4) -> Bool
) -> Bool { return false }

struct Foo {
    static func sort(_ lhs: Self, _ rhs: Self) -> Bool {
        // expected-error@+1 {{reasonable}}
        sort4(lhs, rhs,
              prop1: \.a, comparator1: <,
              prop2: \.b, comparator2: <,
              prop3: \.c, comparator3: <,
              prop4: \.d, comparator4: <)
    }

    var a: Int
    var b: Int
    var c: Int
    var d: Int
}

