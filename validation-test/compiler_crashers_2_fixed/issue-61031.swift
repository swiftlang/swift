// RUN: %target-typecheck-verify-swift

public struct Wrapped<Values>: Sequence where Values: Sequence {
    public var values: Values

    public func makeIterator() -> Values.Iterator {
        values.makeIterator()
    }
}

// expected-error@+1 {{reference to generic type 'Array' requires arguments in <...>}}
extension Wrapped where Values == Array {
    public init(repeating value: Element, count: Int) {
        values = Array(repeating: value, count: count)
    }
}
