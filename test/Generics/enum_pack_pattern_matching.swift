// RUN: %target-typecheck-verify-swift

enum Wrapper<each T> {
    case values(repeat each T)
    case empty
}

// Basic pattern matching in generic context
func matchWrapper<each T>(_ w: Wrapper<repeat each T>) {
    switch w {
    case let .values(v):
        // v should have type `repeat each T`
        let _: (repeat each T) = (repeat each v)
    case .empty:
        break
    }
}

// Using matched values
func useMatchedValues<each T>(_ w: Wrapper<repeat each T>) -> (repeat each T)? {
    switch w {
    case let .values(v):
        return (repeat each v)
    case .empty:
        return nil
    }
}
