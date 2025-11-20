// RUN: %target-swift-frontend -emit-ir %s

func withPointerToElements<each T, E, R>(
    of tuple: borrowing (repeat each T),
    _ body: (UnsafeBufferPointer<E>) -> R
) -> R {
    for t in repeat (each T).self {
        if t != E.self {
            preconditionFailure()
        }
    }
    return withUnsafeBytes(of: tuple) { p in
        return body(p.assumingMemoryBound(to: E.self))
    }
}
