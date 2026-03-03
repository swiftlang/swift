// RUN: %target-swift-emit-silgen -target %target-swift-5.1-abi-triple -verify %s
// rdar://83378116

public protocol P {}
public protocol Q {}

struct SP<T: P>: P { init(t: T) {} }
struct SQ: Q {}

struct Tubb<C: Q, T, F> {
}

extension Tubb: P where T: P, F: P {
    init(c: C, t: () -> T, f: () -> F) {}
}

struct SP2: P {}

func bar() -> some P { return SP2() }

public struct Butt {
    public func foo() -> some P {
        let sp = SP(t: bar())
        return Tubb(c: SQ(), t: { sp }, f: { sp })
    }
}
