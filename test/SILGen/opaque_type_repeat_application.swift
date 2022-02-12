// RUN: %target-swift-emit-sil -disable-availability-checking -verify %s

// rdar://88120984

protocol P {}

private struct Wrapped<T: P>: P { var x: T }

extension P {
    func wrapped() -> some P {
        return Wrapped(x: self)
    }
}

class CWrap<T: P> { init(x: T) {} }

func foo<T: P>(x: T) {
    let y = x.wrapped().wrapped().wrapped().wrapped()

    _ = CWrap(x: y)
}
