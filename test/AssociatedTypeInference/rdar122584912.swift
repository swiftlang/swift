// RUN: %target-typecheck-verify-swift

public struct DefaultA {}

public protocol P {
    associatedtype A = DefaultA
    associatedtype B

    static func foo(_: A, _: B)
    static func bar(_: B)
}

extension P {
    public static func foo(_: A, _: B) where B == Void {}
}

// The only way C can conform to P is with 'A := DefaultA' and 'B := String'.
// We skip C.foo() because it contains 'A', so we must not infer 'B := Void'
// from P.foo() above.
class C: P {
    public static func foo(_: A, _: String) {}
    public static func bar(_: String) {}
}

let x: DefaultA.Type = C.A.self
let y: String.Type = C.B.self