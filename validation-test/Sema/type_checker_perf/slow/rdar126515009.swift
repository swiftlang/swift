// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

protocol P {
    associatedtype A: FixedWidthInteger
    init(_: A)
}

func test<F: P, T: BinaryFloatingPoint>(_: F.Type, _: T.Type) {
    for elt: (x: T, y: Int, z: F.A) in [  // expected-error {{reasonable time}}
        (T.zero, 0, 0),
        (-T.zero, 0, 0),
        (T(0.0), 0, 0),
        (T(0.0), 0, 0),
        (-T(0.0), 0, 0 - 0),
        (-T(0.0), 0, 0 - 0),
        (T(1), Int(F.A.bitWidth / 1), F.A(1) << (F.A.bitWidth / 1))
    ] {}
}
