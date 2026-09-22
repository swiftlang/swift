// RUN: %target-typecheck-verify-swift -solver-scope-threshold=200 -solver-enable-promote-supertypes -solver-enable-type-var-joins

protocol P {
    associatedtype A: FixedWidthInteger
    init(_: A)
}

func test<F: P, T: BinaryFloatingPoint>(_: F.Type, _: T.Type) {
    for elt: (x: T, y: Int, z: F.A) in [ // expected-warning {{immutable value 'elt' was never used; consider replacing with '_' or removing it}}
        (T.zero, 0, 0),
        (-T.zero, 0, 0),
        (T(0.0), 0, 0),
        (T(0.0), 0, 0),
        (-T(0.0), 0, 0 - 0),
        (-T(0.0), 0, 0 - 0),
        (T(1), Int(F.A.bitWidth / 1), F.A(1) << (F.A.bitWidth / 1))
    ] {}
}
