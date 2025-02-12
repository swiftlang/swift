// RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/78371

// Note that the test has to use a standard operator because
// custom ones are not optimized.

struct Scalar : Equatable {
    init(_: UInt8) {}
    init?(_: Int) {}
}

func ==(_: Scalar, _: Scalar) -> Bool { }

extension Optional where Wrapped == Scalar {
    static func ==(_: Wrapped?, _: Wrapped?) -> Wrapped { }
}

// FIXME: There is currently no way to fix this because even
// if the new overload of `==` is scored the same as concrete
// one it would be skipped because it's generic _if_ operator
// is attempted before initializer.

func test(a: Scalar) {
    let result = a == Scalar(0x07FD)
    let _: Scalar = result // expected-error {{cannot convert value of type 'Bool' to specified type 'Scalar'}}
}
