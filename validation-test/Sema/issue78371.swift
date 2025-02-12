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

func test(a: Scalar) {
    let result = a == Scalar(0x07FD)
    let _: Scalar = result // Ok
}
