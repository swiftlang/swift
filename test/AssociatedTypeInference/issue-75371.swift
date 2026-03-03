// RUN: %target-swift-frontend -emit-ir %s

// https://github.com/swiftlang/swift/issues/75371

public protocol MyProtocol {
    associatedtype Value = Self

    static var originalValue: Value { get }
    static var copyValue: Value { get }
}

public struct MyStruct: MyProtocol {}

extension MyStruct {
    public static let originalValue = Self()
    public static let copyValue = originalValue
}

