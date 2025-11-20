// RUN: %target-swift-frontend -parse-stdlib -emit-ir %s -enable-experimental-feature Embedded -Xllvm -link-embedded-runtime=0

// REQUIRES: swift_in_compiler
// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

public struct UInt23 {
}

public protocol MyBinaryInteger {
}

extension UInt23: MyBinaryInteger {
}

protocol MyProto {
    static var my_static_var: UInt23 { get }
    static func foo()
}

struct MyStruct: MyProto {
    static let my_static_var = UInt23()
}

extension MyProto {
    public static func foo() {
        bar(Self.my_static_var)
    }
}

public func bar<T: MyBinaryInteger>(_ value: @autoclosure () -> T) {
}
