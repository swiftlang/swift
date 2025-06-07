// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// CHECK: namespace Structs SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Structs") {

// CHECK-NOT: class SWIFT_SYMBOL({{.*}}) ZeroSizedStruct final {

public struct ZeroSizedStruct {}

public struct ZeroSizedStruct2 {
    var property: ZeroSizedStruct
    var void: Void
    var bar: ()
    public init() {
        property = .init()
    }
}

public enum ZeroSizedEnum {
}

public enum ZeroSizedEnum2 {
    case foo
}

public enum ZeroSizedEnum3 {
    case foo(ZeroSizedStruct, ZeroSizedEnum, ZeroSizedStruct2)
}

public func f() -> ZeroSizedStruct {
    ZeroSizedStruct()
}

public func g(x: ZeroSizedStruct) {
}

// CHECK: class ZeroSizedEnum { } SWIFT_UNAVAILABLE_MSG("'ZeroSizedEnum' is a zero sized value type, it cannot be exposed to C++ yet");

// CHECK: class ZeroSizedEnum2 { } SWIFT_UNAVAILABLE_MSG("'ZeroSizedEnum2' is a zero sized value type, it cannot be exposed to C++ yet");

// CHECK: class ZeroSizedEnum3 { } SWIFT_UNAVAILABLE_MSG("'ZeroSizedEnum3' is a zero sized value type, it cannot be exposed to C++ yet");

// CHECK: class ZeroSizedStruct { } SWIFT_UNAVAILABLE_MSG("'ZeroSizedStruct' is a zero sized value type, it cannot be exposed to C++ yet");

// CHECK: class ZeroSizedStruct2 { } SWIFT_UNAVAILABLE_MSG("'ZeroSizedStruct2' is a zero sized value type, it cannot be exposed to C++ yet");

// CHECK: // Unavailable in C++: Swift global function 'f()'.

// CHECK: // Unavailable in C++: Swift global function 'g(x:)'.

// CHECK: } // namespace Structs
