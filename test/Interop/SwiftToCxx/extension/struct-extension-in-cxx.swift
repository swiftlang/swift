// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Structs -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-private-field -Wno-unused-function -DSWIFT_CXX_INTEROP_HIDE_STL_OVERLAY)


public struct TypeAfterArray {
    var x: Int16
}

public struct Array {
    public var x: Int
}

extension Array {
    public var val: Structs.TypeAfterArray {
        return TypeAfterArray(x: 42)
    }
}

// CHECK: class SWIFT_SYMBOL("s:7Structs14TypeAfterArrayV") TypeAfterArray;
// CHECK: class SWIFT_SYMBOL("s:7Structs5ArrayV") Array final {
// CHECK:        swift::Int getX() const SWIFT_SYMBOL("s:7Structs5ArrayV1xSivp");
// CHECK-NEXT:   SWIFT_INLINE_THUNK void setX(swift::Int value) SWIFT_SYMBOL("s:7Structs5ArrayV1xSivp");
// CHECK-NEXT:   TypeAfterArray getVal() const SWIFT_SYMBOL("s:7Structs5ArrayV3valAA09TypeAfterB0Vvp");
