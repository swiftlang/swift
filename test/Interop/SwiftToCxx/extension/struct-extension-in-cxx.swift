// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-decls=all-public -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-private-field -Wno-unused-function)


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

// CHECK class TypeAfterArray;
// CHECK: class Array final {
// CHECK:        swift::Int getX() const;
// CHECK-NEXT:   inline void setX(swift::Int value);
// CHECK-NEXT:   TypeAfterArray getVal() const;
