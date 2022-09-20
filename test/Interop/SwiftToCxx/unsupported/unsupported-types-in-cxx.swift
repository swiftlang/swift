// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Functions -clang-header-expose-public-decls -emit-clang-header-path %t/functions.h
// RUN: %FileCheck %s < %t/functions.h

// RUN: %check-interop-cxx-header-in-clang(%t/functions.h)

// CHECK:     takeFloat
// CHECK-NOT: takes

public func takeFloat(_ x: Float) {}

public func takesTuple(_ x: (Float, Float)) {}

public func takesVoid(_ x: ()) {}
