// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name CdeclFunctions -emit-cxx-header-path %t/empty.h
// RUN: %FileCheck %s < %t/empty.h

// RUN: %check-cxx-header-in-clang -std=c++14 %t/empty.h
// RUN: %check-cxx-header-in-clang -std=c++17 %t/empty.h

// CHECK-LABEL: namespace CdeclFunctions {

// CHECK: namespace _impl {
// CHECK: extern "C" int cfuncPassTwo(int x, int y) noexcept;
// CHECK: }

@_cdecl("cfuncPassTwo")
public func differentCDeclName(x: CInt, y: CInt) -> CInt { return x + y }

// CHECK: inline int differentCDeclName(int x, int y) noexcept SWIFT_WARN_UNUSED_RESULT {
// CHECK: return _impl::cfuncPassTwo(x, y);
// CHECK: }
