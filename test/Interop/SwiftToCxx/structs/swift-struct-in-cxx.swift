// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h)

// CHECK: namespace Structs {

// CHECK:      class StructWithIntField final {
// CHECK-NEXT: };
struct StructWithIntField {
  let field: Int
}

// Special name gets renamed in C++.
// CHECK: class register_ final {
struct register {
}

// CHECK: } // namespace Structs
