// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// RUN: %check-interop-cxx-header-in-clang(%t/structs.h -Wno-unused-private-field)

// CHECK: namespace Structs {

// CHECK:      class StructWithIntField final {
// CHECK-NEXT: private:
// CHECK-NEXT:   alignas(8) char _storage[8];
// CHECK-NEXT: };
public struct StructWithIntField {
  let field: Int64
}

// Special name gets renamed in C++.
// CHECK: class register_ final {
// CHECK: alignas(8) char _storage[16];
// CHECK-NEXT: };
public struct register {
  let field1: Int64
  let field2: Int64
}

// CHECK: } // namespace Structs
