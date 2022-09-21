// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Structs -clang-header-expose-public-decls -emit-clang-header-path %t/structs.h
// RUN: %FileCheck %s < %t/structs.h

// CHECK: namespace Structs {

// CHECK-NOT: class ZeroSizedStruct final {

public struct ZeroSizedStruct {}

// CHECK: } // namespace Structs
