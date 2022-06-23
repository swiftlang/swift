// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-public-decls -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

public enum EmptyEnum {}
public enum SingleCaseEnum { case first }

// CHECK: namespace Enums {
// CHECK-NOT: EmptyEnum
// CHECK-NOT: SingleCaseEnum
// CHECK: } // namespace Enums
