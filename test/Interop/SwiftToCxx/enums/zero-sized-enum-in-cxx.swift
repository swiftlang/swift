// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-decls=all-public -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

public enum EmptyEnum {}
public enum SingleCaseEnum { case first }

// CHECK: namespace Enums __attribute__((swift_private)) {
// CHECK-NOT: class EmptyEnum final {
// CHECK-NOT: class SingleCaseEnum final {
// CHECK: } // namespace Enums
