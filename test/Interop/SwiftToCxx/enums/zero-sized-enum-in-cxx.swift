// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -module-name Enums -clang-header-expose-decls=all-public -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

public enum EmptyEnum {}
public enum SingleCaseEnum { case first }

// CHECK: namespace Enums __attribute__((swift_private)) SWIFT_SYMBOL_MODULE("Enums") {
// CHECK-NOT: EmptyEnum final {
// CHECK-NOT: SingleCaseEnum final {
// CHECK: } // namespace Enums
