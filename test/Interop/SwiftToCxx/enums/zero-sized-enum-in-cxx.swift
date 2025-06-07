// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name Enums -clang-header-expose-decls=all-public -typecheck -verify -emit-clang-header-path %t/enums.h
// RUN: %FileCheck %s < %t/enums.h

public enum EmptyEnum {}
public enum SingleCaseEnum { case first }

// CHECK: namespace Enums SWIFT_PRIVATE_ATTR SWIFT_SYMBOL_MODULE("Enums") {
// CHECK-NOT: EmptyEnum final {
// CHECK-NOT: SingleCaseEnum final {
// CHECK: } // namespace Enums
