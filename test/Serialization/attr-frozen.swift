// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/attr.swiftmodule %s -verify -warnings-as-errors
// RUN: llvm-bcanalyzer -dump %t/attr.swiftmodule | %FileCheck -check-prefix=CHECK-NON-RESILIENT %s
// RUN: %target-swift-frontend -emit-module -o %t/attr_resilient.swiftmodule -enable-library-evolution -warnings-as-errors %s
// RUN: llvm-bcanalyzer -dump %t/attr_resilient.swiftmodule | %FileCheck -check-prefix=CHECK-RESILIENT %s

// These two should be checking for the same thing.
// CHECK-RESILIENT: Frozen_DECL_ATTR
// CHECK-NON-RESILIENT: Frozen_DECL_ATTR

@frozen // expected-no-warning
public enum SomeEnum {
  case x
}
