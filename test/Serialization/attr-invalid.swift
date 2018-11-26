// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t/attr.swiftmodule %s -verify
// RUN: llvm-bcanalyzer -dump %t/attr.swiftmodule | %FileCheck -check-prefix=CHECK-NON-RESILIENT %s
// RUN: %target-swift-frontend -emit-module -o %t/attr_resilient.swiftmodule -enable-resilience -warnings-as-errors %s
// RUN: llvm-bcanalyzer -dump %t/attr_resilient.swiftmodule | %FileCheck -check-prefix=CHECK-RESILIENT %s

// These two should be checking for the same thing.
// CHECK-RESILIENT: Frozen_DECL_ATTR
// CHECK-NON-RESILIENT-NOT: Frozen_DECL_ATTR

@_frozen // expected-warning {{@_frozen has no effect without -enable-resilience}}
public enum SomeEnum {
  case x
}
