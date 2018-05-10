// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -swift-version 5 -enable-resilience -enable-testing %S/Inputs/exhaustive_switch_testable_helper.swift -emit-module -o %t
// RUN: %target-swift-frontend -typecheck %s -swift-version 5 -I %t -DTESTABLE -verify
// RUN: %target-swift-frontend -typecheck %s -swift-version 5 -I %t 2>&1 | %FileCheck -check-prefix=VERIFY-NON-FROZEN %s

#if TESTABLE
@testable import exhaustive_switch_testable_helper
#else
import exhaustive_switch_testable_helper
#endif

func testFrozen(_ e: FrozenEnum) -> Int {
  switch e {
  case .a: return 1
  case .b, .c: return 2
  }
}

func testNonFrozen(_ e: NonFrozenEnum) -> Int {
  // VERIFY-NON-FROZEN: exhaustive_switch_testable.swift:[[@LINE+1]]:{{[0-9]+}}: warning: switch must be exhaustive
  switch e {
  case .a: return 1
  case .b, .c: return 2
  }
}
