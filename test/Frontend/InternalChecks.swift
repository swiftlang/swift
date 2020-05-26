// RUN: %target-swift-frontend -O -parse-stdlib -D INTERNAL_CHECKS_ENABLED -primary-file %s -emit-sil | %FileCheck %s --check-prefix=CHECKS
// RUN: %target-swift-frontend -O -parse-stdlib -primary-file %s -emit-sil | %FileCheck %s --check-prefix=NOCHECKS

import Swift

func test_internal_checks_config(_ x: Int, _ y: Int) -> Int {
#if INTERNAL_CHECKS_ENABLED
  print("internal check emitted")
#endif
  return x + y
}

// CHECKS-LABEL: $s14InternalChecks27test_internal_checks_configyS2i_SitF
// CHECKS: "internal check emitted"

// NOCHECKS-LABEL: $s14InternalChecks27test_internal_checks_configyS2i_SitF
// NOCHECKS-NOT: "internal check emitted"
