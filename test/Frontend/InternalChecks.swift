// RUN: %target-swift-frontend -O -parse-stdlib -D INTERNAL_CHECKS_ENABLED -primary-file %s -emit-sil | FileCheck %s --check-prefix=CHECKS
// RUN: %target-swift-frontend -O -parse-stdlib -primary-file %s -emit-sil | FileCheck %s --check-prefix=NOCHECKS

import Swift

func test_internal_checks_config(x: Int, _ y: Int) -> Int {
#if INTERNAL_CHECKS_ENABLED
  print("internal check emitted")
#endif
  return x + y
}

// CHECKS-LABEL: _TF14InternalChecks27test_internal_checks_configFTSiSi_Si
// CHECKS: "internal check emitted"

// NOCHECKS-LABEL: _TF14InternalChecks27test_internal_checks_configFTSiSi_Si
// NOCHECKS-NOT: "internal check emitted"
