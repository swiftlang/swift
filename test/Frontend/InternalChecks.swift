// RUN: %swift -O -parse-stdlib -D INTERNAL_CHECKS_ENABLED %s -emit-sil | FileCheck %s --check-prefix=CHECKS
// RUN: %swift -O -parse-stdlib %s -emit-sil | FileCheck %s --check-prefix=NOCHECKS

import Swift

func test_internal_checks_config(x: Int, y: Int) -> Int {
#if INTERNAL_CHECKS_ENABLED
  println("internal check emitted")
#endif
  return x + y
}

// CHECKS-LABEL: _TF14InternalChecks27test_internal_checks_configFTSiSi_Si
// CHECKS: "internal check emitted"

// NOCHECKS-LABEL: _TF14InternalChecks27test_internal_checks_configFTSiSi_Si
// NOCHECKS-NOT: "internal check emitted"
