// RUN: %target-swiftc_driver -warn-swift3-objc-inference-minimal -driver-print-jobs %s 2>&1 | %FileCheck -check-prefix=CHECK-MINIMAL %s
// RUN: %target-swiftc_driver -warn-swift3-objc-inference-complete -warn-swift3-objc-inference-minimal -driver-print-jobs %s 2>&1 | %FileCheck -check-prefix=CHECK-MINIMAL %s
// RUN: %target-swiftc_driver -warn-swift3-objc-inference-complete -driver-print-jobs %s 2>&1 | %FileCheck -check-prefix=CHECK-COMPLETE %s
// RUN: %target-swiftc_driver -warn-swift3-objc-inference-minimal -warn-swift3-objc-inference-complete -driver-print-jobs %s 2>&1 | %FileCheck -check-prefix=CHECK-COMPLETE %s

// REQUIRES: objc_interop

// CHECK-MINIMAL: -warn-swift3-objc-inference-minimal
// CHECK-COMPLETE: -warn-swift3-objc-inference-complete
