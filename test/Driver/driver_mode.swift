// RUN: %swift_driver_plain --driver-mode=swiftc -driver-print-actions %s 2>&1 | FileCheck -check-prefix=CHECK-SWIFTC %s
// RUN: %swift_driver_plain -driver-print-actions %s --driver-mode=swiftc 2>&1 | FileCheck -check-prefix=CHECK-SWIFT %s

// CHECK-SWIFTC: 0: input, "{{.*}}driver_mode.swift", swift
// CHECK-SWIFTC: 1: compile, {0}, object
// CHECK-SWIFTC: 2: link, {1}, image

// CHECK-SWIFT: 0: input, "{{.*}}driver_mode.swift", swift
// CHECK-SWIFT: 1: compile, {0}, none
