// RUN: %swift_driver_plain --driver-mode=swiftc -driver-print-actions %s 2>&1 | FileCheck -check-prefix=CHECK-SWIFTC-%target-object-format %s
// RUN: %swift_driver_plain -driver-print-actions %s --driver-mode=swiftc 2>&1 | FileCheck -check-prefix=CHECK-SWIFT %s

// CHECK-SWIFTC-macho: 0: input, "{{.*}}driver_mode.swift", swift
// CHECK-SWIFTC-macho: 1: compile, {0}, object
// CHECK-SWIFTC-macho: 2: link, {1}, image

// CHECK-SWIFTC-elf: 0: input, "{{.*}}driver_mode.swift", swift
// CHECK-SWIFTC-elf: 1: compile, {0}, object
// CHECK-SWIFTC-elf: 2: swift-autolink-extract, {1}, autolink
// CHECK-SWIFTC-elf: 3: link, {1, 2}, image

// CHECK-SWIFT: 0: input, "{{.*}}driver_mode.swift", swift
// CHECK-SWIFT: 1: compile, {0}, none
