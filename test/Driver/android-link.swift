// RUN: %empty-directory(%t)
// RUN: %swift_driver_plain --driver-mode=swiftc -target armv7-none-linux-androideabihf -emit-executable %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-ARMv7
// CHECK-ARMv7: clang{{(.exe)?"?}} -target armv7-unknown-linux-androideabi

// RUN: %swift_driver_plain --driver-mode=swiftc -target aarch64-mone-linux-androideabi -emit-executable %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-ARM64
// CHECK-ARM64: clang{{(.exe)?"?}} -target aarch64-unknown-linux-android

// RUN: %swift_driver_plain --driver-mode=swiftc -target x86_64-none-linux-androideabi -emit-executable %s -### 2>&1 | %FileCheck %s -check-prefix CHECK-X64
// CHECK-X64: clang{{(.exe)?"?}} -target x86_64-unknown-linux-android

