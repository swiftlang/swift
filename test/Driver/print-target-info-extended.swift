// RUN: %swift_driver_plain -target aarch64-unknown-windows-msvc -print-target-info | %FileCheck -check-prefix CHECK-windows -check-prefix CHECK-aarch64 %s
// RUN: %target-swift-frontend -print-target-info | %FileCheck -check-prefix CHECK-%target-sdk-name -check-prefix CHECK-%target-arch %s

// CHECK-android: "platform": "android",
// CHECK-cygwin: "platform": "cgywin",
// CHECK-embedded: "platform": "embedded",
// CHECK-freebsd: "platform": "freebsd",
// CHECK-linux: "platform": "linux",
// CHECK-mingw: "platform": "mingw",
// CHECK-openbsd: "platform": "openbsd",
// CHECK-wasi: "platform": "wasi",
// CHECK-windows: "platform": "windows",

// CHECK-aarch64: "arch": "aarch64"
// CHECK-x86_64: "arch": "x86_64"
