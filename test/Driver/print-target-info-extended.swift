// RUN: %swift_driver_plain -target aarch64-unknown-windows-msvc -print-target-info | %FileCheck -check-prefix CHECK-windows -check-prefix CHECK-aarch64 %s
// RUN: %target-swift-frontend -print-target-info | %FileCheck -check-prefix CHECK-%target-sdk-name -check-prefix CHECK-%target-arch %s

// CHECK-android: "platform": "android",
// CHECK-appletvos: "platform": "appletvos",
// CHECK-cygwin: "platform": "cgywin",
// CHECK-embedded: "platform": "embedded",
// CHECK-freebsd: "platform": "freebsd",
// CHECK-iphoneos: "platform": "iphoneos",
// CHECK-linux: "platform": "linux",
// CHECK-macosx: "platform": "macosx",
// CHECK-mingw: "platform": "mingw",
// CHECK-openbsd: "platform": "openbsd",
// CHECK-wasi: "platform": "wasi",
// CHECK-watchos: "platform": "watchos",
// CHECK-windows: "platform": "windows",
// CHECK-xros: "platform": "xros",

// CHECK-aarch64: "arch": "aarch64"
// CHECK-arm64: "arch": "arm64"
// CHECK-arm64e: "arch": "arm64e"
// CHECK-arm64_32: "arch": "arm64_32"
// CHECK-i386: "arch": "i386"
// CHECK-x86_64: "arch": "x86_64"
