// RUN: %swift_driver -print-target-triple -target arm64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS %s
// RUN: %target-swift-frontend -print-target-triple -target arm64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS %s

// RUN: %swift_driver -print-target-triple -target x86_64-unknown-linux | %FileCheck -check-prefix CHECK-LINUX %s
// RUN: %target-swift-frontend -print-target-triple -target x86_64-unknown-linux | %FileCheck -check-prefix CHECK-LINUX %s

// CHECK-IOS: arm64-apple-ios12.0
// CHECK-LINUX: x86_64-unknown-linux
