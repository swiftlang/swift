// RUN: %swift_frontend_plain -target arm64-apple-macos11 -enable-experimental-feature Embedded -print-target-info | %FileCheck -check-prefix CHECK-MACOS-EMBEDDED %s
// RUN: %swift_frontend_plain -target arm64-apple-macos11 -enable-experimental-feature Embedded -print-target-info -min-valid-pointer-value=0x2000 | %FileCheck -check-prefix CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE %s

// REQUIRES: swift_feature_Embedded

// CHECK-MACOS-EMBEDDED: "target": {
// CHECK-MACOS-EMBEDDED:   "triple": "arm64-apple-macos11",
// CHECK-MACOS-EMBEDDED:   "moduleTriple": "arm64-apple-macos",
// CHECK-MACOS-EMBEDDED:   "leastValidPointerValue": 4096,
// CHECK-MACOS-EMBEDDED:   "librariesRequireRPath": true
// CHECK-MACOS-EMBEDDED: }

// CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE: "target": {
// CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE:   "triple": "arm64-apple-macos11",
// CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE:   "moduleTriple": "arm64-apple-macos",
// CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE:   "leastValidPointerValue": 8192,
// CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE:   "librariesRequireRPath": true
// CHECK-MACOS-EMBEDDED-WITH-MIN-VALID-POINTER-VALUE: }
