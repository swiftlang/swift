// RUN: %swift_driver -print-target-info -target arm64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS %s
// RUN: %target-swift-frontend -print-target-info -target arm64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS %s

// RUN: %swift_driver -print-target-info -target x86_64-unknown-linux | %FileCheck -check-prefix CHECK-LINUX %s
// RUN: %target-swift-frontend -print-target-info -target x86_64-unknown-linux | %FileCheck -check-prefix CHECK-LINUX %s

// CHECK-IOS:   "target": {
// CHECK-IOS:     "triple": "arm64-apple-ios12.0",
// CHECK-IOS:     "unversionedTriple": "arm64-apple-ios",
// CHECK-IOS:     "moduleTriple": "arm64-apple-ios",
// CHECK-IOS:     "swiftRuntimeCompatibilityVersion": "5.0",
// CHECK-IOS:     "librariesRequireRPath": true
// CHECK-IOS:   }

// CHECK-IOS:   "paths": {
// CHECK-IOS:     "runtimeLibraryPaths": [
// CHECK-IOS:     ],
// CHECK-IOS:     "runtimeLibraryImportPaths": [
// CHECK-IOS:     ],
// CHECK-IOS:     "runtimeResourcePath": "{{.*}}lib{{(/|\\\\)}}swift"
// CHECK-IOS:   }


// CHECK-LINUX:   "target": {
// CHECK-LINUX:     "triple": "x86_64-unknown-linux",
// CHECK-LINUX:     "moduleTriple": "x86_64-unknown-linux",
// CHECK-LINUX:     "librariesRequireRPath": false
// CHECK-LINUX:   }
