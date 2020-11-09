// RUN: %swift_driver -print-target-info -target arm64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS %s
// RUN: %target-swift-frontend -print-target-info -target arm64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS %s

// RUN: %swift_driver -print-target-info -target x86_64-unknown-linux | %FileCheck -check-prefix CHECK-LINUX %s
// RUN: %target-swift-frontend -print-target-info -target x86_64-unknown-linux | %FileCheck -check-prefix CHECK-LINUX %s

// RUN: %swift_driver -print-target-info -target x86_64-unknown-linux -static-executable | %FileCheck -check-prefix CHECK-LINUX-STATIC %s
// RUN: %swift_driver -print-target-info -target x86_64-unknown-linux -static-stdlib | %FileCheck -check-prefix CHECK-LINUX-STATIC %s
// RUN: %target-swift-frontend -print-target-info -target x86_64-unknown-linux -use-static-resource-dir | %FileCheck -check-prefix CHECK-LINUX-STATIC %s

// RUN: %swift_driver -print-target-info -target x86_64-apple-macosx10.15 -target-variant x86_64-apple-ios13-macabi | %FileCheck -check-prefix CHECK-ZIPPERED %s
// RUN: %target-swift-frontend -print-target-info -target x86_64-apple-macosx10.15 -target-variant x86_64-apple-ios13-macabi | %FileCheck -check-prefix CHECK-ZIPPERED %s

// RUN: %swift_driver -print-target-info -target x86_64-apple-ios12.0 | %FileCheck -check-prefix CHECK-IOS-SIM %s

// CHECK-IOS:   "compilerVersion": "{{.*}}Swift version

// CHECK-IOS:   "target": {
// CHECK-IOS:     "triple": "arm64-apple-ios12.0",
// CHECK-IOS:     "unversionedTriple": "arm64-apple-ios",
// CHECK-IOS:     "moduleTriple": "arm64-apple-ios",
// CHECK-IOS:     "swiftRuntimeCompatibilityVersion": "5.0",
// CHECK-IOS:     "compatibilityLibraries": [
// CHECK-IOS:       "libraryName": "swiftCompatibility50",
// CHECK-IOS:       "libraryName": "swiftCompatibility51",
// CHECK-IOS:       "libraryName": "swiftCompatibilityDynamicReplacements"
// CHECK-IOS:       "filter": "executable"
// CHECK-IOS:     ],
// CHECK-IOS:     "librariesRequireRPath": true
// CHECK-IOS:   }

// CHECK-IOS-NOT: "targetVariant":

// CHECK-IOS:   "paths": {
// CHECK-IOS:     "runtimeLibraryPaths": [
// CHECK-IOS:     ],
// CHECK-IOS:     "runtimeLibraryImportPaths": [
// CHECK-IOS:     ],
// CHECK-IOS:     "runtimeResourcePath": "{{.*}}lib{{(/|\\\\)}}swift"
// CHECK-IOS:   }


// CHECK-LINUX:   "compilerVersion": "{{.*}}Swift version

// CHECK-LINUX:   "target": {
// CHECK-LINUX:     "triple": "x86_64-unknown-linux",
// CHECK-LINUX:     "moduleTriple": "x86_64-unknown-linux",
// CHECK-LINUX:     "librariesRequireRPath": false
// CHECK-LINUX:   }

// CHECK-LINUX:   "runtimeResourcePath": "{{.*}}lib{{(/|\\\\)}}swift"

// CHECK-LINUX-NOT: "targetVariant":


// CHECK-LINUX-STATIC:   "compilerVersion": "{{.*}}Swift version

// CHECK-LINUX-STATIC:   "target": {
// CHECK-LINUX-STATIC:     "triple": "x86_64-unknown-linux",
// CHECK-LINUX-STATIC:     "moduleTriple": "x86_64-unknown-linux",
// CHECK-LINUX-STATIC:     "librariesRequireRPath": false
// CHECK-LINUX-STATIC:   }

// CHECK-LINUX-STATIC:   "runtimeResourcePath": "{{.*}}lib{{(/|\\\\)}}swift_static"

// CHECK-LINUX-STATIC-NOT: "targetVariant":


// CHECK-ZIPPERED: "target": {
// CHECK-ZIPPERED:   "triple": "x86_64-apple-macosx10.15"
// CHECK-ZIPPERED:   "unversionedTriple": "x86_64-apple-macosx"
// CHECK-ZIPPERED:   "moduleTriple": "x86_64-apple-macos"
// CHECK-ZIPPERED:   "swiftRuntimeCompatibilityVersion": "5.1"
// CHECK-ZIPPERED:   "librariesRequireRPath": false
// CHECK-ZIPPERED: }

// CHECK-ZIPPERED: "targetVariant": {
// CHECK-ZIPPERED:   "triple": "x86_64-apple-ios13-macabi"
// CHECK-ZIPPERED:   "unversionedTriple": "x86_64-apple-ios-macabi"
// CHECK-ZIPPERED:   "moduleTriple": "x86_64-apple-ios-macabi"
// CHECK-ZIPPERED:   "swiftRuntimeCompatibilityVersion": "5.1"
// CHECK-ZIPPERED:   "librariesRequireRPath": false
// CHECK-ZIPPERED: }

// CHECK-IOS-SIM:   "target": {
// CHECK-IOS-SIM:     "triple": "x86_64-apple-ios12.0-simulator",
// CHECK-IOS-SIM:     "unversionedTriple": "x86_64-apple-ios-simulator",
// CHECK-IOS-SIM:     "moduleTriple": "x86_64-apple-ios-simulator",
// CHECK-IOS-SIM:     "swiftRuntimeCompatibilityVersion": "5.0",
// CHECK-IOS-SIM:     "librariesRequireRPath": true
// CHECK-IOS-SIM:   }
