// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PlatformAgnostic -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PlatformAgnostic -I %t -pretty-print -output-dir %t

// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-FS
// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-FP
// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-SO
// RUN: %FileCheck %s --input-file %t/PlatformAgnostic.symbols.json --check-prefix CHECK-FT

// CHECK-FS: FutureSwift
// CHECK-FS:      "availability": [
// CHECK-FS-NEXT:        {
// CHECK-FS-NEXT:          "domain": "Swift",
// CHECK-FS-NEXT:          "introduced": {
// CHECK-FS-NEXT:            "major": 99
// CHECK-FS-NEXT:          }
// CHECK-FS-NEXT:        }
// CHECK-FS-NEXT:      ]
@available(swift 99)
public struct FutureSwift {}

// CHECK-FP: FuturePackage
// CHECK-FP:      "availability": [
// CHECK-FP-NEXT:        {
// CHECK-FP-NEXT:          "domain": "SwiftPM",
// CHECK-FP-NEXT:          "introduced": {
// CHECK-FP-NEXT:            "major": 99
// CHECK-FP-NEXT:          }
// CHECK-FP-NEXT:        }
// CHECK-FP-NEXT:      ]
@available(_PackageDescription 99)
public struct FuturePackage {}

// CHECK-SO: SwiftObsolete
// CHECK-SO:      "availability": [
// CHECK-SO-NEXT:        {
// CHECK-SO-NEXT:          "domain": "Swift",
// CHECK-SO-NEXT:          "obsoleted": {
// CHECK-SO-NEXT:            "major": 1,
// CHECK-SO-NEXT:            "minor": 0
// CHECK-SO-NEXT:          }
// CHECK-SO-NEXT:        }
// CHECK-SO-NEXT:      ]
@available(swift, obsoleted: 1.0)
public struct SwiftObsolete {}

// CHECK-FT: FutureToolchain
// CHECK-FT:      "availability": [
// CHECK-FT-NEXT:        {
// CHECK-FT-NEXT:          "domain": "SwiftToolchain",
// CHECK-FT-NEXT:          "introduced": {
// CHECK-FT-NEXT:            "major": 99
// CHECK-FT-NEXT:          }
// CHECK-FT-NEXT:        }
// CHECK-FT-NEXT:      ]
@available(_SwiftToolchain 99)
public struct FutureToolchain {}
