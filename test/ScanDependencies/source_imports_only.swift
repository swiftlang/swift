// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

// Ensure that round-trip serialization does not affect result
// RUN: %target-swift-frontend -scan-dependencies -test-dependency-scan-cache-serialization -module-cache-path %t/module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import
// RUN: %validate-json %t/deps.json | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: objc_interop

import C

// CHECK:          "sourceImportedDependencies": [
// CHECK-NEXT:      {
// CHECK-DAG:          "swift": "Swift"
// CHECK-DAG:          "swift": "SwiftOnoneSupport"
// CHECK-DAG:          "clang": "C"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]

// CHECK:         "swiftOverlayDependencies": [
// CHECK-NEXT:      {
// CHECK-NEXT:        "swift": "A"
// CHECK-NEXT:      }
// CHECK-NEXT:    ]
