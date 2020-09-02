// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4

// Check the contents of the JSON output
// RUN: %FileCheck %s < %t/deps.json

// REQUIRES: executable_test
// REQUIRES: objc_interop

import EWrapper
import SubEWrapper

// CHECK:  "directDependencies": [
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "EWrapper"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "SubEWrapper"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "Swift"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "SwiftOnoneSupport"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "_cross_import_E"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT:   "swift": "F"
// CHECK-NEXT: }
// CHECK-NEXT: ],
