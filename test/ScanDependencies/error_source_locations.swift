// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// REQUIRES: objc_interop
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 2>&1 | %FileCheck %s

import P
import FooBar


// CHECK:      {{.*}}{{/|\\}}error_source_locations.swift:7:8: error: Unable to find module dependency: 'FooBar'
// CHECK-NEXT: 5 |
// CHECK-NEXT: 6 | import P
// CHECK-NEXT: 7 | import FooBar
// CHECK-NEXT:   |        |- error: Unable to find module dependency: 'FooBar'
// CHECK-NEXT:   |        `- note: a dependency of main module 'deps'
// CHECK-NEXT: 8 |
// CHECK-NEXT: 9 |

// CHECK:      {{.*}}{{/|\\}}Z.swiftinterface:3:8: error: Unable to find module dependency: 'missing_module'
// CHECK-NEXT: 1 | // swift-interface-format-version: 1.0
// CHECK-NEXT: 2 | // swift-module-flags: -module-name Z
// CHECK-NEXT: 3 | import missing_module
// CHECK-NEXT:   |        |- error: Unable to find module dependency: 'missing_module'
// CHECK-NEXT:   |        |- note: a dependency of Swift module 'Z': '{{.*}}{{/|\\}}Z.swiftinterface'
// CHECK-NEXT:   |        |- note: a dependency of Swift module 'Y': '{{.*}}{{/|\\}}Y.swiftinterface'
// CHECK-NEXT:   |        |- note: a dependency of Swift module 'P': '{{.*}}{{/|\\}}P.swiftinterface'
// CHECK-NEXT:   |        `- note: a dependency of main module 'deps'
// CHECK-NEXT: 4 | public func funcZ() { }
