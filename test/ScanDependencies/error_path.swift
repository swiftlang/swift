// expected-error@-1 {{Unable to find module dependency: 'missing_module'}}
// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// REQUIRES: objc_interop

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %S/Inputs/CHeaders -I %S/Inputs/Swift -emit-dependencies -emit-dependencies-path %t/deps.d -import-objc-header %S/Inputs/CHeaders/Bridging.h -swift-version 4 2>&1 | %FileCheck %s

import P

// CHECK: <unknown>:0: error: Unable to find module dependency: 'missing_module'
// CHECK: <unknown>:0: note: a dependency of Swift module 'Z': '{{.*}}/Inputs/Swift/Z.swiftinterface'
// CHECK: <unknown>:0: note: a dependency of Swift module 'Y': '{{.*}}/Inputs/Swift/Y.swiftinterface'
// CHECK: <unknown>:0: note: a dependency of Swift module 'P': '{{.*}}/Inputs/Swift/P.swiftinterface'
// CHECK: <unknown>:0: note: a dependency of main module 'deps'
