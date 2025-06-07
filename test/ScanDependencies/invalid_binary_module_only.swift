// RUN: %empty-directory(%t)
// RUN: mkdir -p %t/clang-module-cache
// RUN: mkdir -p %t/moduleInputs

// RUN: echo "Not Really a module" >> %t/moduleInputs/FooBar.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/clang-module-cache %s -o %t/deps.json -I %t/moduleInputs -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s

import FooBar

// CHECK: warning: module file '{{.*}}{{/|\\}}moduleInputs{{/|\\}}FooBar.swiftmodule' is incompatible with this Swift compiler: malformed
// CHECK: error: Unable to find module dependency: 'FooBar'

