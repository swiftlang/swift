// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs/Foo.swiftmodule)
// RUN: touch %t/inputs/Foo.swiftmodule/i387.swiftmodule
// RUN: touch %t/inputs/Foo.swiftmodule/ppc65.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/inputs -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s

// CHECK-DAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}ppc65.swiftmodule' is incompatible with this Swift compiler: built for incompatible target
// CHECK-DAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}i387.swiftmodule' is incompatible with this Swift compiler: built for incompatible target
// CHECK: error: unable to resolve module dependency: 'Foo'

import Foo
