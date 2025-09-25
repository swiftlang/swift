// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs/Foo.swiftmodule)
// RUN: touch %t/inputs/Foo.swiftmodule/i387.swiftmodule
// RUN: touch %t/inputs/Foo.swiftmodule/ppc65.swiftmodule

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %s -o %t/deps.json -I %t/inputs -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s

// CHECK: error: unable to resolve Swift module dependency to a compatible module: 'Foo'
// CHECK-DAG: note: found incompatible module '{{.*}}Foo.swiftmodule{{/|\\}}ppc65.swiftmodule': invalid architecture
// CHECK-DAG: note: found incompatible module '{{.*}}Foo.swiftmodule{{/|\\}}i387.swiftmodule': invalid architecture
import Foo
