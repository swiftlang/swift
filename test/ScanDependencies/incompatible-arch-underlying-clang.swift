// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/deps)
// RUN: %empty-directory(%t/module-cache)
// RUN: %empty-directory(%t/inputs/Foo.swiftmodule)
// RUN: touch %t/inputs/Foo.swiftmodule/i387.swiftmodule
// RUN: touch %t/inputs/Foo.swiftmodule/ppc65.swiftmodule
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/client.swift -o %t/deps.json -I %t/inputs -I %t/deps -diagnostic-style llvm -scanner-module-validation 2>&1 | %FileCheck %s

// CHECK-DAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}ppc65.swiftmodule' is incompatible with this Swift compiler: built for incompatible target
// CHECK-DAG: warning: module file '{{.*}}Foo.swiftmodule{{/|\\}}i387.swiftmodule' is incompatible with this Swift compiler: built for incompatible target
// CHECK-NOT: error

//--- deps/foo.h
void foo(void);

//--- deps/module.modulemap
module Foo {
  header "foo.h"
  export *
}

//--- client.swift
import Foo
