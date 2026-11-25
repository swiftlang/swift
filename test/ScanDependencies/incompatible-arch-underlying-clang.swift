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

// The Swift overlay of a Clang dependency is found on a later search path,
// so the directory with only other-target variants is not diagnosed.
// RUN: %empty-directory(%t/valid/Foo.swiftmodule)
// RUN: %target-swift-frontend -emit-module %t/Foo.swift -module-name Foo -emit-module-path %t/valid/Foo.swiftmodule/%target-swiftmodule-name
// RUN: %target-swift-frontend -scan-dependencies -module-cache-path %t/module-cache %t/clientBar.swift -o %t/deps-overlay.json -I %t/inputs -I %t/valid -I %t/deps -diagnostic-style llvm -scanner-module-validation -warnings-as-errors 2>&1 | %FileCheck %s -check-prefix=OVERLAY --allow-empty
// RUN: %FileCheck %s -check-prefix=OVERLAY-DEPS < %t/deps-overlay.json

// OVERLAY-NOT: warning:
// OVERLAY-NOT: error:
// OVERLAY-DEPS: "compiledModulePath": "{{.*}}valid{{.*}}Foo.swiftmodule{{.*}}.swiftmodule"

//--- deps/foo.h
void foo(void);

//--- deps/bar.h
#include "foo.h"

//--- deps/module.modulemap
module Foo {
  header "foo.h"
  export *
}
module Bar {
  header "bar.h"
  export *
}

//--- client.swift
import Foo

//--- clientBar.swift
import Bar

//--- Foo.swift
public func fooOverlay() {}
