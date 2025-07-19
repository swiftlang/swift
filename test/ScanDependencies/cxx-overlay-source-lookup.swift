// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/deps)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -o %t/deps.json %t/client.swift -I %t/deps -cxx-interoperability-mode=default -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import
// RUN: cat %t/deps.json | %FileCheck %s

/// --------Main module
// CHECK-LABEL: "modulePath": "deps.swiftmodule",
// CHECK-NEXT: "sourceFiles": [
// CHECK-NEXT: cxx-overlay-source-lookup.swift
// CHECK-NEXT: ],
// CHECK: "directDependencies": [
// CHECK-DAG: "swift": "Swift"
// CHECK-DAG: "swift": "SwiftOnoneSupport"
// CHECK-DAG: "swift": "Cxx"
// CHECK-DAG: "swift": "CxxStdlib"
// CHECK-DAG: "clang": "CxxShim"
// CHECK-DAG: "clang": "Foo"
// CHECK: ],

//--- deps/bar.h
void bar(void);

//--- deps/foo.h
#include "bar.h"
void foo(void);

//--- deps/module.modulemap
module std_Bar [system] {
  header "bar.h"
  export *
}

module Foo {
  header "foo.h"
  export *
}

//--- client.swift
import Foo
