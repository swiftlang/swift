// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -I %t/include

// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:SwiftShims > %t/shim.cmd
// RUN: %swift_frontend_plain @%t/shim.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:B > %t/B.cmd
// RUN: %swift_frontend_plain @%t/B.cmd
// RUN: %{python} %S/Inputs/BuildCommandExtractor.py %t/deps.json clang:A > %t/A.cmd
// RUN: %swift_frontend_plain @%t/A.cmd
// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:A modulePath > %t/A.path

// RUN: dwarfdump --debug-info @%t/A.path | %FileCheck %s

// CHECK: DW_AT_GNU_dwo_name
// CHECK-SAME: BUILD_DIR

//--- test.swift
import A

//--- include/a.h
#include "b.h"
struct A {
  int a;
};

//--- include/b.h
void b(void);

//--- include/module.modulemap
module A {
  header "a.h"
  export *
}

module B {
  header "b.h"
  export *
}
