// REQUIRES: OS=macosx
// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import \
// RUN:   %t/test.swift -o %t/deps.json -cache-compile-job -cas-path %t/cas -I %t/include

// RUN: %{python} %S/../../utils/swift-build-modules.py --cas %t/cas %swift_frontend_plain %t/deps.json -o %t/MyApp.cmd

// RUN: %{python} %S/Inputs/SwiftDepsExtractor.py %t/deps.json clang:A modulePath > %t/A.path
// RUN: dwarfdump --debug-info @%t/A.path | %FileCheck %s

// CHECK: DW_AT_GNU_dwo_name ("llvmcas://{{.*}}")

// RUN: %target-swift-frontend-plain -emit-module-path %t/Test.swiftmodule  \
// RUN:   -emit-module-interface-path %t/Test.swiftinterface \
// RUN:   -c -o %t/test.o -g \
// RUN:   -cache-compile-job -cas-path %t/cas \
// RUN:   -swift-version 5 -enable-cross-import-overlays \
// RUN:   -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -parse-stdlib \
// RUN:   -module-name Test \
// RUN:   %t/test.swift @%t/MyApp.cmd

// RUN: dwarfdump --debug-info %t/test.o | %FileCheck %s

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
