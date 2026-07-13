// REQUIRES: VENDOR=apple
// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -scan-dependencies -module-name Test -module-cache-path %t/clang-module-cache -O -module-load-mode prefer-serialized -disable-implicit-string-processing-module-import -disable-implicit-concurrency-module-import -scanner-module-validation %t/test.swift -o %t/deps.json -scanner-output-dir %t -scanner-debug-write-output -import-objc-header %t/Bridging.h -Xcc -O2 -target %target-cpu-apple-macosx10.14 -clang-target %target-cpu-apple-macosx10.14 -I %t

// RUN: %{python} %S/../CAS/Inputs/SwiftDepsExtractor.py %t/deps.json Test bridgingHeader | %FileCheck %s -DCPU=%target-cpu

// CHECK:       "commandLine": [
// CHECK-NEXT:    "-frontend",
// CHECK-NEXT:    "-emit-pch",
// CHECK-NEXT:    "-direct-clang-cc1-module-build",
// CHECK-NEXT:    "-O",
// CHECK-NEXT:    "-target",
// CHECK-NEXT:    "[[CPU]]-apple-macosx10.14",
// CHECK-NEXT:    "-clang-target",
// CHECK-NEXT:    "[[CPU]]-apple-macosx10.14",

// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:RequiresOptimize > %t/RequiresOptimize.cmd
// RUN: %swift_frontend_plain @%t/RequiresOptimize.cmd

//--- Bridging.h
#ifndef __OPTIMIZE__
#error "PCH emitted without __OPTIMIZE__ despite -O2"
#endif

//--- module.modulemap
module RequiresOptimize {
  header "RequiresOptimize.h"
  export *
}

//--- RequiresOptimize.h
#ifndef __OPTIMIZE__
#error "PCM emitted without __OPTIMIZE__ despite -O2"
#endif
void requiresOptimize(void);

//--- test.swift
import RequiresOptimize

