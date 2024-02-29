// Test that after compiling a module to a path containing a symlink we still
// get the same scanner output.

// REQUIRES: OS=macosx

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir %t/module-outputs
// RUN: ln -s module-outputs %t/symlink

// RUN: %target-swift-frontend -scan-dependencies %t/a.swift -o %t/deps.json -module-name A -emit-dependencies -emit-dependencies-path %t/deps.d -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/symlink -verify -cache-compile-job -cas-path %t/cas
// Check the contents of the JSON output
// RUN: %validate-json %t/deps.json | %FileCheck %s

// CHECK:      "-fmodule-file=C=[[PCM_PATH:.*symlink.*C-.*.pcm]]"
// CHECK:      "-fmodule-file-cache-key"
// CHECK-NEXT: "-Xcc"
// CHECK-NEXT: "[[PCM_PATH]]"
// CHECK-NEXT: "-Xcc"
// CHECK-NEXT: "llvmcas://

// Emit one of the modules, which will be in the symlinked path.
// RUN: %{python} %S/../CAS/Inputs/BuildCommandExtractor.py %t/deps.json clang:C > %t/C.cmd
// RUN: %swift_frontend_plain @%t/C.cmd

// RUN: %target-swift-frontend -scan-dependencies %t/a.swift -o %t/deps2.json -module-name A -emit-dependencies -emit-dependencies-path %t/deps.d -disable-implicit-concurrency-module-import -disable-implicit-string-processing-module-import -I %t -module-cache-path %t/symlink -verify -cache-compile-job -cas-path %t/cas
// RUN: diff -u %t/deps.json %t/deps2.json

//--- module.modulemap
module B { header "B.h" }
module C { header "C.h" }

//--- B.h
#include "C.h"

//--- C.h

//--- a.swift
import B
