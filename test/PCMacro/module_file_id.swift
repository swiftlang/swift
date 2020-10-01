// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/PCMacroRuntime.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/test -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift 
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/test2 -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift 
// RUN: %target-codesign %t/test2
// RUN: %target-run %t/test2 | %FileCheck %s

// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

import PlaygroundSupport

#sourceLocation(file: "main.swift", line: 8)
var a = true
// CHECK: [1:2] [8:1-8:13] pc before
// CHECK-NEXT: [1:2] [8:1-8:13] pc after
