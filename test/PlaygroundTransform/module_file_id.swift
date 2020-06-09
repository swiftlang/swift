// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/test -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift
// RUN: %target-codesign %t/test
// RUN: %target-run %t/test | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/test2 -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift 
// RUN: %target-codesign %t/test2
// RUN: %target-run %t/test2 | %FileCheck %s
// REQUIRES: executable_test

import PlaygroundSupport

var a = 2
var b = 3
a + b 
// CHECK: [1:2] [{{.*}}] __builtin_log[a='2']
// CHECK-NEXT: [1:2] [{{.*}}] __builtin_log[b='3']
// CHECK-NEXT: [1:2] [{{.*}}] __builtin_log[='5']
