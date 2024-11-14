// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -o %t/test5a -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -o %t/test6a -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift

// -pc-macro -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -o %t/test5b -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -o %t/test6b -I=%t %t/PlaygroundSupport.o %S/Inputs/PlaygroundModuleAndFileIDs.swift %t/main.swift

// RUN: %target-codesign %t/test5a
// RUN: %target-codesign %t/test5b
// RUN: %target-codesign %t/test6a
// RUN: %target-codesign %t/test6b

// RUN: %target-run %t/test5a | %FileCheck %s
// RUN: %target-run %t/test5b | %FileCheck %s
// RUN: %target-run %t/test6a | %FileCheck %s
// RUN: %target-run %t/test6b | %FileCheck %s

// REQUIRES: executable_test

import PlaygroundSupport

var a = 2
var b = 3
a + b 
// CHECK: [1:2] [{{.*}}] __builtin_log[a='2']
// CHECK-NEXT: [1:2] [{{.*}}] __builtin_log[b='3']
// CHECK-NEXT: [1:2] [{{.*}}] __builtin_log[='5']
