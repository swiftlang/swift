// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s -allow-empty
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -disable-playground-transform -o %t/main2 %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s -allow-empty
// REQUIRES: executable_test

var a = 2
var b = 3
a + b
// CHECK-NOT: __builtin_log
