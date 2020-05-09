// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -disable-playground-transform -I=%t %t/PlaygroundSupport.o -o %t/main %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s -allow-empty
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -disable-playground-transform -I=%t %t/PlaygroundSupport.o -o %t/main2 %t/main.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s -allow-empty
// REQUIRES: executable_test

import PlaygroundSupport

var a = 2
var b = 3
a + b
// CHECK-NOT: __builtin_log
