// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift

// Build PlaygroundSupport module
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift

// -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -playground -Xfrontend -disable-playground-transform -I=%t %t/PlaygroundSupport.o -o %t/main5a %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -playground -Xfrontend -disable-playground-transform -I=%t %t/PlaygroundSupport.o -o %t/main6a %t/main.swift

// -pc-macro -playground
// RUN: %target-build-swift -swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -disable-playground-transform -I=%t %t/PlaygroundSupport.o -o %t/main5b %t/main.swift
// RUN: %target-build-swift -swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -disable-playground-transform -I=%t %t/PlaygroundSupport.o -o %t/main6b %t/main.swift

// RUN: %target-codesign %t/main5a
// RUN: %target-codesign %t/main5b
// RUN: %target-codesign %t/main6a
// RUN: %target-codesign %t/main6b

// RUN: %target-run %t/main5a | %FileCheck %s -allow-empty
// RUN: %target-run %t/main5b | %FileCheck %s -allow-empty
// RUN: %target-run %t/main6a | %FileCheck %s -allow-empty
// RUN: %target-run %t/main6b | %FileCheck %s -allow-empty

// REQUIRES: executable_test

import PlaygroundSupport

var a = 2
var b = 3
a + b
// CHECK-NOT: __builtin_log
