// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground %S/Inputs/PlaygroundModuleAndFileIDs.swift) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground %S/Inputs/PlaygroundModuleAndFileIDs.swift) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground %S/Inputs/PlaygroundModuleAndFileIDs.swift) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground %S/Inputs/PlaygroundModuleAndFileIDs.swift) | %FileCheck %s
//
// REQUIRES: executable_test

import PlaygroundSupport

var a = 2
var b = 3
a + b 
// CHECK: [1:2] [{{.*}}] __builtin_log[a='2']
// CHECK-NEXT: [1:2] [{{.*}}] __builtin_log[b='3']
// CHECK-NEXT: [1:2] [{{.*}}] __builtin_log[='5']
