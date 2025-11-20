// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground -Xfrontend -disable-playground-transform) | %FileCheck %s -allow-empty
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground -Xfrontend -disable-playground-transform) | %FileCheck %s -allow-empty
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -disable-playground-transform) | %FileCheck %s -allow-empty
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -disable-playground-transform) | %FileCheck %s -allow-empty
//
// REQUIRES: executable_test

import PlaygroundSupport

var a = 2
var b = 3
a + b
// CHECK-NOT: __builtin_log
