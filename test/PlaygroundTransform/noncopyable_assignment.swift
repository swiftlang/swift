// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
//
// REQUIRES: executable_test

import PlaygroundSupport

struct A: ~Copyable {}

var a: A
a = A()

struct B {}

var b: B
b = B()
// note: a should not be logged (at least until move-only types can be passed to the generic logging functions)
// CHECK: [{{.*}}] __builtin_log[b='B()']
