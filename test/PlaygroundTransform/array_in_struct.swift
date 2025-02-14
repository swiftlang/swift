// -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -playground) | %FileCheck %s
//
// -pc-macro -playground
// RUN: %target-playground-build-run-swift(-swift-version 5 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
// RUN: %target-playground-build-run-swift(-swift-version 6 -Xfrontend -pc-macro -Xfrontend -playground) | %FileCheck %s
//
// REQUIRES: executable_test

// FIXME: We need to instrument mutations of objects that are accessed in
//        arbitrary ways, not just successive member references.
// XFAIL: *

import PlaygroundSupport

struct S {
  var a = [Int]()
}
var s = [S()]
s[0].a.append(3)
// CHECK: [{{.*}}] __builtin_log[s='[main.S(a: [])]']
// CHECK-NEXT: [{{.*}}] __builtin_log[a='[3])]']
