// RUN: rm -rf %t && mkdir -p %t
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main %S/Inputs/PlaygroundsRuntime.swift %S/Inputs/SilentPCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: We need to instrument mutations of objects that are accessed in
//        arbitrary ways, not just successive member references.
// XFAIL: *

struct S {
  var a = [Int]()
}
var s = [S()]
s[0].a.append(3)
// CHECK: [{{.*}}] $builtin_log[s='[main.S(a: [])]']
// CHECK-NEXT: [{{.*}}] $builtin_log[a='[3])]']
