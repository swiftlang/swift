// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift 
// RUN: %target-run %t/main | %FileCheck %s
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
