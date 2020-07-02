// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/PCMacroRuntime.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -Xfrontend -debugger-support -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift 
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test
// XFAIL: *

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

import PlaygroundSupport

class X {
var foo: Int {
  get { return 1 }
  set { }
}
}
_ = X().foo

// CHECK: [16:1-16:12] pc before
// the reason this is XFAILed is because it returns [12:3-11:13].
// CHECK-NEXT: [12:3-12:19] pc before
// CHECK-NEXT: [12:3-12:19] pc after
// CHECK-NEXT: [12:9-12:17] pc before
// CHECK-NEXT: [12:9-12:17] pc after
// CHECK-NEXT: [16:1-16:12] pc after

