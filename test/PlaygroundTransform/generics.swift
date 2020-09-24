// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -whole-module-optimization -module-name PlaygroundSupport -emit-module-path %t/PlaygroundSupport.swiftmodule -parse-as-library -c -o %t/PlaygroundSupport.o %S/Inputs/SilentPCMacroRuntime.swift %S/Inputs/PlaygroundsRuntime.swift
// RUN: %target-build-swift -Xfrontend -playground -o %t/main -I=%t %t/PlaygroundSupport.o %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 -I=%t %t/PlaygroundSupport.o %t/main.swift 
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test

import PlaygroundSupport

func id<T>(_ t: T) -> T {
  return t
}

for i in 0..<3 {
  _ = id(i)
}

// CHECK:      __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log[='0']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log[='0']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log[='1']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log[='1']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log_scope_entry
// CHECK-NEXT: __builtin_log[='2']
// CHECK-NEXT: __builtin_log_scope_exit
// CHECK-NEXT: __builtin_log[='2']
// CHECK-NEXT: __builtin_log_scope_exit
