// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-run %t/main | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 8)
for i in 0..<3 /* comments won't confuse pc macro! */ {
  i // check it is hitting the loop conditional between each iteration.
}
// CHECK: [8:1-8:15] pc before
// CHECK-NEXT: [8:1-8:15] pc after
// CHECK-NEXT: [9:3-9:4] pc before
// CHECK-NEXT: [9:3-9:4] pc after
// CHECK-NEXT: [8:1-8:15] pc before
// CHECK-NEXT: [8:1-8:15] pc after
// CHECK-NEXT: [9:3-9:4] pc before
// CHECK-NEXT: [9:3-9:4] pc after
// CHECK-NEXT: [8:1-8:15] pc before
// CHECK-NEXT: [8:1-8:15] pc after
// CHECK-NEXT: [9:3-9:4] pc before
// CHECK-NEXT: [9:3-9:4] pc after

for i in 0..<3 { // comments here shouldn't confuse it
  i
  continue // checking it includes the continue
}
// CHECK-NEXT: [24:1-24:15] pc before
// CHECK-NEXT: [24:1-24:15] pc after
// CHECK-NEXT: [25:3-25:4] pc before
// CHECK-NEXT: [25:3-25:4] pc after
// CHECK-NEXT: [26:3-26:11] pc before
// CHECK-NEXT: [26:3-26:11] pc after
// CHECK-NEXT: [24:1-24:15] pc before
// CHECK-NEXT: [24:1-24:15] pc after
// CHECK-NEXT: [25:3-25:4] pc before
// CHECK-NEXT: [25:3-25:4] pc after
// CHECK-NEXT: [26:3-26:11] pc before
// CHECK-NEXT: [26:3-26:11] pc after
// CHECK-NEXT: [24:1-24:15] pc before
// CHECK-NEXT: [24:1-24:15] pc after
// CHECK-NEXT: [25:3-25:4] pc before
// CHECK-NEXT: [25:3-25:4] pc after
// CHECK-NEXT: [26:3-26:11] pc before
// CHECK-NEXT: [26:3-26:11] pc after

for i in 0..<3 {
  i
  break // check it only happens once and includes the break
}
// CHECK-NEXT: [47:1-47:15] pc before
// CHECK-NEXT: [47:1-47:15] pc after
// CHECK-NEXT: [48:3-48:4] pc before
// CHECK-NEXT: [48:3-48:4] pc after
// CHECK-NEXT: [49:3-49:8] pc before
// CHECK-NEXT: [49:3-49:8] pc after
1
// check the file is finished
// CHECK-NEXT: [57:1-57:2] pc before
// CHECK-NEXT: [57:1-57:2] pc after
