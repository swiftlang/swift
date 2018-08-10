// RUN: %empty-directory(%t)
// RUN: cp %s %t/main.swift
// RUN: %target-build-swift -Xfrontend -pc-macro -o %t/main %S/Inputs/PCMacroRuntime.swift %t/main.swift
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s
// RUN: %target-build-swift -Xfrontend -pc-macro -Xfrontend -playground -o %t/main2 %S/Inputs/PCMacroRuntime.swift %t/main.swift %S/Inputs/SilentPlaygroundsRuntime.swift
// RUN: %target-codesign %t/main2
// RUN: %target-run %t/main2 | %FileCheck %s
// REQUIRES: executable_test

// FIXME: rdar://problem/30234450 PCMacro tests fail on linux in optimized mode
// UNSUPPORTED: OS=linux-gnu

#sourceLocation(file: "main.swift", line: 10)
func s(_ x: Optional<Int>) -> String {
  switch x {
    case .none:
      print("hello!")
      return "none"
    case .some(1):
      return "one"
    case let .some(x) where x == 2:
      return "two"
    default:
      return "many"
  }
}

s(.none)
s(.some(1))
s(.some(2))
s(.some(3))

// .none
// CHECK: [24:1-24:9] pc before
// CHECK-NEXT: [10:1-10:37] pc before
// CHECK-NEXT: [10:1-10:37] pc after
// CHECK-NEXT: [11:3-11:11] pc before
// CHECK-NEXT: [11:3-11:11] pc after
// CHECK-NEXT: [12:5-12:16] pc before
// CHECK-NEXT: [12:5-12:16] pc after
// CHECK-NEXT: [13:7-13:22] pc before
// next must come hello! since we don't want the contents of the switch evaluated before we simulate entering it. Otherwise it will look out of sync.
// CHECK-NEXT: hello!
// CHECK-NEXT: [13:7-13:22] pc after
// CHECK-NEXT: [14:7-14:20] pc before
// CHECK-NEXT: [14:7-14:20] pc after
// CHECK-NEXT: [24:1-24:9] pc after

// .some(1)
// CHECK-NEXT: [25:1-25:12] pc before
// CHECK-NEXT: [10:1-10:37] pc before
// CHECK-NEXT: [10:1-10:37] pc after
// CHECK-NEXT: [11:3-11:11] pc before
// CHECK-NEXT: [11:3-11:11] pc after
// CHECK-NEXT: [15:5-15:19] pc before
// CHECK-NEXT: [15:5-15:19] pc after
// CHECK-NEXT: [16:7-16:19] pc before
// CHECK-NEXT: [16:7-16:19] pc after
// CHECK-NEXT: [25:1-25:12] pc after

// .some(2)
// CHECK-NEXT: [26:1-26:12] pc before
// CHECK-NEXT: [10:1-10:37] pc before
// CHECK-NEXT: [10:1-10:37] pc after
// CHECK-NEXT: [11:3-11:11] pc before
// CHECK-NEXT: [11:3-11:11] pc after
// CHECK-NEXT: [17:5-17:36] pc before
// CHECK-NEXT: [17:5-17:36] pc after
// CHECK-NEXT: [18:7-18:19] pc before
// CHECK-NEXT: [18:7-18:19] pc after
// CHECK-NEXT: [26:1-26:12] pc after

// .some(3)
// CHECK-NEXT: [27:1-27:12] pc before
// CHECK-NEXT: [10:1-10:37] pc before
// CHECK-NEXT: [10:1-10:37] pc after
// CHECK-NEXT: [11:3-11:11] pc before
// CHECK-NEXT: [11:3-11:11] pc after
// CHECK-NEXT: [19:5-19:13] pc before
// CHECK-NEXT: [19:5-19:13] pc after
// CHECK-NEXT: [20:7-20:20] pc before
// CHECK-NEXT: [20:7-20:20] pc after
// CHECK-NEXT: [27:1-27:12] pc after
