// RUN: %target-swift-emit-silgen -g -Xllvm -sil-print-debuginfo %s | %FileCheck %s

func nop1() {}
func nop2() {}

enum Binary {
  case On
  case Off
}

func isOn(_ b: Binary) -> Bool {
  return b == .On
}

// CHECK: [[LOC:loc "[^"]+"]]

// First, check that we don't assign fresh locations to each case statement,
// except for any relevant debug value instructions.
// CHECK-LABEL: sil hidden @$S16switch_debuginfo5test11iySi_tF
func test1(i: Int) {
  switch i {
           // CHECK-NOT: [[LOC]]:[[@LINE+1]]
  case 0:  // CHECK: debug_value {{.*}} : $Int, let, name "$match", [[LOC]]:[[@LINE]]
           // CHECK-NOT: [[LOC]]:[[@LINE-1]]
    nop1()

           // CHECK-NOT: [[LOC]]:[[@LINE+1]]
  case 1:  // CHECK: debug_value {{.*}} : $Int, let, name "$match", [[LOC]]:[[@LINE]]
           // CHECK-NOT: [[LOC]]:[[@LINE-1]]
    nop1()

  default: // CHECK-NOT: [[LOC]]:[[@LINE]]
    nop1()
  }
}

// Next, check that case statements and switch subjects have the same locations.
// CHECK-LABEL: sil hidden @$S16switch_debuginfo5test21sySS_tF
func test2(s: String) {
  switch s {
  case "a": // CHECK: string_literal utf8 "a", [[LOC]]:[[@LINE-1]]:10
    nop1()
  case "b": // CHECK: string_literal utf8 "b", [[LOC]]:[[@LINE-3]]:10
    nop2()
  default:
    nop1()
  }
}

// Fallthrough shouldn't affect case statement locations.
// CHECK-LABEL: sil hidden @$S16switch_debuginfo5test31sySS_tF
func test3(s: String) {
  switch s {
  case "a", "b":
    // CHECK: string_literal utf8 "a", [[LOC]]:[[@LINE-2]]:10
    // CHECK: string_literal utf8 "b", [[LOC]]:[[@LINE-3]]:10
    nop1()
    fallthrough
  case "b", "c":
    // CHECK: string_literal utf8 "b", [[LOC]]:[[@LINE-7]]:10
    // CHECK: string_literal utf8 "c", [[LOC]]:[[@LINE-8]]:10
    nop2()
    fallthrough
  default:
    nop1()
  }
}

// It should be possible to set breakpoints on where clauses.
// CHECK-LABEL: sil hidden @$S16switch_debuginfo5test41byAA6BinaryO_tF
func test4(b: Binary) {
  switch b {
  case let _        // CHECK-NOT: [[LOC]]:[[@LINE]]
    where isOn(b):  // CHECK: [[LOC]]:[[@LINE]]:11
    nop1()
  case let _        // CHECK-NOT: [[LOC]]:[[@LINE]]
    where !isOn(b): // CHECK: [[LOC]]:[[@LINE]]:11
    nop2()
  default:
    nop1()
  }
}

// Check that we set the right locations before/after nested switches.
// CHECK-LABEL: sil hidden @$S16switch_debuginfo5test51sySS_tF
func test5(s: String) {
  switch s {
  case "a":         // CHECK: string_literal utf8 "a", [[LOC]]:[[@LINE-1]]:10
    switch "b" {
    case "b":       // CHECK: string_literal utf8 "b", [[LOC]]:[[@LINE-1]]:12
      nop1()
    default:
      nop2()
    }
    if "c" == "c" { // CHECK: string_literal utf8 "c", [[LOC]]:[[@LINE]]
      nop1()
    }
  default:
    nop1()
  }
  if "d" == "d" {   // CHECK: string_literal utf8 "d", [[LOC]]:[[@LINE]]
    nop1()
  }
}
