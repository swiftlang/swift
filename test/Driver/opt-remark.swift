// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -O %s -o %t/throwaway -diagnostic-style llvm 2>&1 | %FileCheck -allow-empty -check-prefix=DEFAULT %s
// RUN: %target-swiftc_driver -O -Rpass=sil-inliner %s -o %t/throwaway -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=REMARK_PASSED %s
// RUN: %target-swiftc_driver -O -Rpass-missed=sil-inliner %s -o %t/throwaway -diagnostic-style llvm 2>&1 | %FileCheck -check-prefix=REMARK_MISSED %s

// REQUIRES: swift_stdlib_no_asserts,optimized_stdlib

// DEFAULT-NOT: remark:

func big() {
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
  print("hello")
}

func small() {
  print("hello")
}

func foo() {
  // REMARK_MISSED-NOT: remark: {{.*}} inlined
  // REMARK_MISSED: opt-remark.swift:46:2: remark: Not profitable to inline function "throwaway.big()" (cost = {{.*}}, benefit = {{.*}})
  // REMARK_MISSED-NOT: remark: {{.*}} inlined
	big()
  // REMARK_PASSED-NOT: remark: Not profitable
  // REMARK_PASSED: opt-remark.swift:50:3: remark: "throwaway.small()" inlined into "throwaway.foo()" (cost = {{.*}}, benefit = {{.*}})
  // REMARK_PASSED-NOT: remark: Not profitable
  small()
}
