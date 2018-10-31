// RUN: %target-swiftc_driver -O %s -o /dev/null 2>&1 | %FileCheck -allow-empty -check-prefix=DEFAULT %s
// RUN: %target-swiftc_driver -O -Rpass=sil-inliner %s -o /dev/null 2>&1 | %FileCheck -check-prefix=REMARK_PASSED %s
// RUN: %target-swiftc_driver -O -Rpass-missed=sil-inliner %s -o /dev/null 2>&1 | %FileCheck -check-prefix=REMARK_MISSED %s
// REQUIRES: rdar42247881
// DEFAULT-NOT: remark

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
  // REMARK_MISSED: opt-remark.swift:43:2: remark: Not profitable to inline function "null.big()" (cost = {{.*}}, benefit = {{.*}})
  // REMARK_MISSED-NOT: remark: {{.*}} inlined
	big()
  // REMARK_PASSED-NOT: remark: Not profitable
  // REMARK_PASSED: opt-remark.swift:47:3: remark: "null.small()" inlined into "null.foo()" (cost = {{.*}}, benefit = {{.*}})
  // REMARK_PASSED-NOT: remark: Not profitable
  small()
}
