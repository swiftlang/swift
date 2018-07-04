// XFAIL: broken_std_regex

struct A {}
struct B {
  let a: A
  let b: A
  let c: A
}

// RUN: %complete-test -tok=TOP_LEVEL_0 %s | %FileCheck -check-prefix=TOP_LEVEL_0_ALL %s
// RUN: %complete-test -tok=TOP_LEVEL_0 %s -limit=0 | %FileCheck -check-prefix=TOP_LEVEL_0_ALL %s
// RUN: %complete-test -tok=TOP_LEVEL_0 %s -limit=3 | %FileCheck -check-prefix=TOP_LEVEL_0_3 %s
// RUN: %complete-test -tok=TOP_LEVEL_0 %s -limit=1 | %FileCheck -check-prefix=TOP_LEVEL_0_1 %s
// RUN: %complete-test -tok=TOP_LEVEL_0 %s -start=1 -limit=1 | %FileCheck -check-prefix=TOP_LEVEL_0_11 %s
// RUN: %complete-test -tok=TOP_LEVEL_0 %s -start=2 -limit=1 | %FileCheck -check-prefix=TOP_LEVEL_0_12 %s
// RUN: %complete-test -tok=TOP_LEVEL_0 %s -raw -start=100000 -limit=1 | %FileCheck -check-prefix=TOP_LEVEL_0_NONE %s
func test001() {
  let x: B
  let y: B
  let z: B
    
  #^TOP_LEVEL_0^#
// TOP_LEVEL_0_ALL: let
// TOP_LEVEL_0_ALL-NEXT: var
// TOP_LEVEL_0_ALL-NEXT: if
// TOP_LEVEL_0_ALL-NEXT: for
// TOP_LEVEL_0_ALL-NEXT: while
// TOP_LEVEL_0_ALL-NEXT: return
// TOP_LEVEL_0_ALL-NEXT: func
// TOP_LEVEL_0_ALL-NEXT: x
// TOP_LEVEL_0_ALL-NEXT: y
// TOP_LEVEL_0_ALL-NEXT: z
// TOP_LEVEL_0_ALL-NEXT: A
// TOP_LEVEL_0_ALL-NEXT: B
// TOP_LEVEL_0_ALL-NEXT: test

// TOP_LEVEL_0_3: let
// TOP_LEVEL_0_3-NEXT: var
// TOP_LEVEL_0_3-NEXT: if
// TOP_LEVEL_0_3-NOT: for

// TOP_LEVEL_0_1: let
// TOP_LEVEL_0_1-NOT: var

// TOP_LEVEL_0_11-NOT: let
// TOP_LEVEL_0_11: var
// TOP_LEVEL_0_11-NOT: if

// TOP_LEVEL_0_12-NOT: let
// TOP_LEVEL_0_12-NOT: var
// TOP_LEVEL_0_12: if

// TOP_LEVEL_0_NONE: [
// TOP_LEVEL_0_NONE-NOT: key.name
// TOP_LEVEL_0_NONE: ]
}

// RUN: %complete-test -tok=B_INSTANCE_0 %s | %FileCheck -check-prefix=B_INSTANCE_0_ALL %s
// RUN: %complete-test -tok=B_INSTANCE_0 %s -limit=1 | %FileCheck -check-prefix=B_INSTANCE_0_1 %s
func test002(x: B) {
  x.#^B_INSTANCE_0^#

// B_INSTANCE_0_ALL: a
// B_INSTANCE_0_ALL-NEXT: b
// B_INSTANCE_0_ALL-NEXT: c
// B_INSTANCE_0_ALL-NEXT: self

// B_INSTANCE_0_1: a
// B_INSTANCE_0_1-NOT: b

// B_INSTANCE_1_
}

struct C {
  let aaa: A
  let aab: A
  let aac: A
  let abc: A
}

// RUN: %complete-test -tok=C_INSTANCE_0 %s | %FileCheck -check-prefix=C_INSTANCE_0_ALL %s
// RUN: %complete-test -tok=C_INSTANCE_0 %s -limit=1 | %FileCheck -check-prefix=C_INSTANCE_0_1 %s
func test003(x: C) {
  x.#^C_INSTANCE_0,aa^#

// C_INSTANCE_0_ALL: aaa
// C_INSTANCE_0_ALL-NEXT: aab
// C_INSTANCE_0_ALL-NEXT: aac
// C_INSTANCE_0_ALL-NOT: abc

// C_INSTANCE_0_1: aaa
// C_INSTANCE_0_1-NOT: aa
}

struct D {
  func aaa(x: A) {}
  func aaa(x: B) {}
  func aab() {}
}

// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s | %FileCheck -check-prefix=OVERLOADS_ALL %s
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -limit=1 | %FileCheck -check-prefix=OVERLOADS_1 %s
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -start=1 -limit=1 | %FileCheck -check-prefix=OVERLOADS_11 %s
func test003(x: D) {
  x.#^D_INSTANCE_0^#

// OVERLOADS_ALL: aaa:
// OVERLOADS_ALL-NEXT:   aaa(x: A)
// OVERLOADS_ALL-NEXT:   aaa(x: B)
// OVERLOADS_ALL-NEXT: aab()
// OVERLOADS_ALL-NEXT: self

// limit applies to top-level, not to subgroups
// OVERLOADS_1: aaa:
// OVERLOADS_1:   aaa(x: A)
// OVERLOADS_1:   aaa(x: B)
// OVERLOADS_1-NOT: aa

// OVERLOADS_11-NOT: aa
// OVERLOADS_11: aab()
}

// If we return all the results, nextrequeststart == 0
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -raw | %FileCheck -check-prefix=NEXT-END %s
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -raw -limit=5 | %FileCheck -check-prefix=NEXT-END %s
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -raw -limit=3 | %FileCheck -check-prefix=NEXT-END %s
// NEXT-END: key.nextrequeststart: 0

// If we return the last result, nextrequeststart == 0
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -raw -start=2 -limit=1 | %FileCheck -check-prefix=NEXT-END %s

// Otherwise, it's the next result
// RUN: %complete-test -group=overloads -tok=D_INSTANCE_0 %s -raw -limit=1 | %FileCheck -check-prefix=NEXT1 %s
// NEXT1: key.nextrequeststart: 1
