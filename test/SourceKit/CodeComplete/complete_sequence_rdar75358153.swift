// BEGIN t1.swift
enum E {
  case foo, bar
}
func foo(_ arg: E) {}
func test(val: E) {
  foo(.bar)
}

// BEGIN t2.swift
enum E {
  case foo, bar
}
func foo(_ arg: E) {}
func test(val: E) {
  foo(.bar)
  if v
}

// BEGIN dummy.swift

// rdar://75358153
// Previously, completing inside single expression body, then completing inside
// *non* single expression body in the same function caused a crash because the
// "has single expression" flag didn't cleard. This file tests the scenario not
// to regress again.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=6:8 -text-input %t/t1.swift %t/t.swift -- %t/t.swift == \
// RUN:   -req=complete -pos=7:6 -text-input %t/t2.swift %t/t.swift -- %t/t.swift \
// RUN: | %FileCheck %s

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.description: "bar",
// CHECK-DAG: key.description: "foo",
// CHECK-DAG: key.description: "hash(self: E)",
// CHECK: ]
// CHECK-NOT: key.reusingastcontext: 1

// CHECK-LABEL: key.results: [
// CHECK-DAG: key.description: "val",
// CHECK-DAG: key.description: "foo(arg: E)",
// CHECK: ],
// CHECK: key.reusingastcontext: 1
