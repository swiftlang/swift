struct MyStruct { func foo() {} }

#if DEBUG
import Swift
#endif

#if arch(x86_64)
operator ++++++
#endif

#if !targetEnvironment(simulator)
precedencegroup MyPrecedence
#endif

func foo(value: MyStruct) {
  value.
}

// Ensure that compilation directives are equally evaluated and doesn't affect the fast completions.

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=16:9 -repeat-request=2 %s -- %s -target %target-triple \
// RUN:   | %FileCheck %s

// CHECK-LABEL: key.results: [
// CHECK-NOT: ]
// CHECK-DAG: key.description: "foo()",
// CHECK-DAG: key.description: "self",
// CHECK: ]
// CHECK-NOT: key.reusingastcontext

// CHECK-LABEL: key.results: [
// CHECK-NOT: ]
// CHECK-DAG: key.description: "foo()",
// CHECK-DAG: key.description: "self",
// CHECK: ],
// CHECK: key.reusingastcontext: 1
