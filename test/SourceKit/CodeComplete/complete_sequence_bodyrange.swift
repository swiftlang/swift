class TestChain {
  class Child {
    var value: Struct1?
  }
  class Struct1 {
    var value: Struct2
  }
  class Struct2 {
    var prop1: Int
    var prop2: Int
  }

  var child: Child!

  func foo() {
    let _ = child.value?.value.
  }
}

// rdar://problem/58098222

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s == \
// RUN:   -req=complete -pos=16:32 %s -async -- %s | %FileCheck %s

// CHECK-NOT: key.name: "prop1"
// CHECK-NOT: key.name: "prop2"
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK: key.name: "prop1",
// CHECK: key.name: "prop2",
// CHECK-NOT: key.name: "prop1"
// CHECK-NOT: key.name: "prop2"
