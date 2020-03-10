func test() {
  var localVar = 1

  var afterVar = 1
}

// RUN: %sourcekitd-test \
// RUN:   -req=complete -pos=3:1 %s -- %s == \
// RUN:   -req=complete -pos=3:1 %s -- %s == \
// RUN:   -req=complete -pos=3:1 %s -- %s == \
// RUN:   -req=complete -pos=3:1 %s -- %s == \
// RUN:   -req=complete -pos=3:1 %s -- %s | %FileCheck %s

// CHECK-NOT: key.name: "localVar"
// CHECK-NOT: key.name: "afterVar"
// CHECK: key.name: "localVar"
// CHECK-NOT: key.name: "afterVar"
// CHECK: key.name: "localVar"
// CHECK-NOT: key.name: "afterVar"
// CHECK: key.name: "localVar"
// CHECK-NOT: key.name: "afterVar"
// CHECK: key.name: "localVar"
// CHECK-NOT: key.name: "afterVar"
// CHECK: key.name: "localVar"
// CHECK-NOT: key.name: "localVar"
// CHECK-NOT: key.name: "afterVar"
