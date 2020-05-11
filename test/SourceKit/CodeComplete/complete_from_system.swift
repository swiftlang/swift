struct MyCollection : Collection {
  var startIndex: Int { 0 }
  var endIndex: Int { 0 }
  func index(after i: Int) -> Int { i + 1 }
  subscript(position: Int) -> Int { 0 }
}

func test(col: MyCollection) {
  col.
}

// RUN: %sourcekitd-test -req=complete -pos=9:7 %s -- %s -module-name TestMod | %FileCheck %s

// CHECK: key.name: "makeIterator()",
// CHECK-NOT: },
// CHECK: key.is_system: 1
// CHECK: },

// CHECK: key.name: "startIndex",
// CHECK-NOT: },
// CHECK-NOT: key.is_system: 1
// CHECK: },
