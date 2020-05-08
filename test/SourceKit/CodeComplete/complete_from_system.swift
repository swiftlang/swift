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

// CHECK: {
// CHECK: key.kind: source.lang.swift.decl.function.method.instance,
// CHECK: key.name: "makeIterator()",
// CHECK-NEXT: key.sourcetext: "makeIterator()",
// CHECK-NEXT: key.description: "makeIterator()",
// CHECK-NEXT: key.typename: "IndexingIterator<MyCollection>",
// CHECK-NEXT: key.doc.brief: "{{.*}}",
// CHECK-NEXT: key.context: source.codecompletion.context.superclass,
// CHECK-NEXT: key.typerelation: source.codecompletion.typerelation.unrelated,
// CHECK-NEXT: key.num_bytes_to_erase: 0,
// CHECK-NEXT: key.is_system: 1,
// CHECK-NEXT: key.associated_usrs: "s:Slss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF",
// CHECK-NEXT: key.modulename: "Swift"
// CHECK-NEXT: },
// CHECK: {
// CHECK: key.kind: source.lang.swift.decl.var.instance,
// CHECK: key.name: "startIndex",
// CHECK-NEXT: key.sourcetext: "startIndex",
// CHECK-NEXT: key.description: "startIndex",
// CHECK-NEXT: key.typename: "Int",
// CHECK-NEXT: key.doc.brief: "{{.*}}",
// CHECK-NEXT: key.context: source.codecompletion.context.thisclass,
// CHECK-NEXT: key.typerelation: source.codecompletion.typerelation.unrelated,
// CHECK-NEXT: key.num_bytes_to_erase: 0,
// CHECK-NOT: key.is_system: 1,
// CHECK-NEXT: key.associated_usrs: "s:7TestMod12MyCollectionV10startIndexSivp s:Sl10startIndex0B0Qzvp",
// CHECK-NEXT: key.modulename: "TestMod"
// CHECK-NEXT: },

