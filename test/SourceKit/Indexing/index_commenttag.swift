// RUN: %sourcekitd-test -req=index %s -- %s | %FileCheck %s


/// - Tag: SomeTag
/// - Tag: Other
func testFunc() {}

// CHECK-LABEL: key.entities: [

// CHECK:     {
// CHECK-DAG:   key.kind: source.lang.swift.commenttag
// CHECK-DAG:   key.usr: "t:SomeTag"
// CHECK:     }

// CHECK:     {
// CHECK-DAG:   key.kind: source.lang.swift.commenttag
// CHECK-DAG:   key.usr: "t:Other"
// CHECK:     }

// CHECK:     {
// CHECK-DAG:   key.kind: source.lang.swift.decl.function.free
// CHECK-DAG:   key.name: "testFunc()"
// CHECK-DAG:   key.effective_access: source.decl.effective_access.internal
// CHECK:     }

// CHECK: ]
