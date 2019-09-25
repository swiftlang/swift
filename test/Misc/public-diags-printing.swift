// RUN: not %target-swift-frontend -typecheck %s 2>&1 | %FileCheck %s

let y = 0
var y = 0
// CHECK: error: invalid redeclaration of 'y' [InvalidRedeclaration]
// CHECK: note: 'y' previously declared here

let s = "abc".size
// CHECK: error: value of type 'String' has no member 'size' [NoValueMember]

struct Foo {}
Foo()[3]
// CHECK: error: value of type 'Foo' has no subscripts [NoSubscripts]
