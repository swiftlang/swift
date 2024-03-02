// RUN: %sourcekitd-test -req=semantic-tokens %s -- %s | %FileCheck %s

class MyClass {}

let x: String = "test"
var y = MyClass()


// String in line 5
// CHECK: key.kind: source.lang.swift.ref.struct
// CHECK: key.length: 6,
// CHECK: key.is_system: 1
// CHECK-LABEL: },
    
// MyClass in line 6
// CHECK: key.kind: source.lang.swift.ref.class,
// CHECK: key.length: 7
