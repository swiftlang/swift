struct Vector<Value> {
  init(elements: [Value]) { }
  
  func dot(with other: Vector<Value>) -> Value { }
}

let vec = Vector(elements: [1.0, 2.1, 3.4])
vec.dot(with: )

// RUN: %sourcekitd-test -req=signaturehelp -pos=8:15 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.signatures: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "dot(with: Vector<Double>) -> Double",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 20
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.active_signature: 0
// CHECK-NEXT: }
