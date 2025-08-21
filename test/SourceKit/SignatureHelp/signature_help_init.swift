struct Person {
  init(name: String, age: Int, profession job: String) { }
}

Person(name: "John", age: )

// RUN: %sourcekitd-test -req=signaturehelp -pos=5:27 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.signatures: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "init(name: String, age: Int, profession: String)",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 5,
// CHECK-NEXT:           key.namelength: 12
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 19,
// CHECK-NEXT:           key.namelength: 8
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 29,
// CHECK-NEXT:           key.namelength: 18
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 1
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.active_signature: 0
// CHECK-NEXT: }
