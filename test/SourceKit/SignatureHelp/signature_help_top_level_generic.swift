func add<T>(x: T, y: T, with adder: (T, T) -> T) -> T where T: AdditiveArithmetic {
  return adder(x, y)
}

add(x: "A", y: "B", with: )

// RUN: %sourcekitd-test -req=signaturehelp -pos=5:27 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.active_signature: 0,
// CHECK-NEXT:   key.members: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "add(x: String, y: String, with: (String, String) -> String) -> String",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 4,
// CHECK-NEXT:           key.namelength: 9
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 15,
// CHECK-NEXT:           key.namelength: 9
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 26,
// CHECK-NEXT:           key.namelength: 32
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 2
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
