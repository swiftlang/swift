func apply<Value, Result>(value: Value, body: (Value) -> Result) -> Result {
  return body()
}

// RUN: %sourcekitd-test -req=signaturehelp -pos=2:15 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.active_signature: 0,
// CHECK-NEXT:   key.members: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "body(Value) -> Result",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 5,
// CHECK-NEXT:           key.namelength: 5
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
