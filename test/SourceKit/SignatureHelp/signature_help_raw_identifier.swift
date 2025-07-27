struct `Raw Identifier` {
  func `some method :)`(`param label?` `argument label!`: Int) {}
}

`Raw Identifier`().`some method :)`()

// RUN: %sourcekitd-test -req=signaturehelp -pos=5:37 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.active_signature: 0,
// CHECK-NEXT:   key.members: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "`some method :)`(`param label?`: Int)",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 17,
// CHECK-NEXT:           key.namelength: 19
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 0
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
