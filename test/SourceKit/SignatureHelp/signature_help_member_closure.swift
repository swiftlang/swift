struct Observable {
  var observer: (String, Int?, [AnyHashable: [Double?]]) async throws -> [Observable?]
  
  func notify() async throws {
    observer("EVENT", , [:])
  }
}

// RUN: %sourcekitd-test -req=signaturehelp -pos=5:23 %s -- %s | %FileCheck -check-prefix=CHECK %s

// CHECK:      {
// CHECK-NEXT:   key.signatures: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.name: "observer(String, Int?, [AnyHashable : [Double?]]) async throws -> [Observable?]",
// CHECK-NEXT:       key.parameters: [
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 9,
// CHECK-NEXT:           key.namelength: 6
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 17,
// CHECK-NEXT:           key.namelength: 4
// CHECK-NEXT:         },
// CHECK-NEXT:         {
// CHECK-NEXT:           key.nameoffset: 23,
// CHECK-NEXT:           key.namelength: 25
// CHECK-NEXT:         }
// CHECK-NEXT:       ],
// CHECK-NEXT:       key.active_parameter: 1
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.active_signature: 0
// CHECK-NEXT: }
