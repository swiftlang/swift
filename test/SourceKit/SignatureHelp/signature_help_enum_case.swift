enum Barcode {
  case upc(numberSystem: Int, manufacturer: Int, product: Int, check: Int)
  case qrCode(String)
}

func testLabeled() {
  Barcode.upc(, manufacturer: 85909, product: 51226, check: 3)
}

func testUnlabled() {
  Barcode.qrCode()
}

// RUN: %sourcekitd-test -req=signaturehelp -pos=7:15 %s -- %s | %FileCheck -check-prefix=LABELLED %s
// RUN: %sourcekitd-test -req=signaturehelp -pos=11:18 %s -- %s | %FileCheck -check-prefix=UNLABELLED %s

// LABELLED:      {
// LABELLED-NEXT:   key.active_signature: 0,
// LABELLED-NEXT:   key.members: [
// LABELLED-NEXT:     {
// LABELLED-NEXT:       key.name: "upc(numberSystem: Int, manufacturer: Int, product: Int, check: Int) -> Barcode",
// LABELLED-NEXT:       key.parameters: [
// LABELLED-NEXT:         {
// LABELLED-NEXT:           key.nameoffset: 4,
// LABELLED-NEXT:           key.namelength: 17
// LABELLED-NEXT:         },
// LABELLED-NEXT:         {
// LABELLED-NEXT:           key.nameoffset: 23,
// LABELLED-NEXT:           key.namelength: 17
// LABELLED-NEXT:         },
// LABELLED-NEXT:         {
// LABELLED-NEXT:           key.nameoffset: 42,
// LABELLED-NEXT:           key.namelength: 12
// LABELLED-NEXT:         },
// LABELLED-NEXT:         {
// LABELLED-NEXT:           key.nameoffset: 56,
// LABELLED-NEXT:           key.namelength: 10
// LABELLED-NEXT:         }
// LABELLED-NEXT:       ],
// LABELLED-NEXT:       key.active_parameter: 0
// LABELLED-NEXT:     }
// LABELLED-NEXT:   ]
// LABELLED-NEXT: }

// UNLABELLED:      {
// UNLABELLED-NEXT:   key.active_signature: 0,
// UNLABELLED-NEXT:   key.members: [
// UNLABELLED-NEXT:     {
// UNLABELLED-NEXT:       key.name: "qrCode(String) -> Barcode",
// UNLABELLED-NEXT:       key.parameters: [
// UNLABELLED-NEXT:         {
// UNLABELLED-NEXT:           key.nameoffset: 7,
// UNLABELLED-NEXT:           key.namelength: 6
// UNLABELLED-NEXT:         }
// UNLABELLED-NEXT:       ],
// UNLABELLED-NEXT:       key.active_parameter: 0
// UNLABELLED-NEXT:     }
// UNLABELLED-NEXT:   ]
// UNLABELLED-NEXT: }
