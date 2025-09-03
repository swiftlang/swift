// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %sourcekitd-test -req=signaturehelp -pos=7:15 %t/input.swift -- %t/input.swift > %t/actual_labelled.result
// RUN: diff -u %t/expected_labelled.result %t/actual_labelled.result
// RUN: %sourcekitd-test -req=signaturehelp -pos=11:18 %t/input.swift -- %t/input.swift > %t/actual_unlabelled.result
// RUN: diff -u %t/expected_unlabelled.result %t/actual_unlabelled.result

//--- input.swift
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

//--- expected_labelled.result
{
  key.signatures: [
    {
      key.name: "upc(numberSystem: Int, manufacturer: Int, product: Int, check: Int) -> Barcode",
      key.parameters: [
        {
          key.nameoffset: 4,
          key.namelength: 17
        },
        {
          key.nameoffset: 23,
          key.namelength: 17
        },
        {
          key.nameoffset: 42,
          key.namelength: 12
        },
        {
          key.nameoffset: 56,
          key.namelength: 10
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
//--- expected_unlabelled.result
{
  key.signatures: [
    {
      key.name: "qrCode(String) -> Barcode",
      key.parameters: [
        {
          key.nameoffset: 7,
          key.namelength: 6
        }
      ],
      key.active_parameter: 0
    }
  ],
  key.active_signature: 0
}
