// RUN: %target-swift-ide-test -signature-help -code-completion-token=LABELLED -source-filename=%s | %FileCheck %s --check-prefix=LABELLED
// RUN: %target-swift-ide-test -signature-help -code-completion-token=UNLABELLED -source-filename=%s | %FileCheck %s --check-prefix=UNLABELLED

enum Barcode {
  case upc(numberSystem: Int, manufacturer: Int, product: Int, check: Int)
  case qrCode(String)
}

func testLabeled() {
  Barcode.upc(#^LABELLED^#, manufacturer: 85909, product: 51226, check: 3)
}

func testUnlabled() {
  Barcode.qrCode(#^UNLABELLED^#)
}

// LABELLED:      Begin signatures, 1 items
// LABELLED-NEXT: Signature[Active]: upc(<param name="numberSystem" active>numberSystem: Int</param>, <param name="manufacturer">manufacturer: Int</param>, <param name="product">product: Int</param>, <param name="check">check: Int</param>) -> Barcode
// LABELLED-NEXT: End signatures

// UNLABELLED:      Begin signatures, 1 items
// UNLABELLED-NEXT: Signature[Active]: qrCode(<param name="" active>String</param>) -> Barcode
// UNLABELLED-NEXT: End signatures
