// RUN: %target-swift-ide-test -signature-help -code-completion-token=LABELLED -source-filename=%s | %FileCheck %s --check-prefix=LABELLED
// RUN: %target-swift-ide-test -signature-help -code-completion-token=UNLABELLED -source-filename=%s | %FileCheck %s --check-prefix=UNLABELLED

enum Barcode {
  case upc(numberSystem: Int, manufacturer: Int, product: Int, check: Int)
  case qrCode(String)
}

func testLabeled() {
  Barcode.upc(#^LABELLED^#, manufacturer: 85909, product: 51226, check: 3)
  // LABELLED:     Begin signatures, 1 items
  // LABELLED-DAG: Signature[Active]: upc(<param name="numberSystem" active>numberSystem: Int</param>, <param name="manufacturer">manufacturer: Int</param>, <param name="product">product: Int</param>, <param name="check">check: Int</param>) -> Barcode
}

func testUnlabled() {
  Barcode.qrCode(#^UNLABELLED^#)
  // UNLABELLED:     Begin signatures, 1 items
  // UNLABELLED-DAG: Signature[Active]: qrCode(<param active>String</param>) -> Barcode
}
