// RUN: %swift %s -verify

//===----------------------------------------------------------------------===//
// Refer to members of literals
//===----------------------------------------------------------------------===//
func testLiteralMembers() {
  var x = 0.value
  Int(0.value)
}


//===----------------------------------------------------------------------===//
// Overloading with literals
//===----------------------------------------------------------------------===//
func ovlLitA(_: Int32) -> Int32 {}
func ovlLitA(_: Int64) -> Int64 {}
func ovlLitB(_: Int32) -> Int32 {}
func ovlLitB(_: Int64) -> Int64 {}
func testLiteralOverloadinovlLitB() {
  var y32 : Int32 = ovlLitA(ovlLitB(0))
  var y64 : Int64 = ovlLitA(ovlLitB(0))
  var y /*: Int*/ = ovlLitA(ovlLitB(0))  // expected-error{{could not find an overload for 'ovlLitA' that accepts the supplied arguments}}
}

func literalOverloadSameReturn(i: Int) -> Int {}
func literalOverloadSameReturn(i: Int32) -> Int {}
func literalOverload2() {
  var x : Int = literalOverloadSameReturn(literalOverloadSameReturn(1))
}

//===----------------------------------------------------------------------===//
// Literals and protocols
//===----------------------------------------------------------------------===//
protocol CanWibble {
  func wibble()
}

extension Int : CanWibble {
  func wibble() {}
}

func doWibble(_: CanWibble) {}

func testWibble() {
  doWibble(1)
  doWibble(3.14) // expected-error{{cannot convert the expression's type '()' to type 'CanWibble'}}
}

