// RUN: %target-parse-verify-swift

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
func ovlLitB(_: Int32) -> Int32 {} // expected-note{{}}
func ovlLitB(_: Int64) -> Int64 {} // expected-note{{}}
func testLiteralOverloadinovlLitB() {
  var y32 : Int32 = ovlLitA(ovlLitB(0))
  var y64 : Int64 = ovlLitA(ovlLitB(0))
  var y /*: Int*/ = ovlLitA(ovlLitB(0))  // expected-error{{ambiguous use of 'ovlLitB'}}
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
  doWibble(3.14) // expected-error{{cannot invoke 'doWibble' with an argument list of type '(Double)'}} expected-note{{expected an argument list of type '(CanWibble)'}}
}

