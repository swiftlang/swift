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
func ovlLitA(_ : Int32) -> Int32 {}
func ovlLitA(_ : Int64) -> Int64 {}
func ovlLitB(_ : Int32) -> Int32 {}
func ovlLitB(_ : Int64) -> Int64 {}
func testLiteralOverloadinovlLitB() {
  var y32 : Int32 = ovlLitA(ovlLitB(0))
  var y64a : Int64 = ovlLitA(ovlLitB(0))
  var y64b = ovlLitA(ovlLitB(0))
  y64a = y64b
  y64b = y64a
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

func doWibble(_ : CanWibble) {}

func testWibble() {
  doWibble(1)
  doWibble(3.14) // expected-error{{type 'Double' does not conform to protocol 'CanWibble'}} expected-note{{while converting function argument}}
}

