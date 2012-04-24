// RUN: %swift %s -i | FileCheck %s

var a : Int

func test_Int8() {
  var i8 : Int8
  i8 = 0xFF
  print(Int(i8))
}

func test_UInt8() {
  var ui8 : UInt8
  ui8 = 0xFF
  print(Int(ui8))
  var i8 : Int8
  i8 = Int8(ui8)
  print(Int(i8))
}

test_Int8()
test_UInt8()

// CHECK:  -1
// CHECK:  255
// CHECK:  -1
