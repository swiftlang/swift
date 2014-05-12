// RUN: %target-run-simple-swift | FileCheck %s

var a : Int

func test_Int8() {
  var i8 : Int8
  i8 = -0x1
  println(Int(i8))
  var ui8: UInt8 = i8.asUnsigned()
  println(ui8)  
}

func test_UInt8() {
  var ui8 : UInt8
  ui8 = 0xFF
  println(ui8)
  var i8 : Int8
  i8 = ui8.asSigned()
  println(Int(i8))
}

func test_UInt32() {
  var ui32 : UInt32
  ui32 = 0xFFFFFFFF
  println(ui32)
  var i8 : Int8
  i8 = Int8(ui32 & (0xF))
  println(String(i8))
}

test_Int8()
test_UInt8()
test_UInt32()

var tentwenty : UInt64
tentwenty = 1000
tentwenty += 20
println(tentwenty)

// CHECK:  -1
// CHECK:  255
// CHECK:  255
// CHECK:  -1
// CHECK:  4294967295
// CHECK:  15
// CHECK:  1020
