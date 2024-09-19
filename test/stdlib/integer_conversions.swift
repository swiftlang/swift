// RUN: %target-run-simple-swift | %FileCheck %s
// REQUIRES: executable_test

var a : Int

func test_Int8() {
  var i8 : Int8
  i8 = -0x1
  print(Int(i8))
}

func test_UInt8() {
  var ui8 : UInt8
  ui8 = 0xFF
  print(ui8)
  var i8 = Int8(bitPattern: ui8)
  print(Int(i8))
}

func test_UInt32() {
  var ui32 : UInt32
  ui32 = 0xFFFFFFFF
  print(ui32)
  var i8 : Int8
  i8 = Int8(ui32 & (0xF))
  print(String(i8))
}

test_Int8()
test_UInt8()
test_UInt32()

var tentwenty : UInt64
tentwenty = 1000
tentwenty += 20
print(tentwenty)

// CHECK:  -1
// CHECK:  255
// CHECK:  4294967295
// CHECK:  15
// CHECK:  1020


// Test generic conversions from floating point

print(Int8._convert(from: -128))
print(Int8._convert(from: 128))
print(Int8._convert(from: -127))
print(Int8._convert(from: 127))

// CHECK:  (value: Optional(-128), exact: true)
// CHECK:  (value: nil, exact: false)
// CHECK:  (value: Optional(-127), exact: true)
// CHECK:  (value: Optional(127), exact: true)

print(Int8._convert(from: -129))
print(Int8._convert(from: -128.999))
print(Int8._convert(from: -128.001))
print(Int8._convert(from: -127.999))
print(Int8._convert(from: -127.001))
print(Int8._convert(from: 127.001))
print(Int8._convert(from: 127.999))

// CHECK:  (value: nil, exact: false)
// CHECK:  (value: Optional(-128), exact: false)
// CHECK:  (value: Optional(-128), exact: false)
// CHECK:  (value: Optional(-127), exact: false)
// CHECK:  (value: Optional(-127), exact: false)
// CHECK:  (value: Optional(127), exact: false)
// CHECK:  (value: Optional(127), exact: false)

print(Int8._convert(from: 0))
print(Int8._convert(from: -0.0))
print(Int8._convert(from: 0.001))
print(Int8._convert(from: -0.001))

// CHECK:  (value: Optional(0), exact: true)
// CHECK:  (value: Optional(0), exact: true)
// CHECK:  (value: Optional(0), exact: false)
// CHECK:  (value: Optional(0), exact: false)

print(Int8._convert(from: Double.leastNonzeroMagnitude))
print(Int8._convert(from: -Double.leastNonzeroMagnitude))
print(Int8._convert(from: Double.leastNormalMagnitude))
print(Int8._convert(from: -Double.leastNormalMagnitude))

// CHECK:  (value: Optional(0), exact: false)
// CHECK:  (value: Optional(0), exact: false)
// CHECK:  (value: Optional(0), exact: false)
// CHECK:  (value: Optional(0), exact: false)

print(UInt8._convert(from: -1))
print(UInt8._convert(from: -0.9))
print(UInt8._convert(from: 255))
print(UInt8._convert(from: 256))
print(UInt8._convert(from: Double.infinity))
print(UInt8._convert(from: -Double.infinity))
print(UInt8._convert(from: Double.nan))

// CHECK:  (value: nil, exact: false)
// CHECK:  (value: Optional(0), exact: false)
// CHECK:  (value: Optional(255), exact: true)
// CHECK:  (value: nil, exact: false)
// CHECK:  (value: nil, exact: false)
// CHECK:  (value: nil, exact: false)
// CHECK:  (value: nil, exact: false)

let f = Float(Int64.min)
let ff = Int64._convert(from: f)
let fff = Int64(f)
print(fff == ff.value!)
// CHECK:  true

let g = f.nextUp
let gg = Int64._convert(from: g)
let ggg = Int64(g)
print(ggg == gg.value!)
// CHECK:  true

let h = Float(Int64.max)
let hh = Int64._convert(from: h)
print(hh)
// CHECK:  (value: nil, exact: false)

let i = h.nextDown
let ii = Int64._convert(from: i)
let iii = Int64(i)
print(iii == ii.value!)
// CHECK:  true
