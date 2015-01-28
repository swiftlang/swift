// RUN: mkdir -p %t
// RUN: %target-build-swift %s -parse-stdlib -Xfrontend -disable-access-control -o %t/a.out -Xlinker -dead_strip
// RUN: %target-run %t/a.out env | FileCheck %s
// RUN: %target-run %t/a.out ru_RU.UTF-8 | FileCheck %s
// REQUIRES: sdk

import Swift
import Darwin

// Interpret the command line arguments.
var arg = Process.arguments[1]

if arg == "env" {
  setlocale(LC_ALL, "")
} else {
  setlocale(LC_ALL, arg)
}

func stdlibTypesHaveDescription() {
  func hasDescription(_: Printable) {}

  hasDescription(Int(42))
  hasDescription(UInt(42))

  hasDescription(Int8(-42))
  hasDescription(Int16(-42))
  hasDescription(Int32(-42))
  hasDescription(Int64(-42))
  hasDescription(UInt8(42))
  hasDescription(UInt16(42))
  hasDescription(UInt32(42))
  hasDescription(UInt64(42))

  hasDescription(Bool(true))

  hasDescription(CChar(42))
  hasDescription(CUnsignedChar(42))
  hasDescription(CUnsignedShort(42))
  hasDescription(CUnsignedInt(42))
  hasDescription(CUnsignedLong(42))
  hasDescription(CUnsignedLongLong(42))
  hasDescription(CSignedChar(42))
  hasDescription(CShort(42))
  hasDescription(CInt(42))
  hasDescription(CLong(42))
  hasDescription(CLongLong(42))
  hasDescription(CFloat(1.0))
  hasDescription(CDouble(1.0))

  hasDescription(CWideChar(42))
  hasDescription(CChar16(42))
  hasDescription(CChar32(42))
  hasDescription(CBool(true))
}

var failed = false

func printedIs<T>(
    object: T, expected1: String, expected2: String? = nil,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  var actual = toString(object)
  var match = expected1 == actual
  if !match && expected2 != nil {
    match = expected2! == actual
  }
  if !match {
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected1)\" or \"\(expected2)\"")
    println("actual: \"\(actual)\"")
    println()
    failed = true
  }
}

func debugPrintedIs<T>(
    object: T, expected1: String, expected2: String? = nil,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  var actual = ""
  debugPrint(object, &actual)
  if expected1 != actual && (expected2 != nil && expected2! != actual) {
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected1)\" or \"\(expected2)\"")
    println("actual: \"\(actual)\"")
    println()
    failed = true
  }
}

func assertEquals(
    expected: String, actual: String,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if expected != actual {
    println("check failed at \(file), line \(line)")
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    println()
    failed = true
  }
}

func test_StdlibTypesPrinted() {
  printedIs(Float(1.0), "1.0")
  printedIs(Float(-1.0), "-1.0")
  printedIs(Double(1.0), "1.0")
  printedIs(Double(-1.0), "-1.0")

  printedIs(CChar(42), "42")
  printedIs(CUnsignedChar(42), "42")
  printedIs(CUnsignedShort(42), "42")
  printedIs(CUnsignedInt(42), "42")
  printedIs(CUnsignedLong(42), "42")
  printedIs(CUnsignedLongLong(42), "42")
  printedIs(CSignedChar(42), "42")
  printedIs(CShort(42), "42")
  printedIs(CInt(42), "42")
  printedIs(CLong(42), "42")
  printedIs(CLongLong(42), "42")
  printedIs(CFloat(1.0), "1.0")
  printedIs(CFloat(-1.0), "-1.0")
  printedIs(CDouble(1.0), "1.0")
  printedIs(CDouble(-1.0), "-1.0")

  printedIs(CWideChar(42), "*")
  printedIs(CChar16(42), "42")
  printedIs(CChar32(42), "*")
  printedIs(CBool(true), "true")
  printedIs(CBool(false), "false")

  var s: String = "abc"
  printedIs(s, "abc")
  debugPrintedIs(s, "\"abc\"")
  s = "\\ \' \" \0 \n \r \t \u{05}"
  debugPrintedIs(s, "\"\\\\ \\\' \\\" \\0 \\n \\r \\t \\u{05}\"")

  var ch: Character = "a"
  printedIs(ch, "a")

  var us: UnicodeScalar = "a"
  printedIs(us, "a")
  debugPrintedIs(us, "\"a\"")
  us = "\\"
  printedIs(us, "\\")
  assertEquals("\"\\\\\"", us.description)
  debugPrintedIs(us, "\"\\\\\"")
  us = "あ"
  printedIs(us, "あ")
  assertEquals("\"あ\"", us.description)
  debugPrintedIs(us, "\"\\u{3042}\"")

  if true {
    var implicitlyUnwrappedString: String! = nil
    printedIs(implicitlyUnwrappedString, "nil")
    implicitlyUnwrappedString = "meow"
    printedIs(implicitlyUnwrappedString, "meow")
  }
  if true {
    var optionalString: String? = nil
    printedIs(optionalString, "nil")
    optionalString = "meow"
    printedIs(optionalString, "Optional(\"meow\")")
  }
  if true {
    struct Wrapper: Printable {
      let x: Printable? = nil

      var description: String {
        return "Wrapper(" + x.debugDescription + ")"
      }
    }
    printedIs(Wrapper(), "Wrapper(nil)")
    printedIs(Wrapper(x: Wrapper()), "Wrapper(Optional(Wrapper(nil)))")
    printedIs(Wrapper(x: Wrapper(x: Wrapper())),
        "Wrapper(Optional(Wrapper(Optional(Wrapper(nil)))))")
  }

  println("test_StdlibTypesPrinted done")
}
test_StdlibTypesPrinted()
// CHECK: test_StdlibTypesPrinted done

func test_IntegerPrinting() {
  if (UInt64(Int.max) > 0x1_0000_0000 as UInt64) {
    printedIs(Int.min, "-9223372036854775808")
    printedIs(Int.max, "9223372036854775807")
  } else {
    printedIs(Int.min, "-2147483648")
    printedIs(Int.max, "2147483647")
  }
  printedIs(Int(0), "0")
  printedIs(Int(42), "42")
  printedIs(Int(-42), "-42")

  if (UInt64(UInt.max) > 0x1_0000_0000 as UInt64) {
    printedIs(UInt.max, "18446744073709551615")
  } else {
    printedIs(UInt.max, "4294967295")
  }
  printedIs(UInt.min, "0")
  printedIs(UInt(0), "0")
  printedIs(UInt(42), "42")

  printedIs(Int8.min, "-128")
  printedIs(Int8.max, "127")
  printedIs(Int8(0), "0")
  printedIs(Int8(42), "42")
  printedIs(Int8(-42), "-42")

  printedIs(UInt8.min, "0")
  printedIs(UInt8.max, "255")
  printedIs(UInt8(0), "0")
  printedIs(UInt8(42), "42")

  printedIs(Int16.min, "-32768")
  printedIs(Int16.max, "32767")
  printedIs(Int16(0), "0")
  printedIs(Int16(42), "42")
  printedIs(Int16(-42), "-42")

  printedIs(UInt16.min, "0")
  printedIs(UInt16.max, "65535")
  printedIs(UInt16(0), "0")
  printedIs(UInt16(42), "42")

  printedIs(Int32.min, "-2147483648")
  printedIs(Int32.max, "2147483647")
  printedIs(Int32(0), "0")
  printedIs(Int32(42), "42")
  printedIs(Int32(-42), "-42")

  printedIs(UInt32.min, "0")
  printedIs(UInt32.max, "4294967295")
  printedIs(UInt32(0), "0")
  printedIs(UInt32(42), "42")

  printedIs(Int64.min, "-9223372036854775808")
  printedIs(Int64.max, "9223372036854775807")
  printedIs(Int64(0), "0")
  printedIs(Int64(42), "42")
  printedIs(Int64(-42), "-42")

  printedIs(UInt64.min, "0")
  printedIs(UInt64.max, "18446744073709551615")
  printedIs(UInt64(0), "0")
  printedIs(UInt64(42), "42")

  printedIs(Int8(-42), "-42")
  printedIs(Int16(-42), "-42")
  printedIs(Int32(-42), "-42")
  printedIs(Int64(-42), "-42")
  printedIs(UInt8(42), "42")
  printedIs(UInt16(42), "42")
  printedIs(UInt32(42), "42")
  printedIs(UInt64(42), "42")

  println("test_IntegerPrinting done")
}
test_IntegerPrinting()
// CHECK: test_IntegerPrinting done

func test_FloatingPointPrinting() {
  func asFloat32(f: Float32) -> Float32 { return f }
  func asFloat64(f: Float64) -> Float64 { return f }
#if arch(i386) || arch(x86_64)
  func asFloat80(f: Swift.Float80) -> Swift.Float80 { return f }
#endif

  printedIs(Float.infinity, "inf")
  printedIs(-Float.infinity, "-inf")
  printedIs(Float.NaN, "nan")
  printedIs(asFloat32(0.0), "0.0")
  printedIs(asFloat32(1.0), "1.0")
  printedIs(asFloat32(-1.0), "-1.0")
  printedIs(asFloat32(100.125), "100.125")
  printedIs(asFloat32(-100.125), "-100.125")

  printedIs(Double.infinity, "inf")
  printedIs(-Double.infinity, "-inf")
  printedIs(Double.NaN, "nan")
  printedIs(asFloat64(0.0), "0.0")
  printedIs(asFloat64(1.0), "1.0")
  printedIs(asFloat64(-1.0), "-1.0")
  printedIs(asFloat64(100.125), "100.125")
  printedIs(asFloat64(-100.125), "-100.125")

  printedIs(asFloat32(1.00001), "1.00001")
  printedIs(asFloat32(125000000000000000.0), "1.25e+17")
  printedIs(asFloat32(12500000000000000.0),  "1.25e+16")
  printedIs(asFloat32(1250000000000000.0),   "1.25e+15")
  printedIs(asFloat32(125000000000000.0), "1.25e+14")
  printedIs(asFloat32(12500000000000.0),  "1.25e+13")
  printedIs(asFloat32(1250000000000.0),   "1.25e+12")
  printedIs(asFloat32(125000000000.0),    "1.25e+11")
  printedIs(asFloat32(12500000000.0),     "1.25e+10")
  printedIs(asFloat32(1250000000.0),      "1.25e+09")
  printedIs(asFloat32(125000000.0),       "1.25e+08")
  printedIs(asFloat32(12500000.0),        "1.25e+07")
  printedIs(asFloat32(1250000.0),         "1.25e+06")
  printedIs(asFloat32(125000.0),          "125000.0")
  printedIs(asFloat32(12500.0),           "12500.0")
  printedIs(asFloat32(1250.0),            "1250.0")
  printedIs(asFloat32(125.0), "125.0")
  printedIs(asFloat32(12.5),  "12.5")
  printedIs(asFloat32(1.25),  "1.25")
  printedIs(asFloat32(0.125), "0.125")
  printedIs(asFloat32(0.0125),             "0.0125")
  printedIs(asFloat32(0.00125),            "0.00125")
  printedIs(asFloat32(0.000125),           "0.000125")
  printedIs(asFloat32(0.0000125),          "1.25e-05")
  printedIs(asFloat32(0.00000125),         "1.25e-06")
  printedIs(asFloat32(0.000000125),        "1.25e-07")
  printedIs(asFloat32(0.0000000125),       "1.25e-08")
  printedIs(asFloat32(0.00000000125),      "1.25e-09")
  printedIs(asFloat32(0.000000000125),     "1.25e-10")
  printedIs(asFloat32(0.0000000000125),    "1.25e-11")
  printedIs(asFloat32(0.00000000000125),   "1.25e-12")
  printedIs(asFloat32(0.000000000000125),  "1.25e-13")
  printedIs(asFloat32(0.0000000000000125), "1.25e-14")
  printedIs(asFloat32(0.00000000000000125),   "1.25e-15")
  printedIs(asFloat32(0.000000000000000125),  "1.25e-16")
  printedIs(asFloat32(0.0000000000000000125), "1.25e-17")

  printedIs(asFloat64(1.00000000000001), "1.00000000000001")
  printedIs(asFloat64(125000000000000000.0), "1.25e+17")
  printedIs(asFloat64(12500000000000000.0),  "1.25e+16")
  printedIs(asFloat64(1250000000000000.0),   "1.25e+15")
  printedIs(asFloat64(125000000000000.0), "125000000000000.0")
  printedIs(asFloat64(12500000000000.0),  "12500000000000.0")
  printedIs(asFloat64(1250000000000.0),   "1250000000000.0")
  printedIs(asFloat64(125000000000.0),    "125000000000.0")
  printedIs(asFloat64(12500000000.0),     "12500000000.0")
  printedIs(asFloat64(1250000000.0),      "1250000000.0")
  printedIs(asFloat64(125000000.0),       "125000000.0")
  printedIs(asFloat64(12500000.0),        "12500000.0")
  printedIs(asFloat64(1250000.0),         "1250000.0")
  printedIs(asFloat64(125000.0),          "125000.0")
  printedIs(asFloat64(12500.0),           "12500.0")
  printedIs(asFloat64(1250.0),            "1250.0")
  printedIs(asFloat64(125.0), "125.0")
  printedIs(asFloat64(12.5),  "12.5")
  printedIs(asFloat64(1.25),  "1.25")
  printedIs(asFloat64(0.125), "0.125")
  printedIs(asFloat64(0.0125),             "0.0125")
  printedIs(asFloat64(0.00125),            "0.00125")
  printedIs(asFloat64(0.000125),           "0.000125")
  printedIs(asFloat64(0.0000125),          "1.25e-05")
  printedIs(asFloat64(0.00000125),         "1.25e-06")
  printedIs(asFloat64(0.000000125),        "1.25e-07")
  printedIs(asFloat64(0.0000000125),       "1.25e-08")
  printedIs(asFloat64(0.00000000125),      "1.25e-09")
  printedIs(asFloat64(0.000000000125),     "1.25e-10")
  printedIs(asFloat64(0.0000000000125),    "1.25e-11")
  printedIs(asFloat64(0.00000000000125),   "1.25e-12")
  printedIs(asFloat64(0.000000000000125),  "1.25e-13")
  printedIs(asFloat64(0.0000000000000125), "1.25e-14")
  printedIs(asFloat64(0.00000000000000125),   "1.25e-15")
  printedIs(asFloat64(0.000000000000000125),  "1.25e-16")
  printedIs(asFloat64(0.0000000000000000125), "1.25e-17")

#if arch(i386) || arch(x86_64)
  printedIs(asFloat80(1.00000000000000001), "1.00000000000000001")
  printedIs(asFloat80(12500000000000000000.0), "1.25e+19")
  printedIs(asFloat80(1250000000000000000.0),  "1.25e+18")
  printedIs(asFloat80(125000000000000000.0),   "125000000000000000.0")
  printedIs(asFloat80(12500000000000000.0),    "12500000000000000.0")
  printedIs(asFloat80(1250000000000000.0),     "1250000000000000.0")
  printedIs(asFloat80(125000000000000.0), "125000000000000.0")
  printedIs(asFloat80(12500000000000.0),  "12500000000000.0")
  printedIs(asFloat80(1250000000000.0),   "1250000000000.0")
  printedIs(asFloat80(125000000000.0),    "125000000000.0")
  printedIs(asFloat80(12500000000.0),     "12500000000.0")
  printedIs(asFloat80(1250000000.0),      "1250000000.0")
  printedIs(asFloat80(125000000.0),       "125000000.0")
  printedIs(asFloat80(12500000.0),        "12500000.0")
  printedIs(asFloat80(1250000.0),         "1250000.0")
  printedIs(asFloat80(125000.0),          "125000.0")
  printedIs(asFloat80(12500.0),           "12500.0")
  printedIs(asFloat80(1250.0),            "1250.0")
  printedIs(asFloat80(125.0), "125.0")
  printedIs(asFloat80(12.5),  "12.5")
  printedIs(asFloat80(1.25),  "1.25")
  printedIs(asFloat80(0.125), "0.125")
  printedIs(asFloat80(0.0125),             "0.0125")
  printedIs(asFloat80(0.00125),            "0.00125")
  printedIs(asFloat80(0.000125),           "0.000125")
  printedIs(asFloat80(0.0000125),          "1.25e-05")
  printedIs(asFloat80(0.00000125),         "1.25e-06")
  printedIs(asFloat80(0.000000125),        "1.25e-07")
  printedIs(asFloat80(0.0000000125),       "1.25e-08")
  printedIs(asFloat80(0.00000000125),      "1.25e-09")
  printedIs(asFloat80(0.000000000125),     "1.25e-10")
  printedIs(asFloat80(0.0000000000125),    "1.25e-11")
  printedIs(asFloat80(0.00000000000125),   "1.25e-12")
  printedIs(asFloat80(0.000000000000125),  "1.25e-13")
  printedIs(asFloat80(0.0000000000000125), "1.25e-14")
  printedIs(asFloat80(0.00000000000000125),   "1.25e-15")
  printedIs(asFloat80(0.000000000000000125),  "1.25e-16")
  printedIs(asFloat80(0.0000000000000000125), "1.25e-17")
#endif

  println("test_FloatingPointPrinting done")
}
test_FloatingPointPrinting()
// CHECK: test_FloatingPointPrinting done


func test_BoolPrinting() {
  printedIs(Bool(true), "true")
  printedIs(Bool(false), "false")

  printedIs(true, "true")
  printedIs(false, "false")

  println("test_BoolPrinting done")
}
test_BoolPrinting()
// CHECK: test_BoolPrinting done

func test_CTypesPrinting() {
  printedIs(CChar(42), "42")
  printedIs(CUnsignedChar(42), "42")
  printedIs(CUnsignedShort(42), "42")
  printedIs(CUnsignedInt(42), "42")
  printedIs(CUnsignedLong(42), "42")
  printedIs(CUnsignedLongLong(42), "42")
  printedIs(CSignedChar(42), "42")
  printedIs(CShort(42), "42")
  printedIs(CInt(42), "42")
  printedIs(CLong(42), "42")
  printedIs(CLongLong(42), "42")
  printedIs(CFloat(1.0), "1.0")
  printedIs(CFloat(-1.0), "-1.0")
  printedIs(CDouble(1.0), "1.0")
  printedIs(CDouble(-1.0), "-1.0")

  printedIs(CWideChar(42), "*")
  printedIs(CChar16(42), "42")
  printedIs(CChar32(42), "*")
  printedIs(CBool(true), "true")
  printedIs(CBool(false), "false")

  println("test_CTypesPrinting done")
}
test_CTypesPrinting()
// CHECK: test_CTypesPrinting done


func test_PointerPrinting() {
  let nullUP = UnsafeMutablePointer<Float>()
  let fourByteUP = UnsafeMutablePointer<Float>(bitPattern: 0xabcd1234 as UInt)

#if !(arch(i386) || arch(arm))
  let eightByteAddr: UWord = 0xabcddcba12344321
  let eightByteUP = UnsafeMutablePointer<Float>(bitPattern: eightByteAddr)
#endif

#if arch(i386) || arch(arm)
  let expectedNull = "0x00000000"
  printedIs(fourByteUP, "0xabcd1234")
#else
  let expectedNull = "0x0000000000000000"
  printedIs(fourByteUP, "0x00000000abcd1234")
  printedIs(eightByteUP, "0xabcddcba12344321")
#endif

  printedIs(nullUP, expectedNull)

  printedIs(UnsafeBufferPointer(start: nullUP, count: 0),
      "UnsafeBufferPointer(start: \(expectedNull), length: 0)")
  printedIs(UnsafeMutableBufferPointer(start: nullUP, count: 0),
      "UnsafeMutableBufferPointer(start: \(expectedNull), length: 0)")

  printedIs(COpaquePointer(), expectedNull)
  printedIs(CFunctionPointer<() -> ()>(), expectedNull)
  printedIs(CVaListPointer(_fromUnsafeMutablePointer: nullUP), expectedNull)
  printedIs(AutoreleasingUnsafeMutablePointer<Int>(), expectedNull)

  println("test_PointerPrinting done")
}
test_PointerPrinting()
// CHECK: test_PointerPrinting done


protocol ProtocolUnrelatedToPrinting {}

struct StructPrintable : Printable, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "►\(x)◀︎"
  }
}

struct LargeStructPrintable : Printable, ProtocolUnrelatedToPrinting {
  let a: Int
  let b: Int
  let c: Int
  let d: Int

  init(_ a: Int, _ b: Int, _ c: Int, _ d: Int) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }

  var description: String {
    return "<\(a) \(b) \(c) \(d)>"
  }
}

struct StructDebugPrintable : DebugPrintable {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var debugDescription: String {
    return "►\(x)◀︎"
  }
}

struct StructVeryPrintable : Printable, DebugPrintable, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "<description: \(x)>"
  }

  var debugDescription: String {
    return "<debugDescription: \(x)>"
  }
}

struct WithoutDescription {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }
}

class ClassPrintable : Printable, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "►\(x)◀︎"
  }
}

class ClassVeryPrintable : Printable, DebugPrintable, ProtocolUnrelatedToPrinting {
  let x: Int

  init(_ x: Int) {
    self.x = x
  }

  var description: String {
    return "<description: \(x)>"
  }

  var debugDescription: String {
    return "<debugDescription: \(x)>"
  }
}

func test_ObjectPrinting() {
  if true {
    let s = StructPrintable(1)
    printedIs(s, "►1◀︎")
  }
  if true {
    let s: ProtocolUnrelatedToPrinting = StructPrintable(1)
    printedIs(s, "►1◀︎")
  }
  if true {
    let s: Printable = StructPrintable(1)
    printedIs(s, "►1◀︎")
  }
  if true {
    let s: Any = StructPrintable(1)
    printedIs(s, "►1◀︎")
  }

  if true {
    let s = LargeStructPrintable(10, 20, 30, 40)
    printedIs(s, "<10 20 30 40>")
  }
  if true {
    let s: ProtocolUnrelatedToPrinting = LargeStructPrintable(10, 20, 30, 40)
    printedIs(s, "<10 20 30 40>")
  }
  if true {
    let s: Printable = LargeStructPrintable(10, 20, 30, 40)
    printedIs(s, "<10 20 30 40>")
  }
  if true {
    let s: Any = LargeStructPrintable(10, 20, 30, 40)
    printedIs(s, "<10 20 30 40>")
  }

  if true {
    let s = StructVeryPrintable(1)
    printedIs(s, "<description: 1>")
  }
  if true {
    let s: ProtocolUnrelatedToPrinting = StructVeryPrintable(1)
    printedIs(s, "<description: 1>")
  }
  if true {
    let s: Printable = StructVeryPrintable(1)
    printedIs(s, "<description: 1>")
  }
  if true {
    let s: DebugPrintable = StructVeryPrintable(1)
    printedIs(s, "<description: 1>")
  }
  if true {
    let s: Any = StructVeryPrintable(1)
    printedIs(s, "<description: 1>")
  }

  if true {
    let c = ClassPrintable(1)
    printedIs(c, "►1◀︎")
  }
  if true {
    let c: ProtocolUnrelatedToPrinting = ClassPrintable(1)
    printedIs(c, "►1◀︎")
  }
  if true {
    let c: Printable = ClassPrintable(1)
    printedIs(c, "►1◀︎")
  }
  if true {
    let c: Any = ClassPrintable(1)
    printedIs(c, "►1◀︎")
  }

  if true {
    let c = ClassVeryPrintable(1)
    printedIs(c, "<description: 1>")
  }
  if true {
    let c: ProtocolUnrelatedToPrinting = ClassVeryPrintable(1)
    printedIs(c, "<description: 1>")
  }
  if true {
    let c: Printable = ClassVeryPrintable(1)
    printedIs(c, "<description: 1>")
  }
  if true {
    let c: DebugPrintable = ClassVeryPrintable(1)
    printedIs(c, "<description: 1>")
  }
  if true {
    let c: Any = ClassVeryPrintable(1)
    printedIs(c, "<description: 1>")
  }

  println("test_ObjectPrinting done")
}
test_ObjectPrinting()
// CHECK: test_ObjectPrinting done

func test_ThickMetatypePrintingImpl<T>(thickMetatype: T.Type, expected: String) {
  printedIs(thickMetatype, expected)
  printedIs([ thickMetatype ], "[" + expected + "]")
}

func test_gcMetatypePrinting() {
  let structMetatype = StructPrintable.self
  printedIs(structMetatype, "a.StructPrintable")
  printedIs([ structMetatype ], "[a.StructPrintable]")
  test_ThickMetatypePrintingImpl(structMetatype, "a.StructPrintable")

  let classMetatype = ClassPrintable.self
  printedIs(classMetatype, "a.ClassPrintable")
  printedIs([ classMetatype ], "[a.ClassPrintable]")
  test_ThickMetatypePrintingImpl(classMetatype, "a.ClassPrintable")

  println("test_gcMetatypePrinting done")
}
test_gcMetatypePrinting()
// CHECK: test_gcMetatypePrinting done

func test_ArrayPrinting() {
  var arrayOfInts: [Int] = []
  printedIs(arrayOfInts, "[]")

  printedIs([ 1 ], "[1]")
  printedIs([ 1, 2 ], "[1, 2]")
  printedIs([ 1, 2, 3 ], "[1, 2, 3]")

  var arrayOfStrings = [ "foo", "bar", "bas" ]
  printedIs([ "foo", "bar", "bas" ], "[foo, bar, bas]")
  debugPrintedIs([ "foo", "bar", "bas" ], "[\"foo\", \"bar\", \"bas\"]")

  printedIs([ StructPrintable(1), StructPrintable(2),
              StructPrintable(3) ],
            "[►1◀︎, ►2◀︎, ►3◀︎]")

  printedIs([ LargeStructPrintable(10, 20, 30, 40),
              LargeStructPrintable(50, 60, 70, 80) ],
            "[<10 20 30 40>, <50 60 70 80>]")

  printedIs([ StructDebugPrintable(1) ], "[►1◀︎]")

  printedIs([ ClassPrintable(1), ClassPrintable(2),
              ClassPrintable(3) ],
            "[►1◀︎, ►2◀︎, ►3◀︎]")

  printedIs([ ClassPrintable(1), ClassPrintable(2),
              ClassPrintable(3) ] as Array<AnyObject>,
            "[►1◀︎, ►2◀︎, ►3◀︎]")

  println("test_ArrayPrinting done")
}
test_ArrayPrinting()
// CHECK: test_ArrayPrinting done

func test_DictionaryPrinting() {
  var dictSI: Dictionary<String, Int> = [:]
  printedIs(dictSI, "[:]")
  debugPrintedIs(dictSI, "[:]")

  dictSI = [ "aaa": 1 ]
  printedIs(dictSI, "[aaa: 1]")
  debugPrintedIs(dictSI, "[\"aaa\": 1]")

  dictSI = [ "aaa": 1, "bbb": 2 ]
  printedIs(dictSI, "[aaa: 1, bbb: 2]", expected2: "[bbb: 2, aaa: 1]")
  debugPrintedIs(dictSI, "[\"aaa\": 1, \"bbb\": 2]", expected2: "[\"bbb\": 2, \"aaa\": 1]")

  var dictSS = [ "aaa": "bbb" ]
  printedIs(dictSS, "[aaa: bbb]")
  debugPrintedIs(dictSS, "[\"aaa\": \"bbb\"]")

  println("test_DictionaryPrinting done")
}
test_DictionaryPrinting()
// CHECK: test_DictionaryPrinting done

func test_SetPrinting() {
  var sI = Set<Int>()
  printedIs(sI, "[]")
  debugPrintedIs(sI, "Set([])")

  sI = Set<Int>([11, 22])
  printedIs(sI, "[11, 22]", expected2: "[22, 11]")
  debugPrintedIs(sI, "Set([11, 22])", expected2: "Set([22, 11])")

  let sS = Set<String>(["Hello", "world"])
  printedIs(sS, "[\"Hello\", \"world\"]", expected2: "[\"world\", \"Hello\"]")
  debugPrintedIs(sS, "Set([\"Hello\", \"world\"])", expected2: "Set([\"world\", \"Hello\"])")

  println("test_SetPrinting done")
}
test_SetPrinting()
// CHECK: test_SetPrinting done

func test_TuplePrinting() {
  var tuple1 = (42, ())
  printedIs(tuple1, "(42, ())")

  var tuple2 = ((), 42)
  printedIs(tuple2, "((), 42)")

  var tuple3 = (42, StructPrintable(3))
  printedIs(tuple3, "(42, ►3◀︎)")

  var tuple4 = (42, LargeStructPrintable(10, 20, 30, 40))
  printedIs(tuple4, "(42, <10 20 30 40>)")

  var tuple5 = (42, ClassPrintable(3))
  printedIs(tuple5, "(42, ►3◀︎)")

  var tuple6 = ([123: 123], (1, 2, "3"))
  printedIs(tuple6, "([123: 123], (1, 2, 3))")

  var arrayOfTuples1 =
      [ (1, "two", StructPrintable(3), StructDebugPrintable(4),
         WithoutDescription(5)) ]
  printedIs(arrayOfTuples1, "[(1, two, ►3◀︎, ►4◀︎, a.WithoutDescription)]")

  var arrayOfTuples2 =
      [ (1, "two", WithoutDescription(3)),
        (11, "twenty-two", WithoutDescription(33)),
        (111, "two hundred twenty-two", WithoutDescription(333)) ]
  printedIs(arrayOfTuples2, "[(1, two, a.WithoutDescription), (11, twenty-two, a.WithoutDescription), (111, two hundred twenty-two, a.WithoutDescription)]")

  println("test_TuplePrinting done")
}
test_TuplePrinting()
// CHECK: test_TuplePrinting done

func test_ArbitraryStructPrinting() {
  var arrayOfArbitraryStructs =
      [ WithoutDescription(1), WithoutDescription(2), WithoutDescription(3) ]
  printedIs(arrayOfArbitraryStructs, "[a.WithoutDescription, a.WithoutDescription, a.WithoutDescription]")

  println("test_ArbitraryStructPrinting done")
}
test_ArbitraryStructPrinting()
// CHECK: test_ArbitraryStructPrinting done

func test_MetatypePrinting() {
  printedIs(Int.self, "Swift.Int")

  println("test_MetatypePrinting done")
}
test_MetatypePrinting()
// CHECK: test_MetatypePrinting done

func test_StringInterpolation() {
  assertEquals("1", "\(1)")
  assertEquals("2", "\(1 + 1)")
  assertEquals("aaa1bbb2ccc", "aaa\(1)bbb\(2)ccc")

  assertEquals("1.0", "\(1.0)")
  assertEquals("1.5", "\(1.5)")
  assertEquals("1e-12", "\(1.0 / (1000000000000))")

  assertEquals("inf", "\(1 / 0.0)")
  assertEquals("-inf", "\(-1 / 0.0)")
  assertEquals("nan", "\(0 / 0.0)")

  assertEquals("<[►1◀︎, ►2◀︎, ►3◀︎]>", "<\([ StructPrintable(1), StructPrintable(2), StructPrintable(3) ])>")
  assertEquals("a.WithoutDescription", "\(WithoutDescription(1))")

  println("test_StringInterpolation done")
}
test_StringInterpolation()
// CHECK: test_StringInterpolation done

struct MyString : StringLiteralConvertible, StringInterpolationConvertible {
  init(str: String) {
    value = str
  }

  var value: String

  init(unicodeScalarLiteral value: String) {
    self.init(str: value)
  }

  init(extendedGraphemeClusterLiteral value: String) {
    self.init(str: value)
  }

  init(stringLiteral value: String) {
    self.init(str: value)
  }

  init(stringInterpolation strings: MyString...) {
    var result = ""
    for s in strings {
      result += s.value
    }
    self.init(str: result)
  }

  init<T>(stringInterpolationSegment expr: T) {
    self.init(str: "<segment " + toString(expr) + ">")
  }
}

func test_CustomStringInterpolation() {
  assertEquals("<segment aaa><segment 1><segment bbb>",
               ("aaa\(1)bbb" as MyString).value)

  println("test_CustomStringInterpolation done")
}
test_CustomStringInterpolation()
// CHECK: test_CustomStringInterpolation done

func test_StdoutUTF8Printing() {
  println("\u{00B5}")
// CHECK: {{^}}µ{{$}}

  println("test_StdoutUTF8Printing done")
}
test_StdoutUTF8Printing()
// CHECK: test_StdoutUTF8Printing done

if !failed {
  println("OK")
}
// CHECK: OK

