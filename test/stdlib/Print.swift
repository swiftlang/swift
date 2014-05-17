// RUN: %target-run-simple-swift | FileCheck %s

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

func printedIs<T>(
    object: T, expected: String,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  var actual = toString(object)
  if expected != actual {
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    assert(expected == actual, file: file, line: line)
  }
}

func debugPrintedIs<T>(
    object: T, expected: String,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  var actual = ""
  debugPrint(object, &actual)
  if expected != actual {
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    assert(expected == actual, file: file, line: line)
  }
}

func assertEquals(
    expected: String, actual: String,
    file: StaticString = __FILE__, line: UWord = __LINE__
) {
  if expected != actual {
    println("expected: \"\(expected)\"")
    println("actual: \"\(actual)\"")
    assert(expected == actual, file: file, line: line)
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
  s = "\\ \' \" \0 \n \r \t \x05"
  debugPrintedIs(s, "\"\\\\ \\\' \\\" \\0 \\n \\r \\t \\x05\"")

  var ch: Character = "a"
  printedIs(ch, "a")

  var us: UnicodeScalar = "a"
  printedIs(us, "a")

  var cstr: CString = "abc"
  printedIs(cstr, "abc")
  debugPrintedIs(cstr, "\"abc\"")
  cstr = "\\ \' \" \n \r \t \x05"
  debugPrintedIs(cstr, "\"\\\\ \\\' \\\" \\n \\r \\t \\x05\"")

  var nullCString = CString(UnsafePointer.null())
  printedIs(nullCString, "")
  debugPrintedIs(nullCString, "<null C string>")

  var implicitlyUnwrappedString: String! = nil
  printedIs(implicitlyUnwrappedString, "nil")
  implicitlyUnwrappedString = "meow"
  printedIs(implicitlyUnwrappedString, "meow")

  println("test_StdlibTypesPrinted done")
}
test_StdlibTypesPrinted()
// CHECK: test_StdlibTypesPrinted done

func test_IntegerPrinting() {
  if (Int.max > 0x1_0000_0000) {
    printedIs(Int.min, "-9223372036854775808")
    printedIs(Int.max, "9223372036854775807")
  } else {
    printedIs(Int.min, "-2147483648")
    printedIs(Int.max, "2147483647")
  }
  printedIs(Int(0), "0")
  printedIs(Int(42), "42")
  printedIs(Int(-42), "-42")

  if (UInt.max > 0x1_0000_0000) {
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
  printedIs(Float.inf(), "inf")
  printedIs(-Float.inf(), "-inf")
  printedIs(Float.NaN(), "nan")
  printedIs(Float(0.0), "0.0")
  printedIs(Float(1.0), "1.0")
  printedIs(Float(-1.0), "-1.0")
  printedIs(Float(100.125), "100.125")
  printedIs(Float(-100.125), "-100.125")

  printedIs(Double.inf(), "inf")
  printedIs(-Double.inf(), "-inf")
  printedIs(Double.NaN(), "nan")
  printedIs(Double(0.0), "0.0")
  printedIs(Double(1.0), "1.0")
  printedIs(Double(-1.0), "-1.0")
  printedIs(Double(100.125), "100.125")
  printedIs(Double(-100.125), "-100.125")

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

func test_ArrayPrinting() {
  var arrayOfInts: Int[] = []
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
  printedIs(dictSI, "[aaa: 1, bbb: 2]")
  debugPrintedIs(dictSI, "[\"aaa\": 1, \"bbb\": 2]")

  var dictSS = [ "aaa": "bbb" ]
  printedIs(dictSS, "[aaa: bbb]")
  debugPrintedIs(dictSS, "[\"aaa\": \"bbb\"]")

  println("test_DictionaryPrinting done")
}
test_DictionaryPrinting()
// CHECK: test_DictionaryPrinting done

func test_TuplePrinting() {
  var arrayOfTuples1 =
      [ (1, "two", StructPrintable(3), StructDebugPrintable(4),
         WithoutDescription(5)) ]
  printedIs(arrayOfTuples1, "[(1, two, ►3◀︎, ►4◀︎, V1a18WithoutDescription (has 1 child))]")

  var arrayOfTuples2 =
      [ (1, "two", WithoutDescription(3)),
        (11, "twenty-two", WithoutDescription(33)),
        (111, "two hundred twenty-two", WithoutDescription(333)) ]
  printedIs(arrayOfTuples2, "[(1, two, V1a18WithoutDescription (has 1 child)), (11, twenty-two, V1a18WithoutDescription (has 1 child)), (111, two hundred twenty-two, V1a18WithoutDescription (has 1 child))]")

  println("test_TuplePrinting done")
}
test_TuplePrinting()
// CHECK: test_TuplePrinting done

func test_ArbitraryStructPrinting() {
  var arrayOfArbitraryStructs =
      [ WithoutDescription(1), WithoutDescription(2), WithoutDescription(3) ]
  printedIs(arrayOfArbitraryStructs, "[V1a18WithoutDescription (has 1 child), V1a18WithoutDescription (has 1 child), V1a18WithoutDescription (has 1 child)]")

  println("test_ArbitraryStructPrinting done")
}
test_ArbitraryStructPrinting()
// CHECK: test_ArbitraryStructPrinting done

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
  assertEquals("V1a18WithoutDescription (has 1 child)", "\(WithoutDescription(1))")

  println("test_StringInterpolation done")
}
test_StringInterpolation()
// CHECK: test_StringInterpolation done

struct MyString : StringLiteralConvertible, StringInterpolationConvertible {
  init(str: String) {
    value = str
  }

  var value: String

  static func convertFromExtendedGraphemeClusterLiteral(
      value: String) -> MyString {
    return MyString(str: value)
  }

  static func convertFromStringLiteral(value: String) -> MyString {
    return MyString(str: value)
  }

  static func convertFromStringInterpolation(strings: MyString...) -> MyString {
    var result = ""
    for s in strings {
      result += s.value
    }
    return MyString(str: result)
  }

  static func convertFromStringInterpolationSegment<T>(expr: T) -> MyString {
    return MyString(str: "<segment " + toString(expr) + ">")
  }
}

func test_CustomStringInterpolation() {
  assertEquals("<segment aaa><segment 1><segment bbb>",
               ("aaa\(1)bbb" as MyString).value)

  println("test_CustomStringInterpolation done")
}
test_CustomStringInterpolation()
// CHECK: test_CustomStringInterpolation done


