// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest

var StringTests = TestCase("StringTests")

StringTests.test("sizeof") {
  expectEqual(3 * sizeof(Int.self), sizeof(String.self))
}

func checkUnicodeScalarViewIteration(
    expectedScalars: [UInt32], str: String) -> AssertionResult {
  if true {
    var us = str.unicodeScalars
    var i = us.startIndex
    var end = us.endIndex
    var decoded: [UInt32] = []
    while i != end {
      expectTrue(i < i.successor()) // Check for Comparable conformance
      decoded.append(us[i].value)
      i = i.successor()
    }
    if expectedScalars != decoded {
      return assertionFailure()
          .withDescription("forward traversal:\n")
          .withDescription("expected: \(asHex(expectedScalars))\n")
          .withDescription("actual:   \(asHex(decoded))")
    }
  }
  if true {
    var us = str.unicodeScalars
    var start = us.startIndex
    var i = us.endIndex
    var decoded: [UInt32] = []
    while i != start {
      i = i.predecessor()
      decoded.append(us[i].value)
    }
    if expectedScalars != decoded {
      return assertionFailure()
          .withDescription("backward traversal:\n")
          .withDescription("expected: \(asHex(expectedScalars))\n")
          .withDescription("actual:   \(asHex(decoded))")
    }
  }

  return assertionSuccess()
}

StringTests.test("unicodeScalars") {
  checkUnicodeScalarViewIteration([], "")
  checkUnicodeScalarViewIteration([ 0x0000 ], "\u{0000}")
  checkUnicodeScalarViewIteration([ 0x0041 ], "A")
  checkUnicodeScalarViewIteration([ 0x007f ], "\u{007f}")
  checkUnicodeScalarViewIteration([ 0x0080 ], "\u{0080}")
  checkUnicodeScalarViewIteration([ 0x07ff ], "\u{07ff}")
  checkUnicodeScalarViewIteration([ 0x0800 ], "\u{0800}")
  checkUnicodeScalarViewIteration([ 0xd7ff ], "\u{d7ff}")
  checkUnicodeScalarViewIteration([ 0x8000 ], "\u{8000}")
  checkUnicodeScalarViewIteration([ 0xe000 ], "\u{e000}")
  checkUnicodeScalarViewIteration([ 0xfffd ], "\u{fffd}")
  checkUnicodeScalarViewIteration([ 0xffff ], "\u{ffff}")
  checkUnicodeScalarViewIteration([ 0x10000 ], "\u{00010000}")
  checkUnicodeScalarViewIteration([ 0x10ffff ], "\u{0010ffff}")
}

StringTests.test("indexComparability") {
  let empty = ""
  expectTrue(empty.startIndex == empty.endIndex)
  expectFalse(empty.startIndex != empty.endIndex)
  expectTrue(empty.startIndex <= empty.endIndex)
  expectTrue(empty.startIndex >= empty.endIndex)
  expectFalse(empty.startIndex > empty.endIndex)
  expectFalse(empty.startIndex < empty.endIndex)

  let nonEmpty = "borkus biqualificated"
  expectFalse(nonEmpty.startIndex == nonEmpty.endIndex)
  expectTrue(nonEmpty.startIndex != nonEmpty.endIndex)
  expectTrue(nonEmpty.startIndex <= nonEmpty.endIndex)
  expectFalse(nonEmpty.startIndex >= nonEmpty.endIndex)
  expectFalse(nonEmpty.startIndex > nonEmpty.endIndex)
  expectTrue(nonEmpty.startIndex < nonEmpty.endIndex)
}

StringTests.test("_splitFirst") {
  var (before, after, found) = "foo.bar"._splitFirst(".")
  expectTrue(found)
  expectEqual("foo", before)
  expectEqual("bar", after)
}

StringTests.run()
// CHECK: {{^}}StringTests: All tests passed

func testStringToInt() {
  println("test String to Int")
  // CHECK: test String to Int

  var s1 = "  \t 20ddd"
  var i1 : Optional<Int> = s1.toInt()
  if (!i1) { println("none") } // CHECK-NEXT: none

  if (!"".toInt()) { println("empty is none") }   // CHECK-NEXT: empty is none
  if (!"+".toInt()) { println("+ is none") }      // CHECK-NEXT: + is none
  if (!"-".toInt()) { println("- is none") }      // CHECK-NEXT: - is none
  if ("+20".toInt()! == 20) { println("20") }     // CHECK-NEXT: 20
  if ("0".toInt()! == 0) { println("0") }         // CHECK-NEXT: 0
  if ("-20".toInt()! == -20) { println("-20") }   // CHECK-NEXT: -20
  if (!"-cc20".toInt()) { println("none") }       // CHECK-NEXT: none
  if (!"  -20".toInt()) { println("none") }       // CHECK-NEXT: none

  if (String(Int.min).toInt()! == Int.min) {
    println("round-trip Int.min")
  }
  // CHECK-NEXT: round-trip Int.min

  if (String(Int.max).toInt()! == Int.max) {
    println("round-trip Int.max")
  }
  // CHECK-NEXT: round-trip Int.max


  // Make a String from an Int, mangle the String's characters, 
  // then print if the new String is or is not still an Int.
  func testConvertabilityOfStringWithModification(
    initialValue: Int, 
    modification: (inout chars: [UTF8.CodeUnit]) -> () ) 
  {
    var chars = Array(String(initialValue).utf8)
    modification(chars: &chars)
    var str = String._fromWellFormedCodeUnitSequence(UTF8.self, input: chars)
    var is_isnot = str.toInt() ? "is" : "is not"
    println("\(str) \(is_isnot) an Int")
  }

  var minChars = String(Int.min).utf8

  testConvertabilityOfStringWithModification(Int.min) { 
    (inout chars: [UTF8.CodeUnit]) in ()
  }
  // CHECK-NEXT: {{-9223372036854775808|-2147483648}} is an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0[$0.count-1]--; ()
  }
  // CHECK-NEXT: {{-9223372036854775807|-2147483647}} is an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0[$0.count-1]++; ()  // underflow by one
  }
  // CHECK-NEXT: {{-9223372036854775809|-2147483649}} is not an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0[2]++; ()  // underflow by lots
  }
  // CHECK-NEXT: {{-9323372036854775808|-2247483648}} is not an Int

  testConvertabilityOfStringWithModification(Int.min) { 
    $0.append(Array("0".utf8)[0]); ()  // underflow by adding digits
  }
  // CHECK-NEXT: {{-92233720368547758080|-21474836480}} is not an Int


  testConvertabilityOfStringWithModification(Int.max) { 
    (inout chars: [UTF8.CodeUnit]) in ()
  }
  // CHECK-NEXT: {{9223372036854775807|2147483647}} is an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0[$0.count-1]--; ()
  }
  // CHECK-NEXT: {{9223372036854775806|2147483646}} is an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0[$0.count-1]++; ()  // overflow by one
  }
  // CHECK-NEXT: {{9223372036854775808|2147483648}} is not an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0[1]++; ()  // overflow by lots
  }
  // CHECK-NEXT: {{9323372036854775807|2247483647}} is not an Int

  testConvertabilityOfStringWithModification(Int.max) { 
    $0.append(Array("0".utf8)[0]); ()  // overflow by adding digits
  }
  // CHECK-NEXT: {{92233720368547758070|21474836470}} is not an Int


  // Test values lower than min.
  var ui = UInt(Int.max) + 1
  for index in 0..<20 {
    ui = ui + UInt(index)
    if ("-\(ui)".toInt()) {
      print(".")
    } else {
      print("*")
    }
  }
  println("lower than min")
  // CHECK-NEXT: .*******************lower than min

  // Test values greater than min.
  ui = UInt(Int.max)
  for index in 0..<20 {
    ui = ui - UInt(index)
    if ("-\(ui)".toInt()! == -Int(ui)) {
      print(".")
    } else {
      print("*")
    }
  }
  println("greater than min")
  // CHECK-NEXT: ....................greater than min

  // Test values greater than max.
  ui = UInt(Int.max)
  for index in 0..<20 {
    ui = ui + UInt(index)
    if (String(ui).toInt()) {
      print(".")
    } else {
      print("*")
    }
  }
  println("greater than max")
  // CHECK-NEXT: .*******************greater than max

  // Test values lower than max.
  ui = UInt(Int.max)
  for index in 0..<20 {
    ui = ui - UInt(index)
    if (String(ui).toInt()! == Int(ui)) {
      print(".")
    } else {
      print("*")
    }
  }
  println("lower than max")
  // CHECK-NEXT: ....................lower than max
}

// Make sure strings don't grow unreasonably quickly when appended-to
func testGrowth() {
  var s = ""
  var s2 = s

  for i in 0..<20 {
    s += "x"
    s2 = s
  }
  // CHECK-NEXT: true
  println(s._core.nativeBuffer!.capacity <= 34)
}

testStringToInt()
testGrowth()

func testCompare() {
  // CHECK: testCompare
  println("testCompare")
  // CHECK: 1
  println("hi".unicodeScalars.compare("bye".unicodeScalars))
  // CHECK: -1
  println("bye".unicodeScalars.compare("hi".unicodeScalars))
  // CHECK: 0
  println("swift".unicodeScalars.compare("swift".unicodeScalars))
  // CHECK: 1
  println("a".unicodeScalars.compare("".unicodeScalars))
  // CHECK: 0
  println("a".unicodeScalars.compare("a".unicodeScalars))
  // CHECK: -1
  println("a".unicodeScalars.compare("z".unicodeScalars))
  // CHECK: 1
  println("aa".unicodeScalars.compare("a".unicodeScalars))
  // CHECK: -1
  println("a".unicodeScalars.compare("aa".unicodeScalars))
  // CHECK: 0
  println("".unicodeScalars.compare("".unicodeScalars))
  // CHECK: -1
  println("a".unicodeScalars.compare("b".unicodeScalars))
  // CHECK: 1
  println("b".unicodeScalars.compare("a".unicodeScalars))
  println("testCompare done")
  // CHECK: testCompare done
}
testCompare()

func testCompareUnicode() {
  // CHECK: testCompareUnicode
  println("testCompareUnicode")
  // CHECK: 1
  println("hi".unicodeScalars.compare("bye".unicodeScalars))
  // CHECK: -1
  println("bye".unicodeScalars.compare("hi".unicodeScalars))
  // CHECK: 0
  println("ראשון".unicodeScalars.compare("ראשון".unicodeScalars))
  // CHECK: 1
  println("א".unicodeScalars.compare("".unicodeScalars))
  // CHECK: 0
  println("א".unicodeScalars.compare("א".unicodeScalars))
  // CHECK: -1
  println("א".unicodeScalars.compare("ת".unicodeScalars))
  // CHECK: 1
  println("אא".unicodeScalars.compare("א".unicodeScalars))
  // CHECK: -1
  println("א".unicodeScalars.compare("אא".unicodeScalars))
  // CHECK: 0
  println("".unicodeScalars.compare("".unicodeScalars))
  // CHECK: -1
  println("א".unicodeScalars.compare("ב".unicodeScalars))
  // CHECK: 1
  println("ב".unicodeScalars.compare("א".unicodeScalars))
  println("testCompareUnicode done")
  // CHECK: testCompareUnicode done
}
testCompareUnicode()

