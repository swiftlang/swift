// RUN: %target-run-simple-swift | FileCheck %s

import StdlibUnittest
import Foundation

var StringTests = TestCase("StringTests")

StringTests.test("sizeof") {
  expectEqual(3 * sizeof(Int.self), sizeof(String.self))
}

func checkUnicodeScalarViewIteration(
    expectedScalars: [UInt32], str: String
) {
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
    expectEqual(expectedScalars, decoded)
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
    expectEqual(expectedScalars, decoded)
  }
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

StringTests.test("hasPrefix") {
  expectFalse("".hasPrefix(""))
  expectFalse("".hasPrefix("a"))
  expectFalse("a".hasPrefix(""))
  expectTrue("a".hasPrefix("a"))

  // U+0301 COMBINING ACUTE ACCENT
  // U+00E1 LATIN SMALL LETTER A WITH ACUTE
  expectFalse("abc".hasPrefix("a\u{0301}"))
  expectFalse("a\u{0301}bc".hasPrefix("a"))
  expectTrue("\u{00e1}bc".hasPrefix("a\u{0301}"))
  expectTrue("a\u{0301}bc".hasPrefix("\u{00e1}"))
}


extension String {
  var bufferID: UWord {
    return unsafeBitCast(_core._owner, UWord.self)
  }
}

StringTests.test("appendToSubstring") {
  for initialSize in 1..<16 {
    for sliceStart in [ 0, 2, 8, initialSize ] {
      for sliceEnd in [ 0, 2, 8, sliceStart + 1 ] {
        if sliceStart > initialSize || sliceEnd > initialSize ||
          sliceEnd < sliceStart {
          continue
        }
        var s0 = String(count: initialSize, repeatedValue: UnicodeScalar("x"))
        let originalIdentity = s0.bufferID
        s0 = s0[
          advance(s0.startIndex, sliceStart)..<advance(s0.startIndex, sliceEnd)]
        expectEqual(originalIdentity, s0.bufferID)
        s0 += "x"
        // For a small string size, the allocator could round up the allocation
        // and we could get some unused capacity in the buffer.  In that case,
        // the identity would not change.
        if sliceEnd != initialSize {
          expectNotEqual(originalIdentity, s0.bufferID)
        }
        expectEqual(
          String(
            count: sliceEnd - sliceStart + 1,
            repeatedValue: UnicodeScalar("x")),
          s0)
      }
    }
  }
}

StringTests.test("appendToSubstringBug") {
  // String used to have a heap overflow bug when one attempted to append to a
  // substring that pointed to the end of a string buffer.
  //
  //                           Unused capacity
  //                           VVV
  // String buffer [abcdefghijk   ]
  //                      ^    ^
  //                      +----+
  // Substring -----------+
  //
  // In the example above, there are only three elements of unused capacity.
  // The bug was that the implementation mistakenly assumed 9 elements of
  // unused capacity (length of the prefix "abcdef" plus truly unused elements
  // at the end).

  let size = 1024 * 16
  let suffixSize = 16
  let prefixSize = size - suffixSize
  for i in 1..<10 {
    // We will be overflowing s0 with s1.
    var s0 = String(count: size, repeatedValue: UnicodeScalar("x"))
    let s1 = String(count: prefixSize, repeatedValue: UnicodeScalar("x"))
    let originalIdentity = s0.bufferID

    // Turn s0 into a slice that points to the end.
    s0 = s0[advance(s0.startIndex, prefixSize)..<s0.endIndex]

    // Slicing should not reallocate.
    expectEqual(originalIdentity, s0.bufferID)

    // Overflow.
    s0 += s1

    // We should correctly determine that the storage is too small and
    // reallocate.
    expectNotEqual(originalIdentity, s0.bufferID)

    expectEqual(
      String(
        count: suffixSize + prefixSize,
        repeatedValue: UnicodeScalar("x")), s0)
  }
}

func asciiString<
  S: SequenceType where S.Generator.Element == Character
>(content: S) -> String {
  var s = String()
  s.extend(content)
  expectEqual(1, s._core.elementWidth)
  return s
}

StringTests.test("stringCoreExtensibility") {
  let ascii = UTF16.CodeUnit("X".value)
  let nonAscii = UTF16.CodeUnit("é".value)

  for k in 0..<3 {
    for length in 1..<16 {
      for boundary in 0..<length {
        
        var x = (
            k == 0 ? asciiString("b")
          : k == 1 ? String("b" as NSString)
          : String("b" as NSMutableString)
        )._core

        if k == 0 { expectEqual(1, x.elementWidth) }
        
        for i in 0..<length {
          x.extend(
            Repeat(count: 3, repeatedValue: i < boundary ? ascii : nonAscii))
        }
        // Make sure we can extend wide storage with pure ASCII
        x.extend(Repeat(count: 2, repeatedValue: ascii))
        
        expectEqualSequence(
          [UTF16.CodeUnit("b".value)]
          + Array(Repeat(count: 3*boundary, repeatedValue: ascii))
          + Repeat(count: 3*(length - boundary), repeatedValue: nonAscii)
          + Repeat(count: 2, repeatedValue: ascii),
          x
        )
      }
    }
  }
}

StringTests.test("stringCoreReserve") {
  for k in 0...5 {
    var base: String
    var startedNative: Bool
    let shared: String = "X"

    switch k {
    case 0: (base, startedNative) = (String(), true)
    case 1: (base, startedNative) = (asciiString("x"), true)
    case 2: (base, startedNative) = ("Ξ", true)
    case 3: (base, startedNative) = ("x" as NSString as String, false)
    case 4: (base, startedNative) = ("x" as NSMutableString as String, false)
    case 5: (base, startedNative) = (shared, true)
    default:
      fatalError("case unhandled!")
    }
    expectEqual(!base._core.hasCocoaBuffer, startedNative)
    
    var originalBuffer = base.bufferID
    let startedUnique = startedNative && _isUniquelyReferenced(&originalBuffer)
    
    base._core.reserveCapacity(0)
    // Now it's unique
    
    // If it was already native and unique, no reallocation
    if startedUnique && startedNative {
      expectEqual(originalBuffer, base.bufferID)
    }
    else {
      expectNotEqual(originalBuffer, base.bufferID)
    }

    // Reserving up to the capacity in a unique native buffer is a no-op
    let nativeBuffer = base.bufferID
    let currentCapacity = base._core.nativeBuffer!.capacity
    base._core.reserveCapacity(currentCapacity)
    expectEqual(nativeBuffer, base.bufferID)

    // Reserving more capacity should reallocate
    base._core.reserveCapacity(currentCapacity + 1)
    expectNotEqual(nativeBuffer, base.bufferID)

    // None of this should change the string contents
    var expected: String
    switch k {
    case 0: expected = ""
    case 1,3,4: expected = "x"
    case 2: expected = "Ξ"
    case 5: expected = shared
    default:
      fatalError("case unhandled!")
    }
    expectEqual(expected, base)
  }
}

func makeStringCore(base: String) -> _StringCore {
  var x = _StringCore()
  // make sure some - but not all - replacements will have to grow the buffer
  x.reserveCapacity(base._core.count * 3 / 2)
  x.extend(base._core)
  // In case the core was widened and lost its capacity
  x.reserveCapacity(base._core.count * 3 / 2)
  return x
}

StringTests.test("StringCoreReplace") {
  let narrow = "01234567890"
  let wide = "ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪ"
  for s1 in [narrow, wide] {
    for s2 in [narrow, wide] {
      checkRangeReplaceable(
        { makeStringCore(s1) },
        { makeStringCore(s2 + s2)[0..<$0] }
      )
      checkRangeReplaceable(
        { makeStringCore(s1) },
        { Array(makeStringCore(s2 + s2)[0..<$0]) }
      )
    }
  }
}

StringTests.test("StringReplace") {
  let narrow = "01234567890"
  let wide = "ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪ"
  for s1 in [narrow, wide] {
    for s2 in [narrow, wide] {
      checkRangeReplaceable(
        { String(makeStringCore(s1)) },
        { String(makeStringCore(s2 + s2)[0..<$0]) }
      )
      checkRangeReplaceable(
        { String(makeStringCore(s1)) },
        { Array(String(makeStringCore(s2 + s2)[0..<$0])) }
      )
    }
  }
}

StringTests.test("UnicodeScalarViewReplace") {
  let narrow = "01234567890"
  let wide = "ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪ"
  for s1 in [narrow, wide] {
    for s2 in [narrow, wide] {
      checkRangeReplaceable(
        { String(makeStringCore(s1)).unicodeScalars },
        { String(makeStringCore(s2 + s2)[0..<$0]).unicodeScalars }
      )
      checkRangeReplaceable(
        { String(makeStringCore(s1)).unicodeScalars },
        { Array(String(makeStringCore(s2 + s2)[0..<$0]).unicodeScalars) }
      )
    }
  }
}

StringTests.run()
// CHECK: {{^}}StringTests: All tests passed

func testStringToInt() {
  println("test String to Int")
  // CHECK: test String to Int

  var s1 = "  \t 20ddd"
  var i1 : Optional<Int> = s1.toInt()
  if (i1 == nil) { println("none") } // CHECK-NEXT: none

  if ("".toInt() == nil) { println("empty is none") }   // CHECK-NEXT: empty is none
  if ("+".toInt() == nil) { println("+ is none") }      // CHECK-NEXT: + is none
  if ("-".toInt() == nil) { println("- is none") }      // CHECK-NEXT: - is none
  if ("+20".toInt()! == 20) { println("20") }     // CHECK-NEXT: 20
  if ("0".toInt()! == 0) { println("0") }         // CHECK-NEXT: 0
  if ("-20".toInt()! == -20) { println("-20") }   // CHECK-NEXT: -20
  if ("-cc20".toInt() == nil) { println("none") }       // CHECK-NEXT: none
  if ("  -20".toInt() == nil) { println("none") }       // CHECK-NEXT: none

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
    var is_isnot = str.toInt() != nil ? "is" : "is not"
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
    if ("-\(ui)".toInt()) != nil {
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
    if (String(ui).toInt()) != nil {
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
