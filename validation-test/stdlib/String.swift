// RUN: %target-run-simple-swift

// XFAIL: interpret
// XFAIL: linux

import StdlibUnittest
import Foundation

extension String {
  var bufferID: UWord {
    return unsafeBitCast(_core._owner, UWord.self)
  }
  var nativeCapacity: Int {
    return _core.nativeBuffer!.capacity
  }
  var capacity: Int {
    return _core.nativeBuffer?.capacity ?? 0
  }
}

var StringTests = TestSuite("StringTests")

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

StringTests.test("ForeignIndexes/Valid") {
  // It is actually unclear what the correct behavior is.  This test is just a
  // change detector.
  //
  // <rdar://problem/18037897> Design, document, implement invalidation model
  // for foreign String indexes
  if true {
    let donor = "abcdef"
    let acceptor = "uvwxyz"
    expectEqual("u", acceptor[donor.startIndex])
    expectEqual("wxy",
      acceptor[advance(donor.startIndex, 2)..<advance(donor.startIndex, 5)])
  }
  if true {
    let donor = "abcdef"
    let acceptor = "\u{1f601}\u{1f602}\u{1f603}"
    expectEqual("\u{fffd}", acceptor[donor.startIndex])
    expectEqual("\u{fffd}", acceptor[donor.startIndex.successor()])
    expectEqualUnicodeScalars([ 0xfffd, 0x1f602, 0xfffd ],
      acceptor[advance(donor.startIndex, 1)..<advance(donor.startIndex, 5)])
    expectEqualUnicodeScalars([ 0x1f602, 0xfffd ],
      acceptor[advance(donor.startIndex, 2)..<advance(donor.startIndex, 5)])
  }
}

StringTests.test("ForeignIndexes/UnexpectedCrash")
  .xfail(
    .Custom({ true },
    reason: "<rdar://problem/18029290> String.Index caches the grapheme " +
      "cluster size, but it is not always correct to use"))
  .code {

  let donor = "\u{1f601}\u{1f602}\u{1f603}"
  let acceptor = "abcdef"
  // FIXME: this traps right now when trying to construct Character("ab").
  expectEqual("a", acceptor[donor.startIndex])
}

StringTests.test("ForeignIndexes/subscript(Index)/OutOfBoundsTrap") {
  let donor = "abcdef"
  let acceptor = "uvw"

  expectEqual("u", acceptor[advance(donor.startIndex, 0)])
  expectEqual("v", acceptor[advance(donor.startIndex, 1)])
  expectEqual("w", acceptor[advance(donor.startIndex, 2)])

  expectCrashLater()
  acceptor[advance(donor.startIndex, 3)]
}

StringTests.test("ForeignIndexes/subscript(Range)/OutOfBoundsTrap/1") {
  let donor = "abcdef"
  let acceptor = "uvw"

  expectEqual("uvw", acceptor[donor.startIndex..<advance(donor.startIndex, 3)])

  expectCrashLater()
  acceptor[donor.startIndex..<advance(donor.startIndex, 4)]
}

StringTests.test("ForeignIndexes/subscript(Range)/OutOfBoundsTrap/2") {
  let donor = "abcdef"
  let acceptor = "uvw"

  expectEqual("uvw", acceptor[donor.startIndex..<advance(donor.startIndex, 3)])

  expectCrashLater()
  acceptor[advance(donor.startIndex, 4)..<advance(donor.startIndex, 5)]
}

StringTests.test("ForeignIndexes/replaceRange/OutOfBoundsTrap/1") {
  let donor = "abcdef"
  var acceptor = "uvw"

  acceptor.replaceRange(
    donor.startIndex..<donor.startIndex.successor(), with: "u")
  expectEqual("uvw", acceptor)

  expectCrashLater()
  acceptor.replaceRange(
    donor.startIndex..<advance(donor.startIndex, 4), with: "")
}

StringTests.test("ForeignIndexes/replaceRange/OutOfBoundsTrap/2") {
  let donor = "abcdef"
  var acceptor = "uvw"

  acceptor.replaceRange(
    donor.startIndex..<donor.startIndex.successor(), with: "u")
  expectEqual("uvw", acceptor)

  expectCrashLater()
  acceptor.replaceRange(
    advance(donor.startIndex, 4)..<advance(donor.startIndex, 5), with: "")
}

StringTests.test("ForeignIndexes/removeAtIndex/OutOfBoundsTrap") {
  if true {
    let donor = "abcdef"
    var acceptor = "uvw"

    let removed = acceptor.removeAtIndex(donor.startIndex)
    expectEqual("u", removed)
    expectEqual("vw", acceptor)
  }

  let donor = "abcdef"
  var acceptor = "uvw"

  expectCrashLater()
  acceptor.removeAtIndex(advance(donor.startIndex, 4))
}

StringTests.test("ForeignIndexes/removeRange/OutOfBoundsTrap/1") {
  if true {
    let donor = "abcdef"
    var acceptor = "uvw"

    acceptor.removeRange(
      donor.startIndex..<donor.startIndex.successor())
    expectEqual("vw", acceptor)
  }

  let donor = "abcdef"
  var acceptor = "uvw"

  expectCrashLater()
  acceptor.removeRange(
    donor.startIndex..<advance(donor.startIndex, 4))
}

StringTests.test("ForeignIndexes/removeRange/OutOfBoundsTrap/2") {
  let donor = "abcdef"
  var acceptor = "uvw"

  expectCrashLater()
  acceptor.removeRange(
    advance(donor.startIndex, 4)..<advance(donor.startIndex, 5))
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

StringTests.test("literalConcatenation") {
  if true {
    // UnicodeScalarLiteral + UnicodeScalarLiteral
    var s = "1" + "2"
    expectType(String.self, &s)
    expectEqual("12", s)
  }
  if true {
    // UnicodeScalarLiteral + ExtendedGraphemeClusterLiteral
    var s = "1" + "a\u{0301}"
    expectType(String.self, &s)
    expectEqual("1a\u{0301}", s)
  }
  if true {
    // UnicodeScalarLiteral + StringLiteral
    var s = "1" + "xyz"
    expectType(String.self, &s)
    expectEqual("1xyz", s)
  }

  if true {
    // ExtendedGraphemeClusterLiteral + UnicodeScalar
    var s = "a\u{0301}" + "z"
    expectType(String.self, &s)
    expectEqual("a\u{0301}z", s)
  }
  if true {
    // ExtendedGraphemeClusterLiteral + ExtendedGraphemeClusterLiteral
    var s = "a\u{0301}" + "e\u{0302}"
    expectType(String.self, &s)
    expectEqual("a\u{0301}e\u{0302}", s)
  }
  if true {
    // ExtendedGraphemeClusterLiteral + StringLiteral
    var s = "a\u{0301}" + "xyz"
    expectType(String.self, &s)
    expectEqual("a\u{0301}xyz", s)
  }

  if true {
    // StringLiteral + UnicodeScalar
    var s = "xyz" + "1"
    expectType(String.self, &s)
    expectEqual("xyz1", s)
  }
  if true {
    // StringLiteral + ExtendedGraphemeClusterLiteral
    var s = "xyz" + "a\u{0301}"
    expectType(String.self, &s)
    expectEqual("xyza\u{0301}", s)
  }
  if true {
    // StringLiteral + StringLiteral
    var s = "xyz" + "abc"
    expectType(String.self, &s)
    expectEqual("xyzabc", s)
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

StringTests.test("COW/removeRange/start") {
  var str = "12345678"
  let literalIdentity = str.bufferID

  // Check literal-to-heap reallocation.
  if true {
    let slice = str
    expectEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("12345678", str)
    expectEqual("12345678", slice)

    // This mutation should reallocate the string.
    str.removeRange(str.startIndex..<advance(str.startIndex, 1))
    expectNotEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("2345678", str)
    expectEqual("12345678", slice)

    // No more reallocations are expected.
    str.removeRange(str.startIndex..<advance(str.startIndex, 1))
    // FIXME: extra reallocation, should be expectEqual()
    expectNotEqual(heapStrIdentity, str.bufferID)
    // end FIXME
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("345678", str)
    expectEqual("12345678", slice)
  }

  // Check heap-to-heap reallocation.
  expectEqual("345678", str)
  if true {
    let heapStrIdentity1 = str.bufferID

    let slice = str
    expectEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("345678", str)
    expectEqual("345678", slice)

    // This mutation should reallocate the string.
    str.removeRange(str.startIndex..<advance(str.startIndex, 1))
    expectNotEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    let heapStrIdentity2 = str.bufferID
    expectEqual("45678", str)
    expectEqual("345678", slice)

    // No more reallocations are expected.
    str.removeRange(str.startIndex..<advance(str.startIndex, 1))
    // FIXME: extra reallocation, should be expectEqual()
    expectNotEqual(heapStrIdentity2, str.bufferID)
    // end FIXME
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("5678", str)
    expectEqual("345678", slice)
  }
}

StringTests.test("COW/removeRange/end") {
  var str = "12345678"
  let literalIdentity = str.bufferID

  // Check literal-to-heap reallocation.
  expectEqual("12345678", str)
  if true {
    let slice = str
    expectEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("12345678", str)
    expectEqual("12345678", slice)

    // This mutation should reallocate the string.
    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    expectNotEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("1234567", str)
    expectEqual("12345678", slice)

    // No more reallocations are expected.
    str.append(UnicodeScalar("x"))
    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    // FIXME: extra reallocation, should be expectEqual()
    expectNotEqual(heapStrIdentity, str.bufferID)
    // end FIXME
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("1234567", str)
    expectEqual("12345678", slice)

    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    str.append(UnicodeScalar("x"))
    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    // FIXME: extra reallocation, should be expectEqual()
    //expectNotEqual(heapStrIdentity, str.bufferID)
    // end FIXME
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("123456", str)
    expectEqual("12345678", slice)
  }

  // Check heap-to-heap reallocation.
  expectEqual("123456", str)
  if true {
    let heapStrIdentity1 = str.bufferID

    let slice = str
    expectEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("123456", str)
    expectEqual("123456", slice)

    // This mutation should reallocate the string.
    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    expectNotEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("12345", str)
    expectEqual("123456", slice)

    // No more reallocations are expected.
    str.append(UnicodeScalar("x"))
    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    // FIXME: extra reallocation, should be expectEqual()
    expectNotEqual(heapStrIdentity, str.bufferID)
    // end FIXME
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("12345", str)
    expectEqual("123456", slice)

    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    str.append(UnicodeScalar("x"))
    str.removeRange(advance(str.endIndex, -1)..<str.endIndex)
    // FIXME: extra reallocation, should be expectEqual()
    //expectNotEqual(heapStrIdentity, str.bufferID)
    // end FIXME
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("1234", str)
    expectEqual("123456", slice)
  }
}

StringTests.test("COW/replaceRange/end") {
  // Check literal-to-heap reallocation.
  if true {
    var str = "12345678"
    let literalIdentity = str.bufferID

    var slice = str[str.startIndex..<advance(str.startIndex, 7)]
    expectEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("12345678", str)
    expectEqual("1234567", slice)

    // This mutation should reallocate the string.
    slice.replaceRange(slice.endIndex..<slice.endIndex, with: "a")
    expectNotEqual(literalIdentity, slice.bufferID)
    expectEqual(literalIdentity, str.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("1234567a", slice)
    expectEqual("12345678", str)

    // No more reallocations are expected.
    slice.replaceRange(advance(slice.endIndex, -1)..<slice.endIndex, with: "b")
    // FIXME: extra reallocation, should be expectEqual()
    expectNotEqual(heapStrIdentity, slice.bufferID)
    // end FIXME
    expectEqual(literalIdentity, str.bufferID)

    expectEqual("1234567b", slice)
    expectEqual("12345678", str)
  }

  // Check literal-to-heap reallocation.
  if true {
    var str = "12345678"
    let literalIdentity = str.bufferID

    // Move the string to the heap.
    str.reserveCapacity(32)
    expectNotEqual(literalIdentity, str.bufferID)
    let heapStrIdentity1 = str.bufferID

    var slice = str[str.startIndex..<advance(str.startIndex, 7)]
    expectEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)

    // This mutation should reallocate the string.
    slice.replaceRange(slice.endIndex..<slice.endIndex, with: "a")
    expectNotEqual(heapStrIdentity1, slice.bufferID)
    expectEqual(heapStrIdentity1, str.bufferID)
    let heapStrIdentity2 = slice.bufferID
    expectEqual("1234567a", slice)
    expectEqual("12345678", str)

    // No more reallocations are expected.
    slice.replaceRange(advance(slice.endIndex, -1)..<slice.endIndex, with: "b")
    // FIXME: extra reallocation, should be expectEqual()
    expectNotEqual(heapStrIdentity2, slice.bufferID)
    // end FIXME
    expectEqual(heapStrIdentity1, str.bufferID)

    expectEqual("1234567b", slice)
    expectEqual("12345678", str)
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
  let ascii = UTF16.CodeUnit(UnicodeScalar("X").value)
  let nonAscii = UTF16.CodeUnit(UnicodeScalar("é").value)

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
          [UTF16.CodeUnit(UnicodeScalar("b").value)]
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
    let startedUnique = startedNative && base._core._owner != nil
      && isUniquelyReferencedNonObjC(&base._core._owner!)
    
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
    let currentCapacity = base.capacity
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

StringTests.test("reserveCapacity") {
  var s = ""
  let id0 = s.bufferID
  let oldCap = s.capacity
  let x: Character = "x" // Help the typechecker - <rdar://problem/17128913>
  s.splice(Repeat(count: s.capacity + 1, repeatedValue: x), atIndex: s.endIndex)
  expectNotEqual(id0, s.bufferID)
  s = ""
  println("empty capacity \(s.capacity)")
  s.reserveCapacity(oldCap + 2)
  println("reserving \(oldCap + 2) -> \(s.capacity), width = \(s._core.elementWidth)")
  let id1 = s.bufferID
  s.splice(Repeat(count: oldCap + 2, repeatedValue: x), atIndex: s.endIndex)
  println("extending by \(oldCap + 2) -> \(s.capacity), width = \(s._core.elementWidth)")
  expectEqual(id1, s.bufferID)
  s.splice(Repeat(count: s.capacity + 100, repeatedValue: x), atIndex: s.endIndex)
  expectNotEqual(id1, s.bufferID)
}

StringTests.test("toInt") {
  expectEmpty("".toInt())
  expectEmpty("+".toInt())
  expectEmpty("-".toInt())
  expectOptionalEqual(20, "+20".toInt())
  expectOptionalEqual(0, "0".toInt())
  expectOptionalEqual(-20, "-20".toInt())
  expectEmpty("-cc20".toInt())
  expectEmpty("  -20".toInt())
  expectEmpty("  \t 20ddd".toInt())

  expectOptionalEqual(Int.min, "\(Int.min)".toInt())
  expectOptionalEqual(Int.min + 1, "\(Int.min + 1)".toInt())
  expectOptionalEqual(Int.max, "\(Int.max)".toInt())
  expectOptionalEqual(Int.max - 1, "\(Int.max - 1)".toInt())

  expectEmpty("\(Int.min)0".toInt())
  expectEmpty("\(Int.max)0".toInt())

  // Make a String from an Int, mangle the String's characters,
  // then print if the new String is or is not still an Int.
  func testConvertabilityOfStringWithModification(
    initialValue: Int,
    modification: (inout chars: [UTF8.CodeUnit]) -> () )
  {
    var chars = Array(String(initialValue).utf8)
    modification(chars: &chars)
    var str = String._fromWellFormedCodeUnitSequence(UTF8.self, input: chars)
    expectEmpty(str.toInt())
  }

  testConvertabilityOfStringWithModification(Int.min) {
    $0[2]++; ()  // underflow by lots
  }

  testConvertabilityOfStringWithModification(Int.max) {
    $0[1]++; ()  // overflow by lots
  }

  // Test values lower than min.
  if true {
    let base = UInt(Int.max)
    expectOptionalEqual(Int.min + 1, "-\(base)".toInt())
    expectOptionalEqual(Int.min, "-\(base + 1)".toInt())
    for i in 2..<20 {
      expectEmpty("-\(base + UInt(i))".toInt())
    }
  }

  // Test values greater than min.
  if true {
    let base = UInt(Int.max)
    for i in UInt(0)..<20 {
      expectOptionalEqual(-Int(base - i) , "-\(base - i)".toInt())
    }
  }

  // Test values greater than max.
  if true {
    let base = UInt(Int.max)
    expectOptionalEqual(Int.max, "\(base)".toInt())
    for i in 1..<20 {
      expectEmpty("\(base + UInt(i))".toInt())
    }
  }

  // Test values lower than max.
  if true {
    let base = Int.max
    for i in 0..<20 {
      expectOptionalEqual(base - i, "\(base - i)".toInt())
    }
  }
}

// Make sure strings don't grow unreasonably quickly when appended-to
StringTests.test("growth") {
  var s = ""
  var s2 = s

  for i in 0..<20 {
    s += "x"
    s2 = s
  }
  expectLE(s.nativeCapacity, 34)
}

StringTests.test("Construction") {
  let text = "Thirsty pirates"
  expectEqual(text, String(Array<Character>(text)))
}

StringTests.test("Conversions") {
  if true {
    var c: Character = "a"
    let x = String(c)
    expectTrue(x._core.isASCII)

    var s: String = "a"
    expectEqual(s, x)
  }

  if true {
    var c: Character = "\u{B977}"
    let x = String(c)
    expectFalse(x._core.isASCII)

    var s: String = "\u{B977}"
    expectEqual(s, x)
  }
}

// Check the internal functions are correct for ASCII values
StringTests.test(
  "forall x: Int8, y: Int8 . x < 128 ==> x <ascii y == x <unicode y") {
  let asciiDomain = (0..<128).map({ String(UnicodeScalar($0)) })
  expectEqualMethodsForDomain(
    asciiDomain, asciiDomain, 
    String._compareDeterministicUnicodeCollation, String._compareASCII)
}

StringTests.test("lowercaseString") {
  // Use setlocale so tolower() is correct on ASCII.
  setlocale(LC_ALL, "C")

  // Check the ASCII domain.
  let asciiDomain: [Int32] = Array(0..<128)
  expectEqualFunctionsForDomain(
    asciiDomain,
    { String(UnicodeScalar(Int(tolower($0)))) },
    { String(UnicodeScalar(Int($0))).lowercaseString })

  expectEqual("", "".lowercaseString)
  expectEqual("abcd", "abCD".lowercaseString)
  expectEqual("абвг", "абВГ".lowercaseString)
  expectEqual("たちつてと", "たちつてと".lowercaseString)

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0130}".lowercaseString)

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0049}\u{0307}".lowercaseString)
}

StringTests.test("uppercaseString") {
  // Use setlocale so toupper() is correct on ASCII.
  setlocale(LC_ALL, "C")

  // Check the ASCII domain.
  let asciiDomain: [Int32] = Array(0..<128)
  expectEqualFunctionsForDomain(
    asciiDomain,
    { String(UnicodeScalar(Int(toupper($0)))) },
    { String(UnicodeScalar(Int($0))).uppercaseString })

  expectEqual("", "".uppercaseString)
  expectEqual("ABCD", "abCD".uppercaseString)
  expectEqual("АБВГ", "абВГ".uppercaseString)
  expectEqual("たちつてと", "たちつてと".uppercaseString)

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectEqual("\u{0049}", "\u{0069}".uppercaseString)

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectEqual("\u{0053}\u{0053}", "\u{00df}".uppercaseString)

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectEqual("\u{0046}\u{0049}", "\u{fb01}".uppercaseString)
}

StringTests.test("unicodeViews") {
  // Check the UTF views work with slicing

  // U+FFFD REPLACEMENT CHARACTER
  // U+1F3C2 SNOWBOARDER
  // U+2603 SNOWMAN
  let winter = "\u{1F3C2}\u{2603}"

  // slices
  // It is 4 bytes long, so it should return a replacement character.
  expectEqual(
    "\u{FFFD}", String(
      winter.utf8[
        winter.utf8.startIndex..<winter.utf8.startIndex.successor().successor()
      ]))
  
  expectEqual(
    "\u{1F3C2}", String(
      winter.utf8[winter.utf8.startIndex..<advance(winter.utf8.startIndex, 4)]))

  expectEqual(
    "\u{1F3C2}", String(
      winter.utf16[
        winter.utf16.startIndex..<advance(winter.utf16.startIndex, 2)
      ]))
  
  expectEqual(
    "\u{1F3C2}", String(
      winter.unicodeScalars[
        winter.unicodeScalars.startIndex
        ..< winter.unicodeScalars.startIndex.successor()
      ]))

  // views
  expectEqual(
    winter, String(
      winter.utf8[
        winter.utf8.startIndex..<advance(winter.utf8.startIndex, 7)
      ]))
  
  expectEqual(
    winter, String(
      winter.utf16[
        winter.utf16.startIndex..<advance(winter.utf16.startIndex, 3)
      ]))
  
  expectEqual(
    winter, String(
      winter.unicodeScalars[
        winter.unicodeScalars.startIndex
        ..< advance(winter.unicodeScalars.startIndex, 2)
      ]))

  let ga = "\u{304b}\u{3099}"
  expectEqual(ga, String(ga.utf8[ga.utf8.startIndex ..<
    advance(ga.utf8.startIndex, 6)]))
}

// Validate that index conversion does something useful for Cocoa
// programmers.
StringTests.test("indexConversion") {
  var err: NSError? = nil

  let re = NSRegularExpression(
    pattern: "([^ ]+)er", options: NSRegularExpressionOptions(), error: &err)!

  let s = "go further into the larder to barter."

  var matches: [String] = []
  
  re.enumerateMatchesInString(
    s, options: NSMatchingOptions(), range: NSRange(0..<count(s.utf16))
  ) {
    result, flags, stop
  in
    let r = result!.rangeAtIndex(1)
    let start = String.UTF16Index(r.location)
    let end = String.UTF16Index(r.location + r.length)
    matches.append(String(s.utf16[start..<end])!)
  }

  expectEqual(["furth", "lard", "bart"], matches)
}

StringTests.test("String.append(_: UnicodeScalar)") {
  var s = ""

  if true {
    // U+0061 LATIN SMALL LETTER A
    let input: UnicodeScalar = "\u{61}"
    s.append(input)
    expectEqual([ "\u{61}" ], Array(s.unicodeScalars))
  }
  if true {
    // U+304B HIRAGANA LETTER KA
    let input: UnicodeScalar = "\u{304b}"
    s.append(input)
    expectEqual([ "\u{61}", "\u{304b}" ], Array(s.unicodeScalars))
  }
  if true {
    // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
    let input: UnicodeScalar = "\u{3099}"
    s.append(input)
    expectEqual([ "\u{61}", "\u{304b}", "\u{3099}" ], Array(s.unicodeScalars))
  }
  if true {
    // U+1F425 FRONT-FACING BABY CHICK
    let input: UnicodeScalar = "\u{1f425}"
    s.append(input)
    expectEqual(
      [ "\u{61}", "\u{304b}", "\u{3099}", "\u{1f425}" ],
      Array(s.unicodeScalars))
  }
}

StringTests.test("String.append(_: Character)") {
  let baseCharacters: [Character] = [
    // U+0061 LATIN SMALL LETTER A
    "\u{61}",

    // U+304B HIRAGANA LETTER KA
    // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
    "\u{304b}\u{3099}",

    // U+3072 HIRAGANA LETTER HI
    // U+309A COMBINING KATAKANA-HIRAGANA SEMI-VOICED SOUND MARK
    "\u{3072}\u{309A}",

    // U+1F425 FRONT-FACING BABY CHICK
    "\u{1f425}",

    // U+0061 LATIN SMALL LETTER A
    // U+0300 COMBINING GRAVE ACCENT
    // U+0301 COMBINING ACUTE ACCENT
    "\u{61}\u{0300}\u{0301}",

    // U+0061 LATIN SMALL LETTER A
    // U+0300 COMBINING GRAVE ACCENT
    // U+0301 COMBINING ACUTE ACCENT
    // U+0302 COMBINING CIRCUMFLEX ACCENT
    "\u{61}\u{0300}\u{0301}\u{0302}",

    // U+0061 LATIN SMALL LETTER A
    // U+0300 COMBINING GRAVE ACCENT
    // U+0301 COMBINING ACUTE ACCENT
    // U+0302 COMBINING CIRCUMFLEX ACCENT
    // U+0303 COMBINING TILDE
    "\u{61}\u{0300}\u{0301}\u{0302}\u{0303}",
  ]
  let baseStrings = [ "" ] + map(baseCharacters) { String($0) }

  for baseIdx in indices(baseStrings) {
    for prefix in ["", " "] {
      let base = baseStrings[baseIdx]
      for inputIdx in indices(baseCharacters) {
        let input = last(prefix + String(baseCharacters[inputIdx]))!
        var s = base
        s.append(input)
        expectEqual(Array(base) + [ input ], Array(s)) {
          "baseIdx=\(baseIdx) inputIdx=\(inputIdx)"
        }
      }
    }
  }
}

runAllTests()

