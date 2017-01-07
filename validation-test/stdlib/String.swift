// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// XFAIL: interpret

import StdlibUnittest
import StdlibCollectionUnittest

#if _runtime(_ObjC)
import Foundation  // For NSRange
#endif

extension Collection {
  internal func index(_nth n: Int) -> Index {
    precondition(n >= 0)
    return index(startIndex, offsetBy: numericCast(n))
  }
  internal func index(_nthLast n: Int) -> Index {
    precondition(n >= 0)
    return index(endIndex, offsetBy: -numericCast(n))
  }
}

extension String {
  internal func index(_nth n: Int) -> Index {
    return characters.index(_nth: n)
  }
  internal func index(_nthLast n: Int) -> Index {
    return characters.index(_nthLast: n)
  }
}

extension String {
  var bufferID: UInt {
    return unsafeBitCast(_core._owner, to: UInt.self)
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
  expectEqual(3 * MemoryLayout<Int>.size, MemoryLayout<String>.size)
}

StringTests.test("AssociatedTypes-UTF8View") {
  typealias View = String.UTF8View
  expectCollectionAssociatedTypes(
    collectionType: View.self,
    iteratorType: IndexingIterator<View>.self,
    subSequenceType: View.self,
    indexType: View.Index.self,
    indexDistanceType: Int.self,
    indicesType: DefaultIndices<View>.self)
}

StringTests.test("AssociatedTypes-UTF16View") {
  typealias View = String.UTF16View
  expectCollectionAssociatedTypes(
    collectionType: View.self,
    iteratorType: IndexingIterator<View>.self,
    subSequenceType: View.self,
    indexType: View.Index.self,
    indexDistanceType: Int.self,
    indicesType: View.Indices.self)
}

StringTests.test("AssociatedTypes-UnicodeScalarView") {
  typealias View = String.UnicodeScalarView
  expectCollectionAssociatedTypes(
    collectionType: View.self,
    iteratorType: View.Iterator.self,
    subSequenceType: View.self,
    indexType: View.Index.self,
    indexDistanceType: Int.self,
    indicesType: DefaultBidirectionalIndices<View>.self)
}

StringTests.test("AssociatedTypes-CharacterView") {
  typealias View = String.CharacterView
  expectCollectionAssociatedTypes(
    collectionType: View.self,
    iteratorType: IndexingIterator<View>.self,
    subSequenceType: View.self,
    indexType: View.Index.self,
    indexDistanceType: Int.self,
    indicesType: DefaultBidirectionalIndices<View>.self)
}

func checkUnicodeScalarViewIteration(
    _ expectedScalars: [UInt32], _ str: String
) {
  do {
    let us = str.unicodeScalars
    var i = us.startIndex
    let end = us.endIndex
    var decoded: [UInt32] = []
    while i != end {
      expectTrue(i < us.index(after: i)) // Check for Comparable conformance
      decoded.append(us[i].value)
      i = us.index(after: i)
    }
    expectEqual(expectedScalars, decoded)
  }
  do {
    let us = str.unicodeScalars
    let start = us.startIndex
    var i = us.endIndex
    var decoded: [UInt32] = []
    while i != start {
      i = us.index(before: i)
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
  do {
    let donor = "abcdef"
    let acceptor = "uvwxyz"
    expectEqual("u", acceptor[donor.startIndex])
    expectEqual("wxy",
      acceptor[donor.index(_nth: 2)..<donor.index(_nth: 5)])
  }
  do {
    let donor = "abcdef"
    let acceptor = "\u{1f601}\u{1f602}\u{1f603}"
    expectEqual("\u{fffd}", acceptor[donor.startIndex])
    expectEqual("\u{fffd}", acceptor[donor.index(after: donor.startIndex)])
    expectEqualUnicodeScalars([ 0xfffd, 0x1f602, 0xfffd ],
      acceptor[donor.index(_nth: 1)..<donor.index(_nth: 5)])
    expectEqualUnicodeScalars([ 0x1f602, 0xfffd ],
      acceptor[donor.index(_nth: 2)..<donor.index(_nth: 5)])
  }
}

StringTests.test("ForeignIndexes/UnexpectedCrash")
  .xfail(
    .always("<rdar://problem/18029290> String.Index caches the grapheme " +
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

  expectEqual("u", acceptor[donor.index(_nth: 0)])
  expectEqual("v", acceptor[donor.index(_nth: 1)])
  expectEqual("w", acceptor[donor.index(_nth: 2)])

  let i = donor.index(_nth: 3)
  expectCrashLater()
  _ = acceptor[i]
}

StringTests.test("String/subscript(_:Range)") {
  let s = "foobar"
  let from = s.startIndex
  let to = s.index(before: s.endIndex)
  let actual = s[from..<to]
  expectEqual("fooba", actual)
}

StringTests.test("String/subscript(_:ClosedRange)") {
  let s = "foobar"
  let from = s.startIndex
  let to = s.index(before: s.endIndex)
  let actual = s[from...to]
  expectEqual(s, actual)
}

StringTests.test("ForeignIndexes/subscript(Range)/OutOfBoundsTrap/1") {
  let donor = "abcdef"
  let acceptor = "uvw"

  expectEqual("uvw", acceptor[donor.startIndex..<donor.index(_nth: 3)])

  let r = donor.startIndex..<donor.index(_nth: 4)
  expectCrashLater()
  _ = acceptor[r]
}

StringTests.test("ForeignIndexes/subscript(Range)/OutOfBoundsTrap/2") {
  let donor = "abcdef"
  let acceptor = "uvw"

  expectEqual("uvw", acceptor[donor.startIndex..<donor.index(_nth: 3)])

  let r = donor.index(_nth: 4)..<donor.index(_nth: 5)
  expectCrashLater()
  _ = acceptor[r]
}

StringTests.test("ForeignIndexes/replaceSubrange/OutOfBoundsTrap/1") {
  let donor = "abcdef"
  var acceptor = "uvw"

  acceptor.replaceSubrange(
    donor.startIndex..<donor.index(_nth: 1), with: "u")
  expectEqual("uvw", acceptor)

  let r = donor.startIndex..<donor.index(_nth: 4)
  expectCrashLater()
  acceptor.replaceSubrange(r, with: "")
}

StringTests.test("ForeignIndexes/replaceSubrange/OutOfBoundsTrap/2") {
  let donor = "abcdef"
  var acceptor = "uvw"

  acceptor.replaceSubrange(
    donor.startIndex..<donor.index(_nth: 1), with: "u")
  expectEqual("uvw", acceptor)

  let r = donor.index(_nth: 4)..<donor.index(_nth: 5)
  expectCrashLater()
  acceptor.replaceSubrange(r, with: "")
}

StringTests.test("ForeignIndexes/removeAt/OutOfBoundsTrap") {
  do {
    let donor = "abcdef"
    var acceptor = "uvw"

    let removed = acceptor.remove(at: donor.startIndex)
    expectEqual("u", removed)
    expectEqual("vw", acceptor)
  }

  let donor = "abcdef"
  var acceptor = "uvw"

  let i = donor.index(_nth: 4)
  expectCrashLater()
  acceptor.remove(at: i)
}

StringTests.test("ForeignIndexes/removeSubrange/OutOfBoundsTrap/1") {
  do {
    let donor = "abcdef"
    var acceptor = "uvw"

    acceptor.removeSubrange(
      donor.startIndex..<donor.index(after: donor.startIndex))
    expectEqual("vw", acceptor)
  }

  let donor = "abcdef"
  var acceptor = "uvw"

  let r = donor.startIndex..<donor.index(_nth: 4)
  expectCrashLater()
  acceptor.removeSubrange(r)
}

StringTests.test("ForeignIndexes/removeSubrange/OutOfBoundsTrap/2") {
  let donor = "abcdef"
  var acceptor = "uvw"

  let r = donor.index(_nth: 4)..<donor.index(_nth: 5)
  expectCrashLater()
  acceptor.removeSubrange(r)
}

StringTests.test("_splitFirst") {
  var (before, after, found) = "foo.bar"._splitFirst(separator: ".")
  expectTrue(found)
  expectEqual("foo", before)
  expectEqual("bar", after)
}

StringTests.test("hasPrefix")
  .skip(.nativeRuntime("String.hasPrefix undefined without _runtime(_ObjC)"))
  .code {
#if _runtime(_ObjC)
  expectTrue("".hasPrefix(""))
  expectFalse("".hasPrefix("a"))
  expectTrue("a".hasPrefix(""))
  expectTrue("a".hasPrefix("a"))

  // U+0301 COMBINING ACUTE ACCENT
  // U+00E1 LATIN SMALL LETTER A WITH ACUTE
  expectFalse("abc".hasPrefix("a\u{0301}"))
  expectFalse("a\u{0301}bc".hasPrefix("a"))
  expectTrue("\u{00e1}bc".hasPrefix("a\u{0301}"))
  expectTrue("a\u{0301}bc".hasPrefix("\u{00e1}"))
#else
  expectUnreachable()
#endif
}

StringTests.test("literalConcatenation") {
  do {
    // UnicodeScalarLiteral + UnicodeScalarLiteral
    var s = "1" + "2"
    expectType(String.self, &s)
    expectEqual("12", s)
  }
  do {
    // UnicodeScalarLiteral + ExtendedGraphemeClusterLiteral
    var s = "1" + "a\u{0301}"
    expectType(String.self, &s)
    expectEqual("1a\u{0301}", s)
  }
  do {
    // UnicodeScalarLiteral + StringLiteral
    var s = "1" + "xyz"
    expectType(String.self, &s)
    expectEqual("1xyz", s)
  }

  do {
    // ExtendedGraphemeClusterLiteral + UnicodeScalar
    var s = "a\u{0301}" + "z"
    expectType(String.self, &s)
    expectEqual("a\u{0301}z", s)
  }
  do {
    // ExtendedGraphemeClusterLiteral + ExtendedGraphemeClusterLiteral
    var s = "a\u{0301}" + "e\u{0302}"
    expectType(String.self, &s)
    expectEqual("a\u{0301}e\u{0302}", s)
  }
  do {
    // ExtendedGraphemeClusterLiteral + StringLiteral
    var s = "a\u{0301}" + "xyz"
    expectType(String.self, &s)
    expectEqual("a\u{0301}xyz", s)
  }

  do {
    // StringLiteral + UnicodeScalar
    var s = "xyz" + "1"
    expectType(String.self, &s)
    expectEqual("xyz1", s)
  }
  do {
    // StringLiteral + ExtendedGraphemeClusterLiteral
    var s = "xyz" + "a\u{0301}"
    expectType(String.self, &s)
    expectEqual("xyza\u{0301}", s)
  }
  do {
    // StringLiteral + StringLiteral
    var s = "xyz" + "abc"
    expectType(String.self, &s)
    expectEqual("xyzabc", s)
  }
}

StringTests.test("appendToSubstring") {
  for initialSize in 1..<16 {
    for sliceStart in [0, 2, 8, initialSize] {
      for sliceEnd in [0, 2, 8, sliceStart + 1] {
        if sliceStart > initialSize || sliceEnd > initialSize ||
          sliceEnd < sliceStart {
          continue
        }
        var s0 = String(repeating: "x", count: initialSize)
        let originalIdentity = s0.bufferID
        s0 = s0[s0.index(_nth: sliceStart)..<s0.index(_nth: sliceEnd)]
        expectEqual(originalIdentity, s0.bufferID)
        s0 += "x"
        // For a small string size, the allocator could round up the allocation
        // and we could get some unused capacity in the buffer.  In that case,
        // the identity would not change.
        if sliceEnd != initialSize {
          if sliceStart != sliceEnd {
            expectNotEqual(originalIdentity, s0.bufferID)
          } else {
            expectEqual(0, s0.bufferID)
          }
        }
        expectEqual(
          String(
            repeating: "x",
            count: sliceEnd - sliceStart + 1),
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
    var s0 = String(repeating: "x", count: size)
    let s1 = String(repeating: "x", count: prefixSize)
    let originalIdentity = s0.bufferID

    // Turn s0 into a slice that points to the end.
    s0 = s0[s0.index(_nth: prefixSize)..<s0.endIndex]

    // Slicing should not reallocate.
    expectEqual(originalIdentity, s0.bufferID)

    let originalCapacity = s0.capacity

    // Overflow.
    s0 += s1

    // We should correctly determine if the storage is too small and
    // reallocate.
    if size + prefixSize >= originalCapacity {
      expectNotEqual(originalIdentity, s0.bufferID)
    } else {
      expectEqual(originalIdentity, s0.bufferID)
    }

    expectEqual(
      String(
        repeating: "x",
        count: suffixSize + prefixSize),
      s0)
  }
}

StringTests.test("COW/removeSubrange/start") {
  var str = "12345678"
  let literalIdentity = str.bufferID

  // Check literal-to-heap reallocation.
  do {
    let slice = str
    expectEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("12345678", str)
    expectEqual("12345678", slice)

    // This mutation should reallocate the string.
    str.removeSubrange(str.startIndex..<str.index(_nth: 1))
    expectNotEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("2345678", str)
    expectEqual("12345678", slice)

    // No more reallocations are expected.
    str.removeSubrange(str.startIndex..<str.index(_nth: 1))
    expectEqual(heapStrIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("345678", str)
    expectEqual("12345678", slice)
  }

  // Check heap-to-heap reallocation.
  expectEqual("345678", str)
  do {
    let heapStrIdentity1 = str.bufferID

    let slice = str
    expectEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("345678", str)
    expectEqual("345678", slice)

    // This mutation should reallocate the string.
    str.removeSubrange(str.startIndex..<str.index(_nth: 1))
    expectNotEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    let heapStrIdentity2 = str.bufferID
    expectEqual("45678", str)
    expectEqual("345678", slice)

    // No more reallocations are expected.
    str.removeSubrange(str.startIndex..<str.index(_nth: 1))
    expectEqual(heapStrIdentity2, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("5678", str)
    expectEqual("345678", slice)
  }
}

StringTests.test("COW/removeSubrange/end") {
  var str = "12345678"
  let literalIdentity = str.bufferID

  // Check literal-to-heap reallocation.
  expectEqual("12345678", str)
  do {
    let slice = str
    expectEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("12345678", str)
    expectEqual("12345678", slice)

    // This mutation should reallocate the string.
    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    expectNotEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("1234567", str)
    expectEqual("12345678", slice)

    // No more reallocations are expected.
    str.append("x")
    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    expectEqual(heapStrIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("1234567", str)
    expectEqual("12345678", slice)

    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    str.append("x")
    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    expectEqual(heapStrIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("123456", str)
    expectEqual("12345678", slice)
  }

  // Check heap-to-heap reallocation.
  expectEqual("123456", str)
  do {
    let heapStrIdentity1 = str.bufferID

    let slice = str
    expectEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("123456", str)
    expectEqual("123456", slice)

    // This mutation should reallocate the string.
    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    expectNotEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    let heapStrIdentity = str.bufferID
    expectEqual("12345", str)
    expectEqual("123456", slice)

    // No more reallocations are expected.
    str.append("x")
    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    expectEqual(heapStrIdentity, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("12345", str)
    expectEqual("123456", slice)

    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    str.append("x")
    str.removeSubrange(str.index(_nthLast: 1)..<str.endIndex)
    expectEqual(heapStrIdentity, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)
    expectEqual("1234", str)
    expectEqual("123456", slice)
  }
}

StringTests.test("COW/replaceSubrange/end") {
  // Check literal-to-heap reallocation.
  do {
    var str = "12345678"
    let literalIdentity = str.bufferID

    var slice = str[str.startIndex..<str.index(_nth: 7)]
    expectEqual(literalIdentity, str.bufferID)
    expectEqual(literalIdentity, slice.bufferID)
    expectEqual("12345678", str)
    expectEqual("1234567", slice)

    // This mutation should reallocate the string.
    slice.replaceSubrange(slice.endIndex..<slice.endIndex, with: "a")
    expectNotEqual(literalIdentity, slice.bufferID)
    expectEqual(literalIdentity, str.bufferID)
    let heapStrIdentity = slice.bufferID
    expectEqual("1234567a", slice)
    expectEqual("12345678", str)

    // No more reallocations are expected.
    slice.replaceSubrange(
      slice.index(_nthLast: 1)..<slice.endIndex, with: "b")
    expectEqual(heapStrIdentity, slice.bufferID)
    expectEqual(literalIdentity, str.bufferID)

    expectEqual("1234567b", slice)
    expectEqual("12345678", str)
  }

  // Check literal-to-heap reallocation.
  do {
    var str = "12345678"
    let literalIdentity = str.bufferID

    // Move the string to the heap.
    str.reserveCapacity(32)
    expectNotEqual(literalIdentity, str.bufferID)
    let heapStrIdentity1 = str.bufferID

    var slice = str[str.startIndex..<str.index(_nth: 7)]
    expectEqual(heapStrIdentity1, str.bufferID)
    expectEqual(heapStrIdentity1, slice.bufferID)

    // This mutation should reallocate the string.
    slice.replaceSubrange(slice.endIndex..<slice.endIndex, with: "a")
    expectNotEqual(heapStrIdentity1, slice.bufferID)
    expectEqual(heapStrIdentity1, str.bufferID)
    let heapStrIdentity2 = slice.bufferID
    expectEqual("1234567a", slice)
    expectEqual("12345678", str)

    // No more reallocations are expected.
    slice.replaceSubrange(
      slice.index(_nthLast: 1)..<slice.endIndex, with: "b")
    expectEqual(heapStrIdentity2, slice.bufferID)
    expectEqual(heapStrIdentity1, str.bufferID)

    expectEqual("1234567b", slice)
    expectEqual("12345678", str)
  }
}

func asciiString<
  S: Sequence
>(_ content: S) -> String
where S.Iterator.Element == Character {
  var s = String()
  s.append(contentsOf: content)
  expectEqual(1, s._core.elementWidth)
  return s
}

StringTests.test("stringCoreExtensibility")
  .skip(.nativeRuntime("Foundation dependency"))
  .code {
#if _runtime(_ObjC)
  let ascii = UTF16.CodeUnit(UnicodeScalar("X").value)
  let nonAscii = UTF16.CodeUnit(UnicodeScalar("é").value)

  for k in 0..<3 {
    for count in 1..<16 {
      for boundary in 0..<count {
        
        var x = (
            k == 0 ? asciiString("b".characters)
          : k == 1 ? ("b" as NSString as String)
          : ("b" as NSMutableString as String)
        )._core

        if k == 0 { expectEqual(1, x.elementWidth) }
        
        for i in 0..<count {
          x.append(contentsOf:
            repeatElement(i < boundary ? ascii : nonAscii, count: 3))
        }
        // Make sure we can append pure ASCII to wide storage
        x.append(contentsOf: repeatElement(ascii, count: 2))
        
        expectEqualSequence(
          [UTF16.CodeUnit(UnicodeScalar("b").value)]
          + Array(repeatElement(ascii, count: 3*boundary))
          + repeatElement(nonAscii, count: 3*(count - boundary))
          + repeatElement(ascii, count: 2),
          x
        )
      }
    }
  }
#else
  expectUnreachable()
#endif
}

StringTests.test("stringCoreReserve")
  .skip(.nativeRuntime("Foundation dependency"))
  .code {
#if _runtime(_ObjC)
  for k in 0...5 {
    var base: String
    var startedNative: Bool
    let shared: String = "X"

    switch k {
    case 0: (base, startedNative) = (String(), true)
    case 1: (base, startedNative) = (asciiString("x".characters), true)
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
      && isKnownUniquelyReferenced(&base._core._owner!)
    
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
#else
  expectUnreachable()
#endif
}

func makeStringCore(_ base: String) -> _StringCore {
  var x = _StringCore()
  // make sure some - but not all - replacements will have to grow the buffer
  x.reserveCapacity(base._core.count * 3 / 2)
  x.append(contentsOf: base._core)
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

StringTests.test("CharacterViewReplace") {
  let narrow = "01234567890"
  let wide = "ⅠⅡⅢⅣⅤⅥⅦⅧⅨⅩⅪ"
  
  for s1 in [narrow, wide] {
    for s2 in [narrow, wide] {
      checkRangeReplaceable(
        { String.CharacterView(makeStringCore(s1)) },
        { String.CharacterView(makeStringCore(s2 + s2)[0..<$0]) }
      )
      checkRangeReplaceable(
        { String.CharacterView(makeStringCore(s1)) },
        { Array(String.CharacterView(makeStringCore(s2 + s2)[0..<$0])) }
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
  s.insert(contentsOf: repeatElement(x, count: s.capacity + 1), at: s.endIndex)
  expectNotEqual(id0, s.bufferID)
  s = ""
  print("empty capacity \(s.capacity)")
  s.reserveCapacity(oldCap + 2)
  print("reserving \(oldCap + 2) -> \(s.capacity), width = \(s._core.elementWidth)")
  let id1 = s.bufferID
  s.insert(contentsOf: repeatElement(x, count: oldCap + 2), at: s.endIndex)
  print("extending by \(oldCap + 2) -> \(s.capacity), width = \(s._core.elementWidth)")
  expectEqual(id1, s.bufferID)
  s.insert(contentsOf: repeatElement(x, count: s.capacity + 100), at: s.endIndex)
  expectNotEqual(id1, s.bufferID)
}

StringTests.test("toInt") {
  expectNil(Int(""))
  expectNil(Int("+"))
  expectNil(Int("-"))
  expectOptionalEqual(20, Int("+20"))
  expectOptionalEqual(0, Int("0"))
  expectOptionalEqual(-20, Int("-20"))
  expectNil(Int("-cc20"))
  expectNil(Int("  -20"))
  expectNil(Int("  \t 20ddd"))

  expectOptionalEqual(Int.min, Int("\(Int.min)"))
  expectOptionalEqual(Int.min + 1, Int("\(Int.min + 1)"))
  expectOptionalEqual(Int.max, Int("\(Int.max)"))
  expectOptionalEqual(Int.max - 1, Int("\(Int.max - 1)"))

  expectNil(Int("\(Int.min)0"))
  expectNil(Int("\(Int.max)0"))

  // Make a String from an Int, mangle the String's characters,
  // then print if the new String is or is not still an Int.
  func testConvertabilityOfStringWithModification(
    _ initialValue: Int,
    modification: (_ chars: inout [UTF8.CodeUnit]) -> Void
  ) {
    var chars = Array(String(initialValue).utf8)
    modification(&chars)
    let str = String._fromWellFormedCodeUnitSequence(UTF8.self, input: chars)
    expectNil(Int(str))
  }

  testConvertabilityOfStringWithModification(Int.min) {
    $0[2] += 1; ()  // underflow by lots
  }

  testConvertabilityOfStringWithModification(Int.max) {
    $0[1] += 1; ()  // overflow by lots
  }

  // Test values lower than min.
  do {
    let base = UInt(Int.max)
    expectOptionalEqual(Int.min + 1, Int("-\(base)"))
    expectOptionalEqual(Int.min, Int("-\(base + 1)"))
    for i in 2..<20 {
      expectNil(Int("-\(base + UInt(i))"))
    }
  }

  // Test values greater than min.
  do {
    let base = UInt(Int.max)
    for i in UInt(0)..<20 {
      expectOptionalEqual(-Int(base - i) , Int("-\(base - i)"))
    }
  }

  // Test values greater than max.
  do {
    let base = UInt(Int.max)
    expectOptionalEqual(Int.max, Int("\(base)"))
    for i in 1..<20 {
      expectNil(Int("\(base + UInt(i))"))
    }
  }

  // Test values lower than max.
  do {
    let base = Int.max
    for i in 0..<20 {
      expectOptionalEqual(base - i, Int("\(base - i)"))
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
  expectEqual("abc", String(["a", "b", "c"] as [Character]))
}

StringTests.test("Conversions") {
  do {
    var c: Character = "a"
    let x = String(c)
    expectTrue(x._core.isASCII)

    var s: String = "a"
    expectEqual(s, x)
  }

  do {
    var c: Character = "\u{B977}"
    let x = String(c)
    expectFalse(x._core.isASCII)

    var s: String = "\u{B977}"
    expectEqual(s, x)
  }
}

// Check the internal functions are correct for ASCII values
StringTests.test(
  "forall x: Int8, y: Int8 . x < 128 ==> x <ascii y == x <unicode y")
  .skip(.nativeRuntime("String._compareASCII undefined without _runtime(_ObjC)"))
  .code {
#if _runtime(_ObjC)
  let asciiDomain = (0..<128).map({ String(UnicodeScalar($0)) })
  expectEqualMethodsForDomain(
    asciiDomain, asciiDomain, 
    String._compareDeterministicUnicodeCollation, String._compareASCII)
#else
  expectUnreachable()
#endif
}

#if os(Linux) || os(FreeBSD) || os(PS4) || os(Android)
import Glibc
#endif

StringTests.test("lowercased()") {
  // Use setlocale so tolower() is correct on ASCII.
  setlocale(LC_ALL, "C")

  // Check the ASCII domain.
  let asciiDomain: [Int32] = Array(0..<128)
  expectEqualFunctionsForDomain(
    asciiDomain,
    { String(UnicodeScalar(Int(tolower($0)))!) },
    { String(UnicodeScalar(Int($0))!).lowercased() })

  expectEqual("", "".lowercased())
  expectEqual("abcd", "abCD".lowercased())
  expectEqual("абвг", "абВГ".lowercased())
  expectEqual("たちつてと", "たちつてと".lowercased())

  //
  // Special casing.
  //

  // U+0130 LATIN CAPITAL LETTER I WITH DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0130}".lowercased())

  // U+0049 LATIN CAPITAL LETTER I
  // U+0307 COMBINING DOT ABOVE
  // to lower case:
  // U+0069 LATIN SMALL LETTER I
  // U+0307 COMBINING DOT ABOVE
  expectEqual("\u{0069}\u{0307}", "\u{0049}\u{0307}".lowercased())
}

StringTests.test("uppercased()") {
  // Use setlocale so toupper() is correct on ASCII.
  setlocale(LC_ALL, "C")

  // Check the ASCII domain.
  let asciiDomain: [Int32] = Array(0..<128)
  expectEqualFunctionsForDomain(
    asciiDomain,
    { String(UnicodeScalar(Int(toupper($0)))!) },
    { String(UnicodeScalar(Int($0))!).uppercased() })

  expectEqual("", "".uppercased())
  expectEqual("ABCD", "abCD".uppercased())
  expectEqual("АБВГ", "абВГ".uppercased())
  expectEqual("たちつてと", "たちつてと".uppercased())

  //
  // Special casing.
  //

  // U+0069 LATIN SMALL LETTER I
  // to upper case:
  // U+0049 LATIN CAPITAL LETTER I
  expectEqual("\u{0049}", "\u{0069}".uppercased())

  // U+00DF LATIN SMALL LETTER SHARP S
  // to upper case:
  // U+0053 LATIN CAPITAL LETTER S
  // U+0073 LATIN SMALL LETTER S
  // But because the whole string is converted to uppercase, we just get two
  // U+0053.
  expectEqual("\u{0053}\u{0053}", "\u{00df}".uppercased())

  // U+FB01 LATIN SMALL LIGATURE FI
  // to upper case:
  // U+0046 LATIN CAPITAL LETTER F
  // U+0069 LATIN SMALL LETTER I
  // But because the whole string is converted to uppercase, we get U+0049
  // LATIN CAPITAL LETTER I.
  expectEqual("\u{0046}\u{0049}", "\u{fb01}".uppercased())
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
    "\u{FFFD}", String(describing: 
      winter.utf8[
        winter.utf8.startIndex
        ..<
        winter.utf8.index(after: winter.utf8.index(after: winter.utf8.startIndex))
      ]))
  
  expectEqual(
    "\u{1F3C2}", String(
      winter.utf8[winter.utf8.startIndex..<winter.utf8.index(_nth: 4)]))

  expectEqual(
    "\u{1F3C2}", String(
      winter.utf16[winter.utf16.startIndex..<winter.utf16.index(_nth: 2)]))
  
  expectEqual(
    "\u{1F3C2}", String(
      winter.unicodeScalars[
        winter.unicodeScalars.startIndex..<winter.unicodeScalars.index(_nth: 1)
      ]))

  // views
  expectEqual(
    winter, String(
      winter.utf8[winter.utf8.startIndex..<winter.utf8.index(_nth: 7)]))

  expectEqual(
    winter, String(
      winter.utf16[winter.utf16.startIndex..<winter.utf16.index(_nth: 3)]))

  expectEqual(
    winter, String(
      winter.unicodeScalars[
        winter.unicodeScalars.startIndex..<winter.unicodeScalars.index(_nth: 2)
      ]))

  let ga = "\u{304b}\u{3099}"
  expectEqual(ga, String(ga.utf8[ga.utf8.startIndex..<ga.utf8.index(_nth: 6)]))
}

// Validate that index conversion does something useful for Cocoa
// programmers.
StringTests.test("indexConversion")
  .skip(.nativeRuntime("Foundation dependency"))
  .code {
#if _runtime(_ObjC)
  let re : NSRegularExpression
  do {
    re = try NSRegularExpression(
      pattern: "([^ ]+)er", options: NSRegularExpression.Options())
  } catch { fatalError("couldn't build regexp: \(error)") }

  let s = "go further into the larder to barter."

  var matches: [String] = []
  
  re.enumerateMatches(
    in: s, options: NSRegularExpression.MatchingOptions(), range: NSRange(0..<s.utf16.count)
  ) {
    result, flags, stop
  in
    let r = result!.rangeAt(1)
    let start = String.UTF16Index(_offset: r.location)
    let end = String.UTF16Index(_offset: r.location + r.length)
    matches.append(String(s.utf16[start..<end])!)
  }

  expectEqual(["furth", "lard", "bart"], matches)
#else
  expectUnreachable()
#endif
}

StringTests.test("String.append(_: UnicodeScalar)") {
  var s = ""

  do {
    // U+0061 LATIN SMALL LETTER A
    let input: UnicodeScalar = "\u{61}"
    s.append(String(input))
    expectEqual(["\u{61}"], Array(s.unicodeScalars))
  }
  do {
    // U+304B HIRAGANA LETTER KA
    let input: UnicodeScalar = "\u{304b}"
    s.append(String(input))
    expectEqual(["\u{61}", "\u{304b}"], Array(s.unicodeScalars))
  }
  do {
    // U+3099 COMBINING KATAKANA-HIRAGANA VOICED SOUND MARK
    let input: UnicodeScalar = "\u{3099}"
    s.append(String(input))
    expectEqual(["\u{61}", "\u{304b}", "\u{3099}"], Array(s.unicodeScalars))
  }
  do {
    // U+1F425 FRONT-FACING BABY CHICK
    let input: UnicodeScalar = "\u{1f425}"
    s.append(String(input))
    expectEqual(
      ["\u{61}", "\u{304b}", "\u{3099}", "\u{1f425}"],
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
  let baseStrings = [""] + baseCharacters.map { String($0) }

  for baseIdx in baseStrings.indices {
    for prefix in ["", " "] {
      let base = baseStrings[baseIdx]
      for inputIdx in baseCharacters.indices {
        let input = (prefix + String(baseCharacters[inputIdx])).characters.last!
        var s = base
        s.append(input)
        expectEqualSequence(
          Array(base.characters) + [input],
          Array(s.characters),
          "baseIdx=\(baseIdx) inputIdx=\(inputIdx)")
      }
    }
  }
}

internal func decodeCString<
  C : UnicodeCodec
>(_ s: String, as codec: C.Type)
-> (result: String, repairsMade: Bool)?
where C.CodeUnit : UnsignedInteger {
  let units = s.unicodeScalars.map({ $0.value }) + [0]
  return units.map({ C.CodeUnit(numericCast($0)) }).withUnsafeBufferPointer {
    String.decodeCString($0.baseAddress, as: C.self)
  }
}

StringTests.test("String.decodeCString/UTF8") {
  let actual = decodeCString("foobar", as: UTF8.self)
  expectFalse(actual!.repairsMade)
  expectEqual("foobar", actual!.result)
}

StringTests.test("String.decodeCString/UTF16") {
  let actual = decodeCString("foobar", as: UTF16.self)
  expectFalse(actual!.repairsMade)
  expectEqual("foobar", actual!.result)
}

StringTests.test("String.decodeCString/UTF32") {
  let actual = decodeCString("foobar", as: UTF32.self)
  expectFalse(actual!.repairsMade)
  expectEqual("foobar", actual!.result)
}

internal struct ReplaceSubrangeTest {
  let original: String
  let newElements: String
  let rangeSelection: RangeSelection
  let expected: String
  let closedExpected: String?
  let loc: SourceLoc

  internal init(
    original: String, newElements: String,
    rangeSelection: RangeSelection, expected: String, closedExpected: String? = nil,
    file: String = #file, line: UInt = #line
  ) {
    self.original = original
    self.newElements = newElements
    self.rangeSelection = rangeSelection
    self.expected = expected
    self.closedExpected = closedExpected
    self.loc = SourceLoc(file, line, comment: "replaceSubrange() test data")
  }
}

internal struct RemoveSubrangeTest {
  let original: String
  let rangeSelection: RangeSelection
  let expected: String
  let closedExpected: String
  let loc: SourceLoc

  internal init(
    original: String, rangeSelection: RangeSelection, expected: String,
    closedExpected: String? = nil,
    file: String = #file, line: UInt = #line
  ) {
    self.original = original
    self.rangeSelection = rangeSelection
    self.expected = expected
    self.closedExpected = closedExpected ?? expected
    self.loc = SourceLoc(file, line, comment: "replaceSubrange() test data")
  }
}

let replaceSubrangeTests = [
  ReplaceSubrangeTest(
    original: "",
    newElements: "",
    rangeSelection: .emptyRange,
    expected: ""
  ),
  ReplaceSubrangeTest(
    original: "",
    newElements: "meela",
    rangeSelection: .emptyRange,
    expected: "meela"
  ),
  ReplaceSubrangeTest(
    original: "eela",
    newElements: "m",
    rangeSelection: .leftEdge,
    expected: "meela",
    closedExpected: "mela"
  ),
  ReplaceSubrangeTest(
    original: "meel",
    newElements: "a",
    rangeSelection: .rightEdge,
    expected: "meela",
    closedExpected: "meea"
  ),
  ReplaceSubrangeTest(
    original: "a",
    newElements: "meel",
    rangeSelection: .leftEdge,
    expected: "meela",
    closedExpected: "meel"
  ),
  ReplaceSubrangeTest(
    original: "m",
    newElements: "eela",
    rangeSelection: .rightEdge,
    expected: "meela",
    closedExpected: "eela"
  ),
  ReplaceSubrangeTest(
    original: "alice",
    newElements: "bob",
    rangeSelection: .offsets(1, 1),
    expected: "aboblice",
    closedExpected: "abobice"
  ),
  ReplaceSubrangeTest(
    original: "alice",
    newElements: "bob",
    rangeSelection: .offsets(1, 2),
    expected: "abobice",
    closedExpected: "abobce"
  ),
  ReplaceSubrangeTest(
    original: "alice",
    newElements: "bob",
    rangeSelection: .offsets(1, 3),
    expected: "abobce",
    closedExpected: "abobe"
  ),
  ReplaceSubrangeTest(
    original: "alice",
    newElements: "bob",
    rangeSelection: .offsets(1, 4),
    expected: "abobe",
    closedExpected: "abob"
  ),
  ReplaceSubrangeTest(
    original: "alice",
    newElements: "bob",
    rangeSelection: .offsets(1, 5),
    expected: "abob"
  ),
  ReplaceSubrangeTest(
    original: "bob",
    newElements: "meela",
    rangeSelection: .offsets(1, 2),
    expected: "bmeelab",
    closedExpected: "bmeela"
  ),
]

let removeSubrangeTests = [
  RemoveSubrangeTest(
    original: "",
    rangeSelection: .emptyRange,
    expected: ""
  ),
  RemoveSubrangeTest(
    original: "a",
    rangeSelection: .middle,
    expected: ""
  ),
  RemoveSubrangeTest(
    original: "perdicus",
    rangeSelection: .leftHalf,
    expected: "icus"
  ),
  RemoveSubrangeTest(
    original: "perdicus",
    rangeSelection: .rightHalf,
    expected: "perd"
  ),
  RemoveSubrangeTest(
    original: "alice",
    rangeSelection: .middle,
    expected: "ae"
  ),
  RemoveSubrangeTest(
    original: "perdicus",
    rangeSelection: .middle,
    expected: "pes"
  ),
  RemoveSubrangeTest(
    original: "perdicus",
    rangeSelection: .offsets(1, 2),
    expected: "prdicus",
    closedExpected: "pdicus"
  ),
  RemoveSubrangeTest(
    original: "perdicus",
    rangeSelection: .offsets(3, 6),
    expected: "perus",
    closedExpected: "pers"
  )
]

StringTests.test("String.replaceSubrange()/characters/range") {
  for test in replaceSubrangeTests {
    var theString = test.original
    let c = test.original.characters
    let rangeToReplace = test.rangeSelection.range(in: c)
    let newCharacters : [Character] = test.newElements.characters.map { $0 }
    theString.replaceSubrange(rangeToReplace, with: newCharacters)
    expectEqual(
      test.expected,
      theString,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

StringTests.test("String.replaceSubrange()/string/range") {
  for test in replaceSubrangeTests {
    var theString = test.original
    let c = test.original.characters
    let rangeToReplace = test.rangeSelection.range(in: c)
    theString.replaceSubrange(rangeToReplace, with: test.newElements)
    expectEqual(
      test.expected,
      theString,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

StringTests.test("String.replaceSubrange()/characters/closedRange") {
  for test in replaceSubrangeTests {
    guard let closedExpected = test.closedExpected else {
      continue
    }
    var theString = test.original
    let c = test.original.characters
    let rangeToReplace = test.rangeSelection.closedRange(in: c)
    let newCharacters = Array(test.newElements.characters)
    theString.replaceSubrange(rangeToReplace, with: newCharacters)
    expectEqual(
      closedExpected,
      theString,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

StringTests.test("String.replaceSubrange()/string/closedRange") {
  for test in replaceSubrangeTests {
    guard let closedExpected = test.closedExpected else {
      continue
    }
    var theString = test.original
    let c = test.original.characters
    let rangeToReplace = test.rangeSelection.closedRange(in: c)
    theString.replaceSubrange(rangeToReplace, with: test.newElements)
    expectEqual(
      closedExpected,
      theString,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

StringTests.test("String.removeSubrange()/range") {
  for test in removeSubrangeTests {
    var theString = test.original
    let c = test.original.characters
    let rangeToRemove = test.rangeSelection.range(in: c)
    theString.removeSubrange(rangeToRemove)
    expectEqual(
      test.expected,
      theString,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

StringTests.test("String.removeSubrange()/closedRange") {
  for test in removeSubrangeTests {
    switch test.rangeSelection {
      case .emptyRange: continue
      default: break
    }
    var theString = test.original
    let c = test.original.characters
    let rangeToRemove = test.rangeSelection.closedRange(in: c)
    theString.removeSubrange(rangeToRemove)
    expectEqual(
      test.closedExpected,
      theString,
      stackTrace: SourceLocStack().with(test.loc))
  }
}

runAllTests()
