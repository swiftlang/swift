//===--- StringViews.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift

import Swift
import StdlibUnittest

// CHECK: testing...
println("testing...")

// This string contains a variety of non-ASCII characters, including
// Unicode scalars that must be represented with a surrogate pair in UTF16
// grapheme clusters composed of multiple
let winter = "🏂☃❅❆❄︎⛄️❄️" 
let summer = "school's out!"

func printHexSequence<
  S:SequenceType where S.Generator.Element : IntegerType
>(s: S) {
  print("[")
  var prefix = ""
  for x in s {
    print(prefix);
    print(String(x.toIntMax(), radix: 16))
    prefix = " "
  }
  println("]")
}

var tests = TestSuite("StringViews")

tests.test("decoding") {
  expectEqualSequence(
    [0xf0, 0x9f, 0x8f, 0x82, 0xe2, 0x98, 0x83, 0xe2, 0x9d, 0x85, 0xe2,
    0x9d, 0x86, 0xe2, 0x9d, 0x84, 0xef, 0xb8, 0x8e, 0xe2, 0x9b, 0x84,
    0xef, 0xb8, 0x8f, 0xe2, 0x9d, 0x84, 0xef, 0xb8, 0x8f],
    winter.utf8
  )
  
  expectEqualSequence(
    [0xd83c, 0xdfc2, 0x2603, 0x2745, 0x2746, 0x2744, 0xfe0e, 0x26c4,
      0xfe0f, 0x2744, 0xfe0f],
      winter.utf16
  )

  expectEqualSequence(
    [0x73, 0x63, 0x68, 0x6f, 0x6f, 0x6c, 0x27, 0x73, 0x20, 0x6f, 0x75,
    0x74, 0x21],
    summer.utf8
  )

  expectEqualSequence(
    [0x73, 0x63, 0x68, 0x6f, 0x6f, 0x6c, 0x27, 0x73, 0x20, 0x6f, 0x75,
    0x74, 0x21],
    summer.utf16
  )
}

func utf8GraphemeClusterIndices(s: String) -> [String.UTF8Index] {
  return indices(s).map { $0.samePositionIn(s.utf8) }
}

func utf8UnicodeScalarIndices(s: String) -> [String.UTF8Index] {
  return indices(s.unicodeScalars).map { $0.samePositionIn(s.utf8) }
}

func utf8UTF16Indices(s: String) -> [String.UTF8Index?] {
  return indices(s.utf16).map { $0.samePositionIn(s.utf8) }
}
  
// winter UTF8 grapheme clusters ([]) and unicode scalars (|)
// [f0 9f 8f 82] [e2 98 83] [e2 9d 85] [e2 9d 86] [e2 9d 84 | ef b8 8e]
// [e2 9b 84 | ef b8 8f]    [e2 9d 84 | ef b8 8f]

tests.test("index mapping") {
  // the first four utf8 code units at the start of each grapheme
  // cluster
  expectEqualSequence(
    [
      [0xf0, 0x9f, 0x8f, 0x82],
      [0xe2, 0x98, 0x83, 0xe2],
      [0xe2, 0x9d, 0x85, 0xe2],
      [0xe2, 0x9d, 0x86, 0xe2],
      [0xe2, 0x9d, 0x84, 0xef],
      [0xe2, 0x9b, 0x84, 0xef],
      [0xe2, 0x9d, 0x84, 0xef]] as [[UTF8.CodeUnit]],
    
    map(utf8GraphemeClusterIndices(winter)) {
      i in (0..<4).map { winter.utf8[advance(i, $0)] }
    }, ==)

  expectEqualSequence(
    [0x73, 0x63, 0x68, 0x6f, 0x6f, 0x6c, 0x27, 0x73, 0x20, 0x6f, 0x75,
      0x74, 0x21],
    utf8GraphemeClusterIndices(summer).map { summer.utf8[$0] }
  )

  // the first three utf8 code units at the start of each unicode
  // scalar
  expectEqualSequence(
    [
      [0xf0, 0x9f, 0x8f],
      [0xe2, 0x98, 0x83],
      [0xe2, 0x9d, 0x85],
      [0xe2, 0x9d, 0x86],
      [0xe2, 0x9d, 0x84],
      [0xef, 0xb8, 0x8e],
      [0xe2, 0x9b, 0x84],
      [0xef, 0xb8, 0x8f],
      [0xe2, 0x9d, 0x84],
      [0xef, 0xb8, 0x8f]
    ] as [[UTF8.CodeUnit]],
    
    map(utf8UnicodeScalarIndices(winter)) {
      i in (0..<3).map { winter.utf8[advance(i, $0)] }
    }, ==)

  expectEqualSequence(
    [0x73, 0x63, 0x68, 0x6f, 0x6f, 0x6c, 0x27, 0x73, 0x20, 0x6f, 0x75,
      0x74, 0x21],
    utf8UnicodeScalarIndices(summer).map { summer.utf8[$0] }
  )

  // check the first three utf8 code units at the start of each utf16
  // code unit
  expectEqualSequence(
    [
      [0xf0, 0x9f, 0x8f],
      [], // does not align with any utf8 code unit
      [0xe2, 0x98, 0x83],
      [0xe2, 0x9d, 0x85],
      [0xe2, 0x9d, 0x86],
      [0xe2, 0x9d, 0x84],
      [0xef, 0xb8, 0x8e],
      [0xe2, 0x9b, 0x84],
      [0xef, 0xb8, 0x8f],
      [0xe2, 0x9d, 0x84],
      [0xef, 0xb8, 0x8f]
    ] as [[UTF8.CodeUnit]],
    map(indices(winter.utf16)) {
      i16 in i16.samePositionIn(winter.utf8).map {
        i8 in (0..<3).map { winter.utf8[advance(i8, $0)] }
      } ?? []
    }, ==)
  
  expectEqualSequence(
    [0x73, 0x63, 0x68, 0x6f, 0x6f, 0x6c, 0x27, 0x73, 0x20, 0x6f, 0x75,
      0x74, 0x21],
    utf8UTF16Indices(summer).map { summer.utf8[$0!] }
  )
}
  

func expectEquality<T: Equatable>(x: T, y: T, expected: Bool) {
  let actual = x == y
  if expected != actual {
    let op = actual ? "==" : "!="
    println("unexpectedly, \(x) \(op) \(y)")
  }
  if actual != (y == x) {
    println("equality is asymmetric")
  }
}

func expectNil<T>(x: T?, _ message: String = "unexpected non-nil") {
  if x != nil { println(message) }
}

// These are rather complicated due to their internal buffers, so
// rigorous tests are required
tests.test("UTF8 indexes") {

  // Make sure that equivalent UTF8 indices computed in different ways
  // are still equal.
  //
  // CHECK-NEXT: true
  let abc = "abcdefghijklmnop"
  
  expectEqual(
    String.UTF8Index(abc.startIndex, within: abc.utf8).successor(),
    String.UTF8Index(abc.startIndex.successor(), within: abc.utf8))

  let diverseCharacters = summer + winter + winter + summer
  let s = diverseCharacters.unicodeScalars
  let u8 = diverseCharacters.utf8
  let u16 = diverseCharacters.utf16

  //===--- nested for...in loops ------------------------------------------===//
  // Test all valid subranges si0..<si1 of positions in s.  ds is
  // always distance(si0, si1)
  for si0 in indices(s) {
    for (ds, si1) in enumerate(si0..<s.endIndex) {
      
      // Map those unicode scalar indices into utf8 indices
      let u8i1 = si1.samePositionIn(u8)
      let u8i0 = si0.samePositionIn(u8)

      //===--- while loop -------------------------------------------------===//
      // Advance an index from u8i0 over ds Unicode scalars (thus
      // reaching u8i1) by counting leading bytes traversed
      var u8i0a = u8i0
      var dsa = 0      // number of Unicode scalars it has advanced over
      
      while true {
        //===--- loop condition -------------------------------------------===//
        let b = u8[u8i0a]
        let isLeadingByte = !UTF8.isContinuation(b)
        if dsa == ds && isLeadingByte { break } // 
        //===--------------------------------------------------------------===//
        
        expectEquality(u8i0a, u8i1, false) // We're not there yet

        if isLeadingByte { // On a unicode scalar boundary?
          let u16i0a = u8i0a.samePositionIn(u16)!
           // we should be able to round-trip through UTF16
          expectEqual(u8i0a, u16i0a.samePositionIn(u8)!)

          if UTF16.isLeadSurrogate(u16[u16i0a]) {
            // We only have well-formed UTF16 in this string, so the
            // successor points to a trailing surrogate of a pair and
            // thus shouldn't convert to a UTF8 position
            expectEmpty(u16i0a.successor().samePositionIn(u8))
          }
          
          ++dsa // we're moving off the beginning of a new Unicode scalar
        }
        else {
          expectEmpty(u8i0a.samePositionIn(u16))
        }
        ++u8i0a
      }

      expectEqual(u8i0a, u8i1) // We should be there now

      // Also check some UTF8 positions between unicode scalars for equality
      var u8i0b = u8i0a
      for n0 in 0..<8 {
        var u8i1b = u8i1
        for n1 in 0..<8 {
          expectEqual(u8i0b, u8i1b, n0 == n1 ? (==) : (!=))
          if u8i1b == u8.endIndex { break }
          ++u8i1b
        }
        if u8i0b == u8.endIndex { break }
        ++u8i0b
      }
    }
  }
}

runAllTests()
