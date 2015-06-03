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
// REQUIRES: executable_test

import Swift
import StdlibUnittest

// CHECK: testing...
print("testing...")

let replacementUTF16: UTF16.CodeUnit = 0xFFFD
let replacementUTF8: [UTF8.CodeUnit] = [0xEF, 0xBF, 0xBD]
let replacementScalar = UnicodeScalar(replacementUTF16)
let replacementCharacter = Character(replacementScalar)

// This string contains a variety of non-ASCII characters, including
// Unicode scalars that must be represented with a surrogate pair in
// UTF16, grapheme clusters composed of multiple Unicode scalars, and
// invalid UTF16 that should be replaced with replacement characters.
let winter = String("🏂☃❅❆❄︎⛄️❄️"._core + [0xD83C, 0x0020, 0xDF67, 0xD83C])
let winterInvalidUTF8: [UTF8.CodeUnit] = replacementUTF8 + ([0x20] as [UTF8.CodeUnit]) + replacementUTF8 + replacementUTF8
let winterUTF8: [UTF8.CodeUnit] = [
  0xf0, 0x9f, 0x8f, 0x82, 0xe2, 0x98, 0x83, 0xe2, 0x9d, 0x85, 0xe2,
  0x9d, 0x86, 0xe2, 0x9d, 0x84, 0xef, 0xb8, 0x8e, 0xe2, 0x9b, 0x84,
  0xef, 0xb8, 0x8f, 0xe2, 0x9d, 0x84, 0xef, 0xb8, 0x8f
] + winterInvalidUTF8

let summer = "school's out!"
let summerBytes: [UInt8] = [
  0x73, 0x63, 0x68, 0x6f, 0x6f, 0x6c, 0x27, 0x73, 0x20, 0x6f, 0x75, 0x74, 0x21]

var tests = TestSuite("StringViews")

tests.test("decoding") {
  expectEqualSequence(
    winterUTF8,
    winter.utf8
  )
  
  expectEqualSequence(
    [0xd83c, 0xdfc2, 0x2603, 0x2745, 0x2746, 0x2744, 0xfe0e, 0x26c4,
      0xfe0f, 0x2744, 0xfe0f,
      replacementUTF16, 0x0020, replacementUTF16, replacementUTF16
    ],
    winter.utf16
  )

  expectEqualSequence(
    summerBytes,
    summer.utf8
  )

  expectEqualSequence(
    summerBytes.map {UTF16.CodeUnit($0)},
    summer.utf16
  )
}

// winter UTF8 grapheme clusters ([]) and unicode scalars (|)
// [f0 9f 8f 82] [e2 98 83] [e2 9d 85] [e2 9d 86] [e2 9d 84 | ef b8 8e]
// [e2 9b 84 | ef b8 8f]    [e2 9d 84 | ef b8 8f]

//===--- To UTF8 ----------------------------------------------------------===//
tests.test("index-mapping/character-to-utf8") {
  // the first three utf8 code units at the start of each grapheme
  // cluster
  expectEqualSequence(
    [
      [0xf0, 0x9f, 0x8f],
      [0xe2, 0x98, 0x83],
      [0xe2, 0x9d, 0x85],
      [0xe2, 0x9d, 0x86],
      [0xe2, 0x9d, 0x84],
      [0xe2, 0x9b, 0x84],
      [0xe2, 0x9d, 0x84],
      replacementUTF8,
      [0x20] + replacementUTF8[0..<2],
      replacementUTF8,
      replacementUTF8
    ] as [[UTF8.CodeUnit]],

    winter.characters.indices.map {
      i in (0..<3).map {
        winter.utf8[advance(i.samePositionIn(winter.utf8), $0)]
      }
    }, ==)

  expectEqual(winter.utf8.endIndex, winter.endIndex.samePositionIn(winter.utf8))
  
  expectEqualSequence(
    summerBytes,
    summer.characters.indices.map { summer.utf8[$0.samePositionIn(summer.utf8)] }
  )
  
  expectEqual(summer.utf8.endIndex, summer.endIndex.samePositionIn(summer.utf8))
}

tests.test("index-mapping/unicode-scalar-to-utf8") {
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
      [0xef, 0xb8, 0x8f],
      replacementUTF8,
      [0x20] + replacementUTF8[0..<2],
      replacementUTF8,
      replacementUTF8
    ] as [[UTF8.CodeUnit]],
    
    winter.unicodeScalars.indices.map {
      i in (0..<3).map {
        winter.utf8[advance(i.samePositionIn(winter.utf8), $0)]
      }
    }, ==)

  expectEqual(
    winter.utf8.endIndex,
    winter.unicodeScalars.endIndex.samePositionIn(winter.utf8))
  
  expectEqualSequence(
    summerBytes,
    summer.unicodeScalars.indices.map {
      summer.utf8[$0.samePositionIn(summer.utf8)]
    }
  )

  expectEqual(
    summer.utf8.endIndex,
    summer.unicodeScalars.endIndex.samePositionIn(summer.utf8))
}

tests.test("index-mapping/utf16-to-utf8") {
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
      [0xef, 0xb8, 0x8f],
      replacementUTF8,
      [0x20] + replacementUTF8[0..<2],
      replacementUTF8,
      replacementUTF8
    ] as [[UTF8.CodeUnit]],
    winter.utf16.indices.map {
      i16 in i16.samePositionIn(winter.utf8).map {
        i8 in (0..<3).map { winter.utf8[advance(i8, $0)] }
      } ?? []
    }, ==)

  expectNotEmpty(winter.utf16.endIndex.samePositionIn(winter.utf8))
  expectEqual(
    winter.utf8.endIndex,
    winter.utf16.endIndex.samePositionIn(winter.utf8)!)
  
  expectEqualSequence(
    summerBytes,
    summer.utf16.indices.map {
      summer.utf8[$0.samePositionIn(summer.utf8)!]
    }
  )
  
  expectNotEmpty(summer.utf16.endIndex.samePositionIn(summer.utf8))
  expectEqual(
    summer.utf8.endIndex,
    summer.utf16.endIndex.samePositionIn(summer.utf8)!)
}

tests.test("index-mapping/utf8-to-unicode-scalar") {
  // Define expectation separately to help the type-checker, which
  // otherwise runs out of time solving.
  let winterUtf8UnicodeScalars: [UnicodeScalar?] = [
    UnicodeScalar(0x1f3c2), nil, nil, nil,
    UnicodeScalar(0x2603), nil, nil,
    UnicodeScalar(0x2745), nil, nil,
    UnicodeScalar(0x2746), nil, nil,
    UnicodeScalar(0x2744), nil, nil, UnicodeScalar(0xfe0e), nil, nil,
    UnicodeScalar(0x26c4), nil, nil, UnicodeScalar(0xfe0f), nil, nil,
    UnicodeScalar(0x2744), nil, nil, UnicodeScalar(0xfe0f), nil, nil
  ]
  
  expectEqualSequence(
    winterUtf8UnicodeScalars,
    winter.utf8.indices.map {
      i in i.samePositionIn(winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(winter.utf8.endIndex.samePositionIn(winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf8.endIndex.samePositionIn(winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf8.indices.map {
      i in i.samePositionIn(summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(summer.utf8.endIndex.samePositionIn(summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf8.endIndex.samePositionIn(summer.unicodeScalars)!)
}

tests.test("index-mapping/utf16-to-unicode-scalar") {
  let winterUtf16UnicodeScalars: [UnicodeScalar?] = [
    UnicodeScalar(0x1f3c2), nil,
    UnicodeScalar(0x2603), 
    UnicodeScalar(0x2745), 
    UnicodeScalar(0x2746), 
    UnicodeScalar(0x2744), UnicodeScalar(0xfe0e),
    UnicodeScalar(0x26c4), UnicodeScalar(0xfe0f),
    UnicodeScalar(0x2744), UnicodeScalar(0xfe0f)
  ]
  
  expectEqualSequence(
    winterUtf16UnicodeScalars,
    winter.utf16.indices.map {
      i in i.samePositionIn(winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(winter.utf16.endIndex.samePositionIn(winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf16.endIndex.samePositionIn(winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf16.indices.map {
      i in i.samePositionIn(summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(summer.utf16.endIndex.samePositionIn(summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf16.endIndex.samePositionIn(summer.unicodeScalars)!)
}

//===--- To UTF16 ----------------------------------------------------------===//
tests.test("index-mapping/character-to-utf16") {
  expectEqualSequence(
    [
      0xd83c, // 0xdfc2,
      0x2603,
      0x2745,
      0x2746,
      0x2744, // 0xfe0e,
      0x26c4, // 0xfe0f,
      0x2744, // 0xfe0f,
      replacementUTF16, 0x20, replacementUTF16, replacementUTF16
    ] as [UTF16.CodeUnit],
    
    winter.characters.indices.map {
      winter.utf16[$0.samePositionIn(winter.utf16)]
    }, ==)

  expectEqual(winter.utf16.endIndex, winter.endIndex.samePositionIn(winter.utf16))
  
  expectEqualSequence(
    summerBytes.map { UTF16.CodeUnit($0) },
    summer.characters.indices.map { summer.utf16[$0.samePositionIn(summer.utf16)] }
  )
  
  expectEqual(summer.utf16.endIndex, summer.endIndex.samePositionIn(summer.utf16))
}

tests.test("index-mapping/unicode-scalar-to-utf16") {
  expectEqualSequence(
    [
      0xd83c, // 0xdfc2,
      0x2603,
      0x2745,
      0x2746,
      0x2744, 0xfe0e,
      0x26c4, 0xfe0f,
      0x2744, 0xfe0f,
      replacementUTF16, 0x20, replacementUTF16, replacementUTF16
    ] as [UTF16.CodeUnit],
    
    winter.unicodeScalars.indices.map {
      winter.utf16[$0.samePositionIn(winter.utf16)]
    })

  expectEqual(
    winter.utf16.endIndex,
    winter.unicodeScalars.endIndex.samePositionIn(winter.utf16))
  
  expectEqualSequence(
    summerBytes.map { UTF16.CodeUnit($0) },
    summer.unicodeScalars.indices.map {
      summer.utf16[$0.samePositionIn(summer.utf16)]
    }
  )

  expectEqual(
    summer.utf16.endIndex,
    summer.unicodeScalars.endIndex.samePositionIn(summer.utf16))
}

tests.test("index-mapping/utf8-to-utf16") {
  expectEqualSequence(
    [
      0xd83c, nil, nil, nil,
      0x2603, nil, nil,
      0x2745, nil, nil,
      0x2746, nil, nil,
      0x2744, nil, nil,
      0xfe0e, nil, nil,
      0x26c4, nil, nil,
      0xfe0f, nil, nil,
      0x2744, nil, nil,
      0xfe0f, nil, nil,
      replacementUTF16, nil, nil,
      0x20,
      replacementUTF16, nil, nil,
      replacementUTF16, nil, nil
    ] as [UTF16.CodeUnit?],

    winter.utf8.indices.map {
      $0.samePositionIn(winter.utf16).map {
        winter.utf16[$0]
      }
    }, ==)

  expectNotEmpty(winter.utf8.endIndex.samePositionIn(winter.utf16))
  expectEqual(
    winter.utf16.endIndex,
    winter.utf8.endIndex.samePositionIn(winter.utf16)!)
  
  expectEqualSequence(
    summerBytes.map { UTF16.CodeUnit($0) },
    summer.utf8.indices.map { summer.utf16[$0.samePositionIn(summer.utf16)!] }
  )
  
  expectNotEmpty(summer.utf8.endIndex.samePositionIn(summer.utf16))
  expectEqual(
    summer.utf16.endIndex,
    summer.utf8.endIndex.samePositionIn(summer.utf16)!)
}

//===--- To UnicodeScalar -------------------------------------------------===//
tests.test("index-mapping/character-to-unicode-scalar") {
  let winterCharacterUnicodeScalars: [UnicodeScalar] = [
    UnicodeScalar(0x1f3c2),
    UnicodeScalar(0x2603),
    UnicodeScalar(0x2745),
    UnicodeScalar(0x2746),
    UnicodeScalar(0x2744), // 0xfe0e,
    UnicodeScalar(0x26c4), // 0xfe0f,
    UnicodeScalar(0x2744), // 0xfe0f
    replacementScalar, UnicodeScalar(0x20), replacementScalar, replacementScalar
  ]
  
  expectEqualSequence(
    winterCharacterUnicodeScalars,
    winter.characters.indices.map {
      winter.unicodeScalars[$0.samePositionIn(winter.unicodeScalars)]
    })

  expectEqual(winter.unicodeScalars.endIndex, winter.endIndex.samePositionIn(winter.unicodeScalars))
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) },
    summer.characters.indices.map { summer.unicodeScalars[$0.samePositionIn(summer.unicodeScalars)] }
  )
  
  expectEqual(summer.unicodeScalars.endIndex, summer.endIndex.samePositionIn(summer.unicodeScalars))
}

tests.test("index-mapping/utf8-to-unicode-scalar") {
  // Define expectation separately to help the type-checker, which
  // otherwise runs out of time solving.
  let winterUtf8UnicodeScalars: [UnicodeScalar?] = [
    UnicodeScalar(0x1f3c2), nil, nil, nil,
    UnicodeScalar(0x2603), nil, nil,
    UnicodeScalar(0x2745), nil, nil,
    UnicodeScalar(0x2746), nil, nil,
    UnicodeScalar(0x2744), nil, nil, UnicodeScalar(0xfe0e), nil, nil,
    UnicodeScalar(0x26c4), nil, nil, UnicodeScalar(0xfe0f), nil, nil,
    UnicodeScalar(0x2744), nil, nil, UnicodeScalar(0xfe0f), nil, nil,
    replacementScalar, nil, nil,
    UnicodeScalar(0x20),
    replacementScalar, nil, nil,
    replacementScalar, nil, nil
  ]
  
  expectEqualSequence(
    winterUtf8UnicodeScalars,
    winter.utf8.indices.map {
      i in i.samePositionIn(winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(winter.utf8.endIndex.samePositionIn(winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf8.endIndex.samePositionIn(winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf8.indices.map {
      i in i.samePositionIn(summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(summer.utf8.endIndex.samePositionIn(summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf8.endIndex.samePositionIn(summer.unicodeScalars)!)
}

tests.test("index-mapping/utf16-to-unicode-scalar") {
  let winterUtf16UnicodeScalars: [UnicodeScalar?] = [
    UnicodeScalar(0x1f3c2), nil,
    UnicodeScalar(0x2603), 
    UnicodeScalar(0x2745), 
    UnicodeScalar(0x2746), 
    UnicodeScalar(0x2744), UnicodeScalar(0xfe0e),
    UnicodeScalar(0x26c4), UnicodeScalar(0xfe0f),
    UnicodeScalar(0x2744), UnicodeScalar(0xfe0f),
    replacementScalar, UnicodeScalar(0x20), replacementScalar, replacementScalar
  ]
  
  expectEqualSequence(
    winterUtf16UnicodeScalars,
    winter.utf16.indices.map {
      i in i.samePositionIn(winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(winter.utf16.endIndex.samePositionIn(winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf16.endIndex.samePositionIn(winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf16.indices.map {
      i in i.samePositionIn(summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, ==
  )

  expectNotEmpty(summer.utf16.endIndex.samePositionIn(summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf16.endIndex.samePositionIn(summer.unicodeScalars)!)
}

//===--- To Character -------------------------------------------------===//
tests.test("index-mapping/unicode-scalar-to-character") {
  let winterUnicodeScalarCharacters: [Character?] = [
    "🏂", "☃", "❅", "❆", "❄︎", nil, "⛄️", nil, "❄️", nil,
    replacementCharacter, "\u{20}", replacementCharacter, replacementCharacter
  ]
  
  expectEqualSequence(
    winterUnicodeScalarCharacters,
    
    winter.unicodeScalars.indices.map {
      i in i.samePositionIn(winter).map {
        winter[$0]
      }
    },
    ==
  )

  expectEqual(winter.endIndex, winter.unicodeScalars.endIndex.samePositionIn(winter)!)
  
  expectEqualSequence(
    summerBytes.map { Character(UnicodeScalar($0)) },
    summer.unicodeScalars.indices.map { summer[$0.samePositionIn(summer)!] }
  )
  
  expectEqual(summer.endIndex, summer.unicodeScalars.endIndex.samePositionIn(summer)!)
}

tests.test("index-mapping/utf8-to-character") {
  // Define expectation separately to help the type-checker, which
  // otherwise runs out of time solving.
  let winterUtf8Characters: [Character?] = [
    "🏂", nil, nil, nil,
    "☃", nil, nil,
    "❅", nil, nil,
    "❆", nil, nil,
    "❄︎", nil, nil, nil, nil, nil,
    "⛄️", nil, nil, nil, nil, nil,
    "❄️", nil, nil, nil, nil, nil,
    replacementCharacter, nil, nil,
    "\u{20}",
    replacementCharacter, nil, nil,
    replacementCharacter, nil, nil,
  ]

  expectEqualSequence(
    winterUtf8Characters,
    winter.utf8.indices.map {
      (i:String.UTF8Index)->Character? in i.samePositionIn(winter).map {
        winter[$0]
      }
    }, ==
  )

  expectNotEmpty(winter.utf8.endIndex.samePositionIn(winter))
  expectEqual(
    winter.endIndex,
    winter.utf8.endIndex.samePositionIn(winter)!)
  
  expectEqualSequence(
    summerBytes.map { Character(UnicodeScalar($0)) },
    summer.utf8.indices.map { summer[$0.samePositionIn(summer)!] }
  )

  expectNotEmpty(summer.utf8.endIndex.samePositionIn(summer))
  expectEqual(
    summer.endIndex,
    summer.utf8.endIndex.samePositionIn(summer)!)
}

tests.test("index-mapping/utf16-to-character") {
  let winterUtf16Characters: [Character?] = [
    "🏂", nil, "☃", "❅", "❆", "❄︎", nil, "⛄️", nil, "❄️", nil,
    replacementCharacter, "\u{20}", replacementCharacter, replacementCharacter
  ]
  
  expectEqualSequence(
    winterUtf16Characters,
    winter.utf16.indices.map {
      i in i.samePositionIn(winter).map {
        winter[$0]
      }
    }, ==
  )

  expectNotEmpty(winter.utf16.endIndex.samePositionIn(winter))
  expectEqual(
    winter.endIndex,
    winter.utf16.endIndex.samePositionIn(winter)!)
  
  expectEqualSequence(
    summerBytes.map { Character(UnicodeScalar($0)) },
    summer.utf16.indices.map {
      summer[$0.samePositionIn(summer)!]
    }
  )

  expectNotEmpty(summer.utf16.endIndex.samePositionIn(summer))
  expectEqual(
    summer.endIndex,
    summer.utf16.endIndex.samePositionIn(summer)!)
}

//===----------------------------------------------------------------------===//
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
  for si0 in s.indices {
    for (ds, si1) in (si0..<s.endIndex).enumerate() {
      
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
        
        expectNotEqual(u8i0a, u8i1) // We're not there yet

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

tests.test("UTF16->String") {
  let s = summer + winter + winter + summer
  let v = s.utf16
  for i in v.indices {
    for j in i..<v.endIndex {      
      if let si = i.samePositionIn(s) {
        if let sj = j.samePositionIn(s) {
          expectEqual(s[si..<sj], String(v[i..<j])!)
          continue
        }
      }
      expectEmpty(String(v[i..<j]))
    }
  }
}

tests.test("UTF8->String") {
  let s = summer + winter + winter + summer
  let v = s.utf8
  for i in v.indices {
    for j in i..<v.endIndex {      
      if let si = i.samePositionIn(s) {
        if let sj = j.samePositionIn(s) {
          expectEqual(s[si..<sj], String(v[i..<j])!)
          continue
        }
      }
      expectEmpty(String(v[i..<j]))
    }
  }
}

tests.test("UnicodeScalars->String") {
  let s = summer + winter + winter + summer
  let v = s.unicodeScalars
  for i in s.characters.indices {
    for j in i..<s.endIndex {
      expectEqual(
        s[i..<j],
        String(v[i.samePositionIn(v)..<j.samePositionIn(v)])
      )
    }
  }
}


runAllTests()
