//===--- StringViews.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest
import StdlibUnicodeUnittest
import StdlibCollectionUnittest

#if _runtime(_ObjC)
// FIXME: Foundation leaks through StdlibUnittest.  It adds some conformances
// that overload resolution picks up in this code.
import Foundation
#endif


// CHECK: testing...
print("testing...")

let replacementUTF16: UTF16.CodeUnit = 0xFFFD
let replacementUTF8: [UTF8.CodeUnit] = [0xEF, 0xBF, 0xBD]
let replacementScalar = UnicodeScalar(replacementUTF16)!
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
        winter.utf8[
          winter.utf8.index(
            i.samePosition(in: winter.utf8), offsetBy: $0)]
      }
    }, sameValue: ==)

  expectEqual(winter.utf8.endIndex, winter.endIndex.samePosition(in: winter.utf8))
  
  expectEqualSequence(
    summerBytes,
    summer.characters.indices.map { summer.utf8[$0.samePosition(in: summer.utf8)] }
  )
  
  expectEqual(summer.utf8.endIndex, summer.endIndex.samePosition(in: summer.utf8))
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
        winter.utf8[
          winter.utf8.index(i.samePosition(in: winter.utf8), offsetBy: $0)]
      }
    }, sameValue: ==)

  expectEqual(
    winter.utf8.endIndex,
    winter.unicodeScalars.endIndex.samePosition(in: winter.utf8))
  
  expectEqualSequence(
    summerBytes,
    summer.unicodeScalars.indices.map {
      summer.utf8[$0.samePosition(in: summer.utf8)]
    }
  )

  expectEqual(
    summer.utf8.endIndex,
    summer.unicodeScalars.endIndex.samePosition(in: summer.utf8))
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
      i16 in i16.samePosition(in: winter.utf8).map {
        i8 in (0..<3).map {
          winter.utf8[winter.utf8.index(i8, offsetBy: $0)]
        }
      } ?? []
    }, sameValue: ==)

  expectNotNil(winter.utf16.endIndex.samePosition(in: winter.utf8))
  expectEqual(
    winter.utf8.endIndex,
    winter.utf16.endIndex.samePosition(in: winter.utf8)!)
  
  expectEqualSequence(
    summerBytes,
    summer.utf16.indices.map {
      summer.utf8[$0.samePosition(in: summer.utf8)!]
    }
  )
  
  expectNotNil(summer.utf16.endIndex.samePosition(in: summer.utf8))
  expectEqual(
    summer.utf8.endIndex,
    summer.utf16.endIndex.samePosition(in: summer.utf8)!)
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
      i in i.samePosition(in: winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, sameValue: ==
  )

  expectNotNil(winter.utf8.endIndex.samePosition(in: winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf8.endIndex.samePosition(in: winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf8.indices.map {
      i in i.samePosition(in: summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, sameValue: ==
  )

  expectNotNil(summer.utf8.endIndex.samePosition(in: summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf8.endIndex.samePosition(in: summer.unicodeScalars)!)
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
      i in i.samePosition(in: winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, sameValue: ==
  )

  expectNotNil(winter.utf16.endIndex.samePosition(in: winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf16.endIndex.samePosition(in: winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf16.indices.map {
      i in i.samePosition(in: summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, sameValue: ==
  )

  expectNotNil(summer.utf16.endIndex.samePosition(in: summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf16.endIndex.samePosition(in: summer.unicodeScalars)!)
}

//===--- To UTF16 ---------------------------------------------------------===//
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
      winter.utf16[$0.samePosition(in: winter.utf16)]
    },
    sameValue: ==)

  expectEqual(winter.utf16.endIndex, winter.endIndex.samePosition(in: winter.utf16))
  
  expectEqualSequence(
    summerBytes.map { UTF16.CodeUnit($0) },
    summer.characters.indices.map { summer.utf16[$0.samePosition(in: summer.utf16)] }
  )
  
  expectEqual(summer.utf16.endIndex, summer.endIndex.samePosition(in: summer.utf16))
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
      winter.utf16[$0.samePosition(in: winter.utf16)]
    })

  expectEqual(
    winter.utf16.endIndex,
    winter.unicodeScalars.endIndex.samePosition(in: winter.utf16))
  
  expectEqualSequence(
    summerBytes.map { UTF16.CodeUnit($0) },
    summer.unicodeScalars.indices.map {
      summer.utf16[$0.samePosition(in: summer.utf16)]
    }
  )

  expectEqual(
    summer.utf16.endIndex,
    summer.unicodeScalars.endIndex.samePosition(in: summer.utf16))
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
      $0.samePosition(in: winter.utf16).map {
        winter.utf16[$0]
      }
    }, sameValue: ==)

  expectNotNil(winter.utf8.endIndex.samePosition(in: winter.utf16))
  expectEqual(
    winter.utf16.endIndex,
    winter.utf8.endIndex.samePosition(in: winter.utf16)!)
  
  expectEqualSequence(
    summerBytes.map { UTF16.CodeUnit($0) },
    summer.utf8.indices.map { summer.utf16[$0.samePosition(in: summer.utf16)!] }
  )
  
  expectNotNil(summer.utf8.endIndex.samePosition(in: summer.utf16))
  expectEqual(
    summer.utf16.endIndex,
    summer.utf8.endIndex.samePosition(in: summer.utf16)!)
}

//===--- To UnicodeScalar -------------------------------------------------===//
tests.test("index-mapping/character-to-unicode-scalar") {
  let winterCharacterUnicodeScalars: [UnicodeScalar] = [
    UnicodeScalar(0x1f3c2)!,
    UnicodeScalar(0x2603)!,
    UnicodeScalar(0x2745)!,
    UnicodeScalar(0x2746)!,
    UnicodeScalar(0x2744)!, // 0xfe0e,
    UnicodeScalar(0x26c4)!, // 0xfe0f,
    UnicodeScalar(0x2744)!, // 0xfe0f
    replacementScalar, UnicodeScalar(0x20)!, replacementScalar, replacementScalar
  ]
  
  expectEqualSequence(
    winterCharacterUnicodeScalars,
    winter.characters.indices.map {
      winter.unicodeScalars[$0.samePosition(in: winter.unicodeScalars)]
    })

  expectEqual(winter.unicodeScalars.endIndex, winter.endIndex.samePosition(in: winter.unicodeScalars))
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) },
    summer.characters.indices.map { summer.unicodeScalars[$0.samePosition(in: summer.unicodeScalars)] }
  )
  
  expectEqual(summer.unicodeScalars.endIndex, summer.endIndex.samePosition(in: summer.unicodeScalars))
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
      i in i.samePosition(in: winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, sameValue: ==)

  expectNotNil(winter.utf8.endIndex.samePosition(in: winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf8.endIndex.samePosition(in: winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf8.indices.map {
      i in i.samePosition(in: summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, sameValue: ==)

  expectNotNil(summer.utf8.endIndex.samePosition(in: summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf8.endIndex.samePosition(in: summer.unicodeScalars)!)
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
      i in i.samePosition(in: winter.unicodeScalars).map {
        winter.unicodeScalars[$0]
      }
    }, sameValue: ==)

  expectNotNil(winter.utf16.endIndex.samePosition(in: winter.unicodeScalars))
  expectEqual(
    winter.unicodeScalars.endIndex,
    winter.utf16.endIndex.samePosition(in: winter.unicodeScalars)!)
  
  expectEqualSequence(
    summerBytes.map { UnicodeScalar($0) as UnicodeScalar? },
    summer.utf16.indices.map {
      i in i.samePosition(in: summer.unicodeScalars).map {
        summer.unicodeScalars[$0]
      }
    }, sameValue: ==)

  expectNotNil(summer.utf16.endIndex.samePosition(in: summer.unicodeScalars))
  expectEqual(
    summer.unicodeScalars.endIndex,
    summer.utf16.endIndex.samePosition(in: summer.unicodeScalars)!)
}

//===--- To Character -----------------------------------------------------===//
tests.test("index-mapping/unicode-scalar-to-character") {
  let winterUnicodeScalarCharacters: [Character?] = [
    "🏂", "☃", "❅", "❆", "❄︎", nil, "⛄️", nil, "❄️", nil,
    replacementCharacter, "\u{20}", replacementCharacter, replacementCharacter
  ]
  
  expectEqualSequence(
    winterUnicodeScalarCharacters,
    
    winter.unicodeScalars.indices.map {
      i in i.samePosition(in: winter).map {
        winter[$0]
      }
    }, sameValue: ==)

  expectEqual(winter.endIndex, winter.unicodeScalars.endIndex.samePosition(in: winter)!)
  
  expectEqualSequence(
    summerBytes.map { Character(UnicodeScalar($0)) },
    summer.unicodeScalars.indices.map { summer[$0.samePosition(in: summer)!] }
  )
  
  expectEqual(summer.endIndex, summer.unicodeScalars.endIndex.samePosition(in: summer)!)
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
      (i:String.UTF8Index) -> Character? in i.samePosition(in: winter).map {
        winter[$0]
      }
    }, sameValue: ==)

  expectNotNil(winter.utf8.endIndex.samePosition(in: winter))
  expectEqual(
    winter.endIndex,
    winter.utf8.endIndex.samePosition(in: winter)!)
  
  expectEqualSequence(
    summerBytes.map { Character(UnicodeScalar($0)) },
    summer.utf8.indices.map { summer[$0.samePosition(in: summer)!] }
  )

  expectNotNil(summer.utf8.endIndex.samePosition(in: summer))
  expectEqual(
    summer.endIndex,
    summer.utf8.endIndex.samePosition(in: summer)!)
}

tests.test("index-mapping/utf16-to-character") {
  let winterUtf16Characters: [Character?] = [
    "🏂", nil, "☃", "❅", "❆", "❄︎", nil, "⛄️", nil, "❄️", nil,
    replacementCharacter, "\u{20}", replacementCharacter, replacementCharacter
  ]
  
  expectEqualSequence(
    winterUtf16Characters,
    winter.utf16.indices.map {
      i in i.samePosition(in: winter).map {
        winter[$0]
      }
    }, sameValue: ==)

  expectNotNil(winter.utf16.endIndex.samePosition(in: winter))
  expectEqual(
    winter.endIndex,
    winter.utf16.endIndex.samePosition(in: winter)!)
  
  expectEqualSequence(
    summerBytes.map { Character(UnicodeScalar($0)) },
    summer.utf16.indices.map {
      summer[$0.samePosition(in: summer)!]
    }
  )

  expectNotNil(summer.utf16.endIndex.samePosition(in: summer))
  expectEqual(
    summer.endIndex,
    summer.utf16.endIndex.samePosition(in: summer)!)
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
  
  do {
    let start = String.UTF8Index(abc.startIndex, within: abc.utf8)
    expectEqual(
      abc.utf8.index(after: start),
      String.UTF8Index(abc.index(after: abc.startIndex), within: abc.utf8))
  }

  let diverseCharacters = summer + winter + winter + summer
  let s = diverseCharacters.unicodeScalars
  let u8 = diverseCharacters.utf8
  let u16 = diverseCharacters.utf16

  //===--- nested for...in loops ------------------------------------------===//
  // Test all valid subranges si0..<si1 of positions in s.  ds is
  // always si0.distance(to: si1)
  for si0 in s.indices {
    for (ds, si1) in s.indices[si0..<s.endIndex].enumerated() {
      
      // Map those unicode scalar indices into utf8 indices
      let u8i1 = si1.samePosition(in: u8)
      let u8i0 = si0.samePosition(in: u8)

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
          let u16i0a = u8i0a.samePosition(in: u16)!
           // we should be able to round-trip through UTF16
          expectEqual(u8i0a, u16i0a.samePosition(in: u8)!)

          if UTF16.isLeadSurrogate(u16[u16i0a]) {
            // We only have well-formed UTF16 in this string, so the
            // successor points to a trailing surrogate of a pair and
            // thus shouldn't convert to a UTF8 position
            expectNil(u16.index(after: u16i0a).samePosition(in: u8))
          }
          
          dsa = dsa.advanced(by: 1) // we're moving off the beginning of a new Unicode scalar
        }
        else {
          expectNil(u8i0a.samePosition(in: u16))
        }
        u8i0a = u8.index(u8i0a, offsetBy: 1)
      }

      expectEqual(u8i0a, u8i1) // We should be there now

      // Also check some UTF8 positions between unicode scalars for equality
      var u8i0b = u8i0a
      for n0 in 0..<8 {
        var u8i1b = u8i1
        for n1 in 0..<8 {
          expectEqualTest(u8i0b, u8i1b, sameValue: n0 == n1 ? (==) : (!=))
          if u8i1b == u8.endIndex { break }
          u8i1b = u8.index(u8i1b, offsetBy: 1)
        }
        if u8i0b == u8.endIndex { break }
        u8i0b = u8.index(u8i0b, offsetBy: 1)
      }
    }
  }
}

tests.test("index/Comparable")
  .forEach(in: [summer, winter]) { str in
  checkComparable(str.characters.indices, oracle: <=>)
  checkComparable(str.unicodeScalars.indices, oracle: <=>)
  checkComparable(str.utf16.indices, oracle: <=>)
  checkComparable(str.utf8.indices, oracle: <=>)
}

tests.test("UTF16->String") {
  let s = summer + winter + winter + summer
  let v = s.utf16
  for i in v.indices {
    for j in v.indices[i..<v.endIndex] {
      if let si = i.samePosition(in: s) {
        if let sj = j.samePosition(in: s) {
          expectEqual(s[si..<sj], String(v[i..<j])!)
          continue
        }
      }
      expectNil(String(v[i..<j]))
    }
  }
}

tests.test("UTF8->String") {
  let s = summer + winter + winter + summer
  let v = s.utf8
  for i in v.indices {
    for j in v.indices[i..<v.endIndex] {
      if let si = i.samePosition(in: s) {
        if let sj = j.samePosition(in: s) {
          expectEqual(s[si..<sj], String(v[i..<j])!)
          continue
        }
      }
      expectNil(String(v[i..<j]))
    }
  }
}

tests.test("UnicodeScalars->String") {
  let s = summer + winter + winter + summer
  let v = s.unicodeScalars
  for i in s.characters.indices {
    for j in s.characters.indices[i..<s.endIndex] {
      expectEqual(
        s[i..<j],
        String(v[i.samePosition(in: v)..<j.samePosition(in: v)])
      )
    }
  }
}

#if _runtime(_ObjC)
tests.test("String.UTF16View.Index/Strideable")
  .forEach(in: utfTests) {
  test in

  func allIndices<C : Collection>(of c: C) -> [C.Index]
  where C.Indices.Iterator.Element == C.Index
  {
    var result = Array(c.indices)
    result.append(c.endIndex)
    return result
  }

  checkStrideable(
    instances: allIndices(of: test.string.utf16),
    distances: Array(0..<test.string.utf16.count),
    distanceOracle: { $1 - $0 })
}
#endif

tests.test("String.UTF8View/Collection")
  .forEach(in: utfTests) {
  test in

  // FIXME(ABI)#72 : should be `checkBidirectionalCollection`.
  checkForwardCollection(test.utf8, test.string.utf8) { $0 == $1 }
}

#if _runtime(_Native)
tests.test("String.UTF16View/BidirectionalCollection")
  .forEach(in: utfTests) {
  test in

  checkBidirectionalCollection(test.utf16, test.string.utf16) { $0 == $1 }
}
#else
tests.test("String.UTF16View/RandomAccessCollection")
  .forEach(in: utfTests) {
  test in

  checkRandomAccessCollection(test.utf16, test.string.utf16) { $0 == $1 }
}
#endif

tests.test("String.UTF32View/BidirectionalCollection")
  .forEach(in: utfTests) {
  test in

  checkBidirectionalCollection(
    test.unicodeScalars, test.string.unicodeScalars) { $0 == $1 }
}

runAllTests()
