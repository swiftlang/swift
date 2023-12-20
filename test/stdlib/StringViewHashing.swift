// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: reflection

//
// Tests for String view hashability
//

import StdlibUnittest

#if _runtime(_ObjC)
import Foundation
#endif

var StringViewHashingTests = TestSuite("StringViewHashingTests")

let taggedNSString = "dog" as NSString //up to 11 ascii bytes can be stored in a tagged NSString
let asciiNSString = "The quick brown fox jumps over the lazy dog" as NSString
let nonAsciiNSString = "The quick brown fox jumps over the lazy dög" as NSString
let testStrings = [
  "" /* empty */,
  "dog" /* short ascii */,
  "dög" /* short non-ascii*/,
  "The quick brown fox jumps over the lazy dog" /* long ascii (long is defined as >15 bytes) */,
  "The quick brown fox jumps over the lazy dög" /* long non-ascii */,
  "👩‍👧" /* short multi-scalar */,
  "👩‍👩‍👧‍👧" /* long multi-scalar */,
  taggedNSString as String,
  asciiNSString as String,
  nonAsciiNSString as String
]

let testSubstrings = testStrings.map { $0.prefix($0.count)) }

if #available(SwiftStdlib 5.7, *) {
  StringViewHashingTests.test("StringViewEquatableHashable") {
    testStrings.forEach { string in
      expectEqual(string.utf8, string.utf8)
      expectEqual(string.utf16, string.utf16)
      expectEqual(string.unicodeScalars, string.unicodeScalars)

      checkHashable(string.utf8, equalityOracle: { $0 == $1 })
      checkHashable(string.utf16, equalityOracle: { $0 == $1 })
      checkHashable(string.unicodeScalars, equalityOracle: { $0 == $1 })
    }

    testSubstrings.forEach { substring in
      expectEqual(substring.utf8, substring.utf8)
      expectEqual(substring.utf16, substring.utf16)
      expectEqual(substring.unicodeScalars, substring.unicodeScalars)

      checkHashable(substring.utf8, equalityOracle: { $0 == $1 })
      checkHashable(substring.utf16, equalityOracle: { $0 == $1 })
      checkHashable(substring.unicodeScalars, equalityOracle: { $0 == $1 })
    }
  }
}

runAllTests()
