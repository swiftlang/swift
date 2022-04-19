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

let testString: String = "dogs"
let UTF8View = testString.utf8
let UTF16View = testString.utf16
let unicodeScalarView = testString.unicodeScalars

let testSubstring = testString.suffix(3)
let substringUTF8View = testSubstring.utf8
let substringUTF16View = testSubstring.utf16
let substringUnicodeScalarView = testSubstring.unicodeScalars

if #available(SwiftStdlib 5.7, *) {
    StringViewHashingTests.test("StringViewEquatable") {
        expectEqual(UTF8View, UTF8View)
        expectEqual(UTF16View, UTF16View)
        expectEqual(unicodeScalarView, unicodeScalarView)
        expectEqual(substringUTF8View, substringUTF8View)
        expectEqual(substringUTF16View, substringUTF16View)
        expectEqual(substringUnicodeScalarView, substringUnicodeScalarView)
    }

    StringViewHashingTests.test("StringViewHashable") {
        checkHashable(UTF8View, equalityOracle: { $0 == $1 })
        checkHashable(UTF16View, equalityOracle: { $0 == $1 })
        checkHashable(unicodeScalarView, equalityOracle: { $0 == $1 })
        checkHashable(substringUTF8View, equalityOracle: { $0 == $1 })
        checkHashable(substringUTF16View, equalityOracle: { $0 == $1 })
        checkHashable(substringUnicodeScalarView, equalityOracle: { $0 == $1 })
    }
}

runAllTests()
