// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: long_test

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif

var suite = TestSuite("StringIndexLong")
defer { runAllTests() }

let _examples: [StaticString] = [
  "abc\r\ndefg",
  "a\r\ncдe\u{301}日🧟‍♀️x🏳️‍🌈🇺🇸🇨🇦",
]

let examples: [String] = _examples.flatMap { s in
  let str = "\(s)"
  #if _runtime(_ObjC)
  let unichars = Array(str.utf16)
  let nsstr = NSString(characters: unichars, length: unichars.count)
  return [str, nsstr as String]
  #else
  return [str]
  #endif
}

suite.test("Substring.replaceSubrange index validation")
.forEach(in: examples) { string in
  guard #available(SwiftStdlib 5.7, *) else {
    // Index navigation in 5.7 always rounds input indices down to the nearest
    // Character, so that we always have a well-defined distance between
    // indices, even if they aren't valid.
    //
    // 5.6 and below did not behave consistently in this case.
    return
  }

  string.dumpIndices()

  let scalarMap = string.scalarMap()
  let allIndices = string.allIndices()

  for i in allIndices {
    print(i)
    for j in allIndices {
      guard i <= j else { continue }
      let si = scalarMap[i]!.index
      let sj = scalarMap[j]!.index

      let substring = string[i ..< j]

      let subindices = substring.allIndices()
      for m in subindices {
        for n in subindices {
          guard m <= n else { continue }
          let sm = scalarMap[m]!.index
          let sn = scalarMap[n]!.index

          let replacement = "x"

          var _expected = "".unicodeScalars
          _expected += string.unicodeScalars[si ..< sm]
          _expected += replacement.unicodeScalars
          _expected += string.unicodeScalars[sn ..< sj]
          let expected = String(_expected)[...]

          // Check Substring.replaceSubrange(_:with:)
          do {
            var actual = substring
            actual.replaceSubrange(m ..< n, with: Array(replacement))

            expectEqual(actual, expected,
              """
              string: \(string.debugDescription)
              i:      \(i)
              j:      \(j)
              m:      \(m)
              n:      \(n)
              """)
          }

          // Check String.unicodeScalars.replaceSubrange(_:with:)
          do {
            var actual = substring
            actual.unicodeScalars.replaceSubrange(
              m ..< n, with: Array(replacement.unicodeScalars))

            expectEqual(actual, expected,
              """
              string: \(string.debugDescription)
              i:      \(i)
              j:      \(j)
              m:      \(m)
              n:      \(n)
              """)
          }
        }
      }
    }
  }
}
