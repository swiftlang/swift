// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// UNSUPPORTED: freestanding

import StdlibUnittest
#if _runtime(_ObjC)
import Foundation
#endif

var StringIndexTests = TestSuite("StringIndexTests")

enum SimpleString: String {
  case smallASCII = "abcdefg"
  case smallUnicode = "abéÏ𓀀"
  case largeASCII = "012345678901234567890"
  case largeUnicode = "abéÏ012345678901234567890𓀀"
  case emoji = "😀😃🤢🤮👩🏿‍🎤🧛🏻‍♂️🧛🏻‍♂️👩‍👩‍👦‍👦"
}

let simpleStrings: [String] = [
  SimpleString.smallASCII.rawValue,
  SimpleString.smallUnicode.rawValue,
  SimpleString.largeASCII.rawValue,
  SimpleString.largeUnicode.rawValue,
  SimpleString.emoji.rawValue,
  "",
]

func dumpIndices(_ string: String) {
  print("-------------------------------------------------------------------")
  print("String: \(String(reflecting: string))")
  print("Characters:")
  string.indices.forEach { i in
    let char = string[i]
    print("  \(i) -> \(String(reflecting: char))")
  }
  print("Unicode Scalars:")
  string.unicodeScalars.indices.forEach { i in
    let scalar = string.unicodeScalars[i]
    let value = String(scalar.value, radix: 16, uppercase: true)
    let padding = String(repeating: "0", count: max(0, 4 - value.count))
    let name = scalar.properties.name ?? "\(scalar.debugDescription)"
    print("  \(i) -> U+\(padding)\(value) \(name)")
  }
  print("UTF-8:")
  string.utf8.indices.forEach { i in
    let code = string.utf8[i]
    let value = String(code, radix: 16, uppercase: true)
    let padding = value.count < 2 ? "0" : ""
    print("  \(i) -> \(padding)\(value)")
  }
  print("UTF-16:")
  string.utf16.indices.forEach { i in
    let code = string.utf16[i]
    let value = String(code, radix: 16, uppercase: true)
    let padding = String(repeating: "0", count: 4 - value.count)
    print("  \(i) -> \(padding)\(value)")
  }
}

StringIndexTests.test("basic sanity checks") {
  for s in simpleStrings {
    let utf8 = Array(s.utf8)
    let subUTF8 = Array(s[...].utf8)
    let utf16 = Array(s.utf16)
    let subUTF16 = Array(s[...].utf16)
    let utf32 = Array(s.unicodeScalars.map { $0.value })
    let subUTF32 = Array(s[...].unicodeScalars.map { $0.value })

    expectEqual(s, String(decoding: utf8, as: UTF8.self))
    expectEqual(s, String(decoding: subUTF8, as: UTF8.self))
    expectEqual(s, String(decoding: utf16, as: UTF16.self))
    expectEqual(s, String(decoding: subUTF16, as: UTF16.self))
    expectEqual(s, String(decoding: utf32, as: UTF32.self))
    expectEqual(s, String(decoding: subUTF32, as: UTF32.self))
  }
}

StringIndexTests.test("view counts") {
  func validateViewCount<View: BidirectionalCollection>(
    _ view: View, for string: String,
    stackTrace: SourceLocStack = SourceLocStack(),
    showFrame: Bool = true,
    file: String = #file, line: UInt = #line
  ) where View.Element: Equatable, View.Index == String.Index {

    let stackTrace = stackTrace.pushIf(showFrame, file: file, line: line)

    let count = view.count
    func expect(_ i: Int,
      file: String = #file, line: UInt = #line
    ) {
      expectEqual(count, i, "for String: \(string)",
        stackTrace: stackTrace.pushIf(showFrame, file: file, line: line),
        showFrame: false)
    }

    let reversedView = view.reversed()

    expect(Array(view).count)
    expect(view.indices.count)
    expect(view.indices.reversed().count)
    expect(reversedView.indices.count)
    expect(view.distance(from: view.startIndex, to: view.endIndex))
    expect(reversedView.distance(
      from: reversedView.startIndex, to: reversedView.endIndex))

    // Access the elements from the indices
    expectEqual(Array(view), view.indices.map { view[$0] })
    expectEqual(
      Array(reversedView), reversedView.indices.map { reversedView[$0] })

    let indicesArray = Array<String.Index>(view.indices)
    for i in 0..<indicesArray.count {
      var idx = view.startIndex
      idx = view.index(idx, offsetBy: i)
      expectEqual(indicesArray[i], idx)
    }
  }

  for s in simpleStrings {
    validateViewCount(s, for: s)
    validateViewCount(s.utf8, for: s)
    validateViewCount(s.utf16, for: s)
    validateViewCount(s.unicodeScalars, for: s)

    validateViewCount(s[...], for: s)
    validateViewCount(s[...].utf8, for: s)
    validateViewCount(s[...].utf16, for: s)
    validateViewCount(s[...].unicodeScalars, for: s)
  }
}

StringIndexTests.test("interchange") {
  // Basic index alignment

  func validateIndices(_ s: String) {
    for idx in s.utf8.indices {
      let char = s.utf8[idx]

      // ASCII or leading code unit in the scalar
      if char <= 0x7F || char >= 0b1100_0000 {
        expectEqual(idx, idx.samePosition(in: s.unicodeScalars))
        expectEqual(idx, idx.samePosition(in: s.utf16))

        // ASCII
        if char <= 0x7F {
          expectEqual(UInt16(char), s.utf16[idx])
          expectEqual(UInt32(char), s.unicodeScalars[idx].value)
        }
      } else {
        // Continuation code unit
        assert(char & 0b1100_0000 == 0b1000_0000)
        expectNil(idx.samePosition(in: s))
        expectNil(idx.samePosition(in: s.utf16))
        expectNil(idx.samePosition(in: s.unicodeScalars))
      }
    }
  }

  for s in simpleStrings {
    validateIndices(s)
  }
}

StringIndexTests.test("UTF-16 Offsets") {
  func validateOffsets(_ s: String) {
    let end = s.endIndex
    let utf16Count = s.utf16.count

    expectEqual(end, String.Index(utf16Offset: utf16Count, in: s))
    expectEqual(end, String.Index(utf16Offset: utf16Count, in: s[...]))

    let pastEnd = String.Index(utf16Offset: utf16Count+1, in: s)

    expectNotEqual(end, pastEnd)
    expectEqual(pastEnd, String.Index(utf16Offset: utf16Count+1, in: s[...]))
    expectEqual(pastEnd, String.Index(utf16Offset: utf16Count+2, in: s))
    expectEqual(pastEnd, String.Index(utf16Offset: -1, in: s))
    expectEqual(
      pastEnd, String.Index(utf16Offset: Swift.max(1, utf16Count), in: s.dropFirst()))

    let utf16Indices = Array(s.utf16.indices)
    expectEqual(utf16Count, utf16Indices.count)
    for i in 0..<utf16Indices.count {
      let idx = String.Index(utf16Offset: i, in: s)
      expectEqual(utf16Indices[i], idx)
      expectEqual(i, idx.utf16Offset(in: s))
      expectEqual(i, idx.utf16Offset(in: s[...]))

      if i < s.dropLast().utf16.count {
        expectEqual(
          utf16Indices[i], String.Index(utf16Offset: i, in: s.dropLast()))
        expectEqual(i, idx.utf16Offset(in: s.dropLast()))
      } else if i == s.dropLast().utf16.count {
        expectEqual(
          utf16Indices[i], String.Index(utf16Offset: i, in: s.dropLast()))
      } else {
        expectNotEqual(
          utf16Indices[i], String.Index(utf16Offset: i, in: s.dropLast()))
      }
    }
  }

  for s in simpleStrings {
    validateOffsets(s)
  }
}

func swift5ScalarAlign(_ idx: String.Index, in str: String) -> String.Index {
  var idx = idx
  while str.utf8[idx] & 0xC0 == 0x80 { str.utf8.formIndex(before: &idx) }
  return idx
}

StringIndexTests.test("Scalar Align UTF-8 indices") {
  // TODO: Test a new aligning API when we add it. For now, we
  // test scalar-aligning UTF-8 indices

  let str = "a😇"
  let subScalarIdx = str.utf8.index(str.utf8.startIndex, offsetBy: 2)

  let roundedIdx = swift5ScalarAlign(subScalarIdx, in: str)
  expectEqual(1, roundedIdx.utf16Offset(in: str))

  let roundedIdx2 = str.utf8[...subScalarIdx].lastIndex { $0 & 0xC0 != 0x80 }
  expectEqual(roundedIdx, roundedIdx2)

  var roundedIdx3 = subScalarIdx
  while roundedIdx3.samePosition(in: str.unicodeScalars) == nil {
    str.utf8.formIndex(before: &roundedIdx3)
  }
  expectEqual(roundedIdx, roundedIdx3)
}

#if _runtime(_ObjC)
StringIndexTests.test("String.Index(_:within) / Range<String.Index>(_:in:)") {
  guard #available(SwiftStdlib 5.1, *) else {
    return
  }

  let str = simpleStrings.joined()
  let substr = str[...]
  for idx in str.utf8.indices {
    expectEqual(
      String.Index(idx, within: str), String.Index(idx, within: substr))
  }

  expectNil(String.Index(str.startIndex, within: str.dropFirst()))
  expectNil(String.Index(str.endIndex, within: str.dropLast()))
  expectNotNil(String.Index(str.startIndex, within: str))
  expectNotNil(String.Index(str.endIndex, within: str))

  let utf16Count = str.utf16.count
  let utf16Indices = Array(str.utf16.indices) + [str.utf16.endIndex]
  for location in 0..<utf16Count {
    for length in 0...(utf16Count - location) {
      let strLB = String.Index(utf16Indices[location], within: str)
      let substrLB = String.Index(utf16Indices[location], within: substr)
      let strUB = String.Index(utf16Indices[location+length], within: str)
      let substrUB = String.Index(utf16Indices[location+length], within: substr)
      expectEqual(strLB, substrLB)
      expectEqual(strUB, substrUB)

      let nsRange = NSRange(location: location, length: length)
      let strRange = Range<String.Index>(nsRange, in: str)
      let substrRange = Range<String.Index>(nsRange, in: substr)

      expectEqual(strRange, substrRange)
      guard strLB != nil && strUB != nil else {
        expectNil(strRange)
        continue
      }
      expectEqual(strRange, Range(uncheckedBounds: (strLB!, strUB!)))
    }
  }
}

StringIndexTests.test("Misaligned") {
  // Misaligned indices were fixed in 5.1
  guard _hasSwift_5_1() else { return }

  func doIt(_ str: String) {
    let characterIndices = Array(str.indices)
    let scalarIndices = Array(str.unicodeScalars.indices) + [str.endIndex]
    let utf8Indices = Array(str.utf8.indices)

    var lastScalarI = 0
    for i in 1..<utf8Indices.count {
      let idx = utf8Indices[i]

      // Skip aligned indices
      guard idx < scalarIndices[lastScalarI + 1] else {
        assert(idx == scalarIndices[lastScalarI + 1])
        lastScalarI += 1
        continue
      }
      expectTrue(UTF8.isContinuation(str.utf8[idx]))

      let lastScalarIdx = scalarIndices[lastScalarI]

      // Check aligning-down
      expectEqual(str[lastScalarIdx], str[idx])
      expectEqual(str.utf16[lastScalarIdx], str.utf16[idx])
      expectEqual(str.unicodeScalars[lastScalarIdx], str.unicodeScalars[idx])

      // Check distance
      let (start, end) = (str.startIndex, str.endIndex)
      if characterIndices.contains(lastScalarIdx) {
        expectEqual(0, str.distance(from: lastScalarIdx, to: idx))
        expectEqual(str[..<idx].count, str.distance(from: start, to: idx))
        expectEqual(str[idx...].count, str.distance(from: idx, to: end))
      }
      expectEqual(
        0, str.unicodeScalars.distance(from: lastScalarIdx, to: idx))
      expectEqual(
        str.unicodeScalars[..<idx].count,
        str.unicodeScalars.distance(from: start, to: idx))
      expectEqual(
        str.unicodeScalars[idx...].count,
        str.unicodeScalars.distance(from: idx, to: end))

      expectEqual(0, str.utf16.distance(from: lastScalarIdx, to: idx))
      expectEqual(
        str.utf16[..<idx].count, str.utf16.distance(from: start, to: idx))
      expectEqual(
        str.utf16[idx...].count, str.utf16.distance(from: idx, to: end))
    }
  }

  let nsstring: NSString = "aодиde\u{301}日🧟‍♀️"
  doIt(nsstring as String)

  let string = "aодиde\u{301}日🧟‍♀️"
  doIt(string)
}

StringIndexTests.test("Exhaustive Index Interchange") {
  // Exhaustively test aspects of string index interchange
  func testInterchange(
    _ str: String,
    stackTrace: SourceLocStack = SourceLocStack(),
    showFrame: Bool = true,
    file: String = #file,
    line: UInt = #line
  ) {
    guard #available(SwiftStdlib 5.1, *) else {
      return
    }

    let stackTrace = stackTrace.pushIf(showFrame, file: file, line: line)
    func expect(
      _ condition: @autoclosure () -> Bool,
      _ message: String = "",
      file: String = #file,
      line: UInt = #line
    ) {
      expectTrue(condition(), message,
        stackTrace: stackTrace, showFrame: showFrame,
        file: file, line: line)
    }

    var curCharIdx = str.startIndex
    var curScalarIdx = str.startIndex
    var curUTF8Idx = str.startIndex
    var curUTF16Idx = str.startIndex

    while curCharIdx < str.endIndex {
      let curChar = str[curCharIdx]
      expect(curChar == str[curScalarIdx])
      expect(curChar == str[curUTF8Idx])
      expect(curChar == str[curUTF16Idx])

      // Advance the character index once and have the scalar index catch up
      str.formIndex(after: &curCharIdx)

      let scalarStartIdx = curScalarIdx
      defer {
        let sub = str[scalarStartIdx..<curScalarIdx]
        expect(sub.count == 1)
        expect(sub.first! == curChar)
        expect(str.distance(from: scalarStartIdx, to: curScalarIdx) == 1)
      }

      while curScalarIdx < curCharIdx {
        let scalarStartIdx = curScalarIdx
        let curScalar = str.unicodeScalars[curScalarIdx]
        let curSubChar = str[curScalarIdx]

        // If there is a Character prior to this scalar, remember it and check
        // that misalignd code unit indices also produce it.
        let scalarPriorCharacter: Character?
        if scalarStartIdx == str.startIndex {
          scalarPriorCharacter = nil
        } else {
          scalarPriorCharacter = str[str.index(before: scalarStartIdx)]
        }

        // Advance the scalar index once and have the code unit indices catch up
        str.unicodeScalars.formIndex(after: &curScalarIdx)


        let utf8StartIdx = curUTF8Idx
        defer {
          let sub = str.unicodeScalars[utf8StartIdx..<curUTF8Idx]
          expect(sub.count == 1)
          expect(sub.first! == curScalar)
          expect(str.unicodeScalars.distance(
            from: utf8StartIdx, to: curUTF8Idx) == 1)
          expect(str.utf8.distance(
            from: utf8StartIdx, to: curUTF8Idx) == curScalar.utf8.count)
        }

        while curUTF8Idx < curScalarIdx {
          expect(curScalar == str.unicodeScalars[curUTF8Idx])
          expect(curSubChar == str[curUTF8Idx])
          expect(!UTF16.isTrailSurrogate(str.utf16[curUTF8Idx]))
          expect(utf8StartIdx == str[curUTF8Idx...].startIndex)
          expect(str[utf8StartIdx..<curUTF8Idx].isEmpty)
          expect(0 == str.utf16.distance(from: utf8StartIdx, to: curUTF8Idx))

          if let scalarPrior = scalarPriorCharacter {
            expect(scalarPrior == str[str.index(before: curUTF8Idx)])
          }

          str.utf8.formIndex(after: &curUTF8Idx)
        }
        expect(curUTF8Idx == curScalarIdx)

        var utf8RevIdx = curUTF8Idx
        while utf8RevIdx > utf8StartIdx {
          str.utf8.formIndex(before: &utf8RevIdx)

          expect(curScalar == str.unicodeScalars[utf8RevIdx])
          expect(curSubChar == str[utf8RevIdx])
          expect(!UTF16.isTrailSurrogate(str.utf16[utf8RevIdx]))
          expect(utf8StartIdx == str[utf8RevIdx...].startIndex)
          expect(str[utf8StartIdx..<utf8RevIdx].isEmpty)
          expect(0 == str.utf16.distance(from: utf8StartIdx, to: utf8RevIdx))
        }
        expect(utf8RevIdx == utf8StartIdx)

        let utf16StartIdx = curUTF16Idx
        defer {
          let sub = str.unicodeScalars[utf16StartIdx..<curUTF16Idx]
          expect(sub.count == 1)
          expect(sub.first! == curScalar)
          expect(str.unicodeScalars.distance(
            from: utf16StartIdx, to: curUTF16Idx) == 1)
          expect(str.utf16.distance(
            from: utf16StartIdx, to: curUTF16Idx) == curScalar.utf16.count)
        }

        while curUTF16Idx < curScalarIdx {
          expect(curScalar == str.unicodeScalars[curUTF16Idx])
          expect(curSubChar == str[curUTF16Idx])
          expect(!UTF8.isContinuation(str.utf8[curUTF16Idx]))
          expect(utf16StartIdx == str[curUTF16Idx...].startIndex)
          expect(str[utf16StartIdx..<curUTF16Idx].isEmpty)
          expect(0 == str.utf8.distance(from: utf16StartIdx, to: curUTF16Idx))

          if let scalarPrior = scalarPriorCharacter {
            expect(scalarPrior == str[str.index(before: curUTF16Idx)])
          }

          str.utf16.formIndex(after: &curUTF16Idx)
        }
        expect(curUTF16Idx == curScalarIdx)

        var utf16RevIdx = curUTF16Idx
        while utf16RevIdx > utf16StartIdx {
          str.utf16.formIndex(before: &utf16RevIdx)

          expect(curScalar == str.unicodeScalars[utf16RevIdx])
          expect(curSubChar == str[utf16RevIdx])
          expect(!UTF8.isContinuation(str.utf8[utf16RevIdx]))
          expect(utf16StartIdx == str[utf16RevIdx...].startIndex)
          expect(str[utf16StartIdx..<utf16RevIdx].isEmpty)
          expect(0 == str.utf8.distance(from: utf16StartIdx, to: utf16RevIdx))
        }
        expect(utf16RevIdx == utf16StartIdx)

      }
    }
  }

  testInterchange("abc\r\ndefg")

#if _runtime(_ObjC)
  testInterchange(("abc\r\ndefg" as NSString) as String)
#endif // _runtime(_ObjC)

  testInterchange("ab\r\ncдe\u{301}日🧟‍♀️x🧟x🏳️‍🌈🇺🇸🇨🇦")

#if _runtime(_ObjC)
  testInterchange(("ab\r\ncдe\u{301}日🧟‍♀️x🧟x🏳️‍🌈🇺🇸🇨🇦" as NSString) as String)
#endif // _runtime(_ObjC)
}
#endif

extension Collection {
  // Assuming both `self` and `other` are sorted, call `body` for each element
  // `a` in `other` together with the slice in `self` that starts with the first
  // element in `self` that is greater than or equal to `a`, up to the first
  // element that is greater than or equal to the next value in `other`.
  //
  // `other` must start with an item that is less than or equal to the first
  // item in `self`.
  func forEachIndexGroup<G: Collection>(
    by other: G,
    body: (G.Index, Self.SubSequence, Int) throws -> Void
  ) rethrows
  where G.Index == Self.Index
  {
    if other.isEmpty {
      assert(self.isEmpty)
      return
    }
    var i = other.startIndex
    var j = self.startIndex
    var offset = 0
    while i != other.endIndex {
      let current = i
      other.formIndex(after: &i)
      let start = j
      while j < i, j < self.endIndex {
        self.formIndex(after: &j)
      }
      let end = j
      try body(current, self[start ..< end], offset)
      offset += 1
    }
  }
}

extension String {
  /// Returns a dictionary mapping each valid index to the index that lies on
  /// the nearest scalar boundary, rounding down.
  func scalarMap() -> [String.Index: (index: String.Index, offset: Int)] {
    var map: [String.Index: (index: String.Index, offset: Int)] = [:]

    self.utf8.forEachIndexGroup(by: self.unicodeScalars) { scalar, slice, offset in
      for i in slice.indices { map[i] = (scalar, offset) }
    }
    self.utf16.forEachIndexGroup(by: self.unicodeScalars) { scalar, slice, offset in
      for i in slice.indices { map[i] = (scalar, offset) }
    }
    self.forEachIndexGroup(by: self.unicodeScalars) { scalar, slice, offset in
      for i in slice.indices { map[i] = (scalar, offset) }
    }
    map[endIndex] = (endIndex, self.unicodeScalars.count)
    return map
  }

  /// Returns a dictionary mapping each valid index to the index that lies on
  /// the nearest character boundary, rounding down.
  func characterMap() -> [String.Index: (index: String.Index, offset: Int)] {
    var map: [String.Index: (index: String.Index, offset: Int)] = [:]
    self.utf8.forEachIndexGroup(by: self) { char, slice, offset in
      for i in slice.indices { map[i] = (char, offset) }
    }
    self.utf16.forEachIndexGroup(by: self) { char, slice, offset in
      for i in slice.indices { map[i] = (char, offset) }
    }
    self.unicodeScalars.forEachIndexGroup(by: self) { char, slice, offset in
      for i in slice.indices { map[i] = (char, offset) }
    }
    map[endIndex] = (endIndex, count)
    return map
  }
}

StringIndexTests.test("Extra Exhaustive Index Interchange") {
  guard #available(SwiftStdlib 5.7, *) else {
    // Index navigation in 5.7 always rounds input indices down to the nearest
    // Character, so that we always have a well-defined distance between
    // indices, even if they aren't valid.
    //
    // 5.6 and below did not behave consistently in this case.
    return
  }

  func check(
    _ string: String,
    stackTrace: SourceLocStack = SourceLocStack(),
    showFrame: Bool = true,
    file: String = #file,
    line: UInt = #line
  ) {
    dumpIndices(string)

    let scalarMap = string.scalarMap()
    let characterMap = string.characterMap()

    // This is a list of every valid index in every string view, including end
    // indices. We keep equal indices because they may have different grapheme
    // size caches or flags etc.
    var allIndices = Array(string.indices) + [string.endIndex]
    allIndices += Array(string.unicodeScalars.indices) + [string.unicodeScalars.endIndex]
    allIndices += Array(string.utf8.indices) + [string.utf8.endIndex]
    allIndices += Array(string.utf16.indices) + [string.utf16.endIndex]

    func referenceCharacterDistance(
      from i: String.Index, to j: String.Index
    ) -> Int {
      let ci = characterMap[i]!
      let cj = characterMap[j]!
      return cj.offset - ci.offset
    }

    func referenceScalarDistance(
      from i: String.Index, to j: String.Index
    ) -> Int {
      let si = scalarMap[i]!
      let sj = scalarMap[j]!
      return sj.offset - si.offset
    }

    for i in allIndices {
      for j in allIndices {
        let characterDistance = referenceCharacterDistance(from: i, to: j)
        let scalarDistance = referenceScalarDistance(from: i, to: j)

        let substringDistance: Int
        if i <= j {
          // The substring `string[i..<j]` does not round its bounds to
          // `Character` boundaries, so it may have a different count than what
          // the base string reports.
          let s = String.UnicodeScalarView(string.unicodeScalars[i ..< j])
          substringDistance = String(s).count
        } else {
          substringDistance = -1
        }

        // Check distance calculations.
        if #available(SwiftStdlib 5.7, *) {
          expectEqual(
            string.distance(from: i, to: j),
            characterDistance,
            """
            string: \(string.debugDescription)
            i:      \(i)
            j:      \(j)
            """)
          if i <= j {
            expectEqual(string[i ..< j].count, substringDistance,
              """
              string: \(string.debugDescription)
              i:      \(i)
              j:      \(j)
              """)
          }
        }

        expectEqual(
          string.unicodeScalars.distance(from: i, to: j),
          scalarDistance,
          """
          string: \(string.debugDescription)
          i:      \(i)
          j:      \(j)
          """)
        if i <= j {
          // The `Character` alignment consideration above doesn't apply to
          // Unicode scalars in a substring.
          expectEqual(string.unicodeScalars[i ..< j].count, scalarDistance,
            """
            string: \(string.debugDescription)
            i:      \(i)
            j:      \(j)
            """)
        }

        // Check reachability of substring bounds.
        if i <= j {
          if #available(SwiftStdlib 5.7, *) {
            let substring = string[i ..< j]
            expectEqual(
              substring.index(substring.startIndex, offsetBy: substringDistance),
              substring.endIndex,
              """
              string:   \(string.debugDescription)
              i:        \(i)
              j:        \(j)
              distance: \(characterDistance)
              """)
            expectEqual(
              substring.index(substring.endIndex, offsetBy: -substringDistance),
              substring.startIndex,
              """
              string:   \(string.debugDescription)
              i:        \(i)
              j:        \(j)
              distance: \(-characterDistance)
              """)
          }
          let subscalars = string.unicodeScalars[i ..< j]
          expectEqual(
            subscalars.index(subscalars.startIndex, offsetBy: scalarDistance),
            subscalars.endIndex,
            """
            string:   \(string.debugDescription)
            i:        \(i)
            j:        \(j)
            distance: \(scalarDistance)
            """)
          expectEqual(
            subscalars.index(subscalars.endIndex, offsetBy: -scalarDistance),
            subscalars.startIndex,
            """
            string:   \(string.debugDescription)
            i:        \(i)
            j:        \(j)
            distance: \(-scalarDistance)
            """)
        }
      }
    }

    // Check `String.index(_:offsetBy:limitedBy:)`.
    if #available(SwiftStdlib 5.7, *) {
      for i in allIndices {
        for j in string.indices + [string.endIndex] { // End on a char boundary
          let distance = referenceCharacterDistance(from: i, to: j)
          for limit in allIndices {
            let expectHit = (
              distance > 0 && i <= limit && j > limit ? true
              : distance < 0 && i >= limit && j < limit ? true
              : false)
            expectEqual(
              string.index(i, offsetBy: distance, limitedBy: limit),
              expectHit ? nil : j,
              """
              string: \(string.debugDescription)
              i:      \(i)
              j:      \(j)   (distance: \(distance))
              limit:  \(limit)
              """)
          }
        }
      }
    }
  }

  let strings: [StaticString] = [
    "abc\r\ndefg",
    "ab\r\ncдe\u{301}日🧟‍♀️x🧟x🏳️‍🌈🇺🇸🇨🇦",
  ]

  for s in strings {
    let str = "\(s)"
    check(str)

    #if _runtime(_ObjC)
    let unichars = Array(str.utf16)
    let nsstr = NSString(characters: unichars, length: unichars.count)
    check(nsstr as String)
    #endif
  }
}

StringIndexTests.test("Global vs local grapheme cluster boundaries") {
  guard #available(SwiftStdlib 5.7, *) else {
    // Index navigation in 5.7 always rounds input indices down to the nearest
    // Character, so that we always have a well-defined distance between
    // indices, even if they aren't valid.
    //
    // 5.6 and below did not behave consistently in this case.
    return
  }

  let str = "a🇺🇸🇨🇦b"
  // U+0061 LATIN SMALL LETTER A
  // U+1F1FA REGIONAL INDICATOR SYMBOL LETTER U
  // U+1F1F8 REGIONAL INDICATOR SYMBOL LETTER S
  // U+1F1E8 REGIONAL INDICATOR SYMBOL LETTER C
  // U+1F1E6 REGIONAL INDICATOR SYMBOL LETTER A
  // U+0062 LATIN SMALL LETTER B

  let c = Array(str.indices) + [str.endIndex]
  let s = Array(str.unicodeScalars.indices) + [str.unicodeScalars.endIndex]
  let u8 = Array(str.utf8.indices) + [str.utf8.endIndex]
  let u16 = Array(str.utf16.indices) + [str.utf16.endIndex]

  // Index navigation must always round the input index down to the nearest
  // Character.

  expectEqual(str.count, 4)
  expectEqual(str.index(after: c[0]), c[1])
  expectEqual(str.index(after: c[1]), c[2])
  expectEqual(str.index(after: c[2]), c[3])
  expectEqual(str.index(after: c[3]), c[4])

  expectEqual(str.index(before: c[4]), c[3])
  expectEqual(str.index(before: c[3]), c[2])
  expectEqual(str.index(before: c[2]), c[1])
  expectEqual(str.index(before: c[1]), c[0])

  // Scalars
  expectEqual(str.unicodeScalars.count, 6)
  expectEqual(str.index(after: s[0]), s[1])
  expectEqual(str.index(after: s[1]), s[3])
  expectEqual(str.index(after: s[2]), s[3]) // s[2] ≅ s[1]
  expectEqual(str.index(after: s[3]), s[5])
  expectEqual(str.index(after: s[4]), s[5]) // s[4] ≅ s[3]
  expectEqual(str.index(after: s[5]), s[6])

  expectEqual(str.index(before: s[6]), s[5])
  expectEqual(str.index(before: s[5]), s[3])
  expectEqual(str.index(before: s[4]), s[1]) // s[4] ≅ s[3]
  expectEqual(str.index(before: s[3]), s[1])
  expectEqual(str.index(before: s[2]), s[0]) // s[2] ≅ s[1]
  expectEqual(str.index(before: s[1]), s[0])

  dumpIndices(str)
  // UTF-8
  expectEqual(str.utf8.count, 18)
  expectEqual(str.index(after: u8[0]), u8[1])
  for i in 1 ..< 9 { // s[i] ≅ s[1]
    expectEqual(str.index(after: u8[i]), u8[9])
  }
  for i in 9 ..< 17 { // s[i] ≅ s[9]
    expectEqual(str.index(after: u8[i]), u8[17])
  }
  expectEqual(str.index(after: u8[17]), u8[18])

  // UTF-16
  expectEqual(str.utf16.count, 10)
  expectEqual(str.index(after: u16[0]), u16[1])
  expectEqual(str.index(after: u16[1]), u16[5])
  expectEqual(str.index(after: u16[2]), u16[5]) // s[2] ≅ s[1]
  expectEqual(str.index(after: u16[3]), u16[5]) // s[3] ≅ s[1]
  expectEqual(str.index(after: u16[4]), u16[5]) // s[4] ≅ s[1]
  expectEqual(str.index(after: u16[5]), u16[9])
  expectEqual(str.index(after: u16[6]), u16[9]) // s[6] ≅ s[5]
  expectEqual(str.index(after: u16[7]), u16[9]) // s[7] ≅ s[5]
  expectEqual(str.index(after: u16[8]), u16[9]) // s[8] ≅ s[5]
  expectEqual(str.index(after: u16[9]), u16[10])

  let i = s[2] // second scalar of US flag
  let j = s[4] // second scalar of CA flag
  // However, subscripting should only round down to the nearest scalar.
  // Per SE-0180, `s[i..<j]` should be the Seychelles flag (country code SC)
  let slice = str[i ..< j]
  expectEqual(slice, "\u{1F1F8}\u{1F1E8}")
  expectEqual(slice.first, "\u{1F1F8}\u{1F1E8}" as Character)
  expectEqual(slice.count, 1)

  // Index navigation within the substring must work as if we were in a string
  // containing exactly the same scalars. Substring bounds must be reachable,
  // even if they aren't reachable in the base string.
  expectEqual(slice.startIndex, i)
  expectEqual(slice.endIndex, j)
  expectEqual(slice.index(after: slice.startIndex), j)
  expectEqual(slice.index(before: slice.endIndex), i)

  expectEqual(slice.unicodeScalars.count, 2)
  expectEqual(slice.utf8.count, 8)
  expectEqual(slice.utf16.count, 4)
}

runAllTests()
