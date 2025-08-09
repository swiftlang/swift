// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values
// REQUIRES: objc_interop
// REQUIRES: optimized_stdlib

import Swift
import StdlibUnittest
import StdlibUnicodeUnittest

var suite = TestSuite("CharacterRecognizer")
defer { runAllTests() }

if #available(SwiftStdlib 5.8, *) {
  suite.test("Unicode test data/hasBreak") {
    for test in graphemeBreakTests {
      var recognizer = Unicode._CharacterRecognizer()
      var pieces: [[Unicode.Scalar]] = []
      var piece: [Unicode.Scalar] = []
      for scalar in test.string.unicodeScalars {
        if recognizer.hasBreak(before: scalar) {
          if !piece.isEmpty { pieces.append(piece) }
          piece = [scalar]
        } else {
          piece.append(scalar)
        }
      }
      if !piece.isEmpty { pieces.append(piece) }
      expectEqual(pieces, test.pieces,
        "string: \(String(reflecting: test.string))")
    }
  }
}

func scalars(in buffer: some Sequence<UInt8>) -> [Unicode.Scalar] {
  var result: [Unicode.Scalar] = []
  var it = buffer.makeIterator()
  var utf8Decoder = UTF8()
  while true {
    switch utf8Decoder.decode(&it) {
    case .scalarValue(let v): result.append(v)
    case .emptyInput: return result
    case .error: expectTrue(false, "Invalid scalar")
    }
  }
}

if #available(SwiftStdlib 5.8, *) {
  suite.test("Unicode test data/_firstBreak") {
    for test in graphemeBreakTests {
      var recognizer = Unicode._CharacterRecognizer()
      var pieces: [[Unicode.Scalar]] = []
      var str = test.string
      str.withUTF8 { buffer in
        var i = buffer.startIndex
        var last = i
        while i < buffer.endIndex {
          guard let scalar = recognizer._firstBreak(
            inUncheckedUnsafeUTF8Buffer: buffer, startingAt: i)
          else { break }

          if scalar.lowerBound > last {
            pieces.append(scalars(in: buffer[last..<scalar.lowerBound]))
          }

          last = scalar.lowerBound
          i = scalar.upperBound
        }

        pieces.append(scalars(in: buffer[last...]))
      }
      expectEqual(pieces, test.pieces,
        "string: \(String(reflecting: test.string))")
    }
  }
}

let sampleString = #"""
    The powerful programming language that is also easy to learn.
    손쉽게 학습할 수 있는 강력한 프로그래밍 언어.
    🪙 A 🥞 short 🍰 piece 🫘 of 🌰 text 👨‍👨‍👧‍👧 with 👨‍👩‍👦 some 🚶🏽 emoji 🇺🇸🇨🇦 characters 🧈
    some🔩times 🛺 placed 🎣 in 🥌 the 🆘 mid🔀dle 🇦🇶or🏁 around 🏳️‍🌈 a 🍇 w🍑o🥒r🥨d
    Unicode is such fun!
    U̷n̷i̷c̷o̴d̴e̷ ̶i̸s̷ ̸s̵u̵c̸h̷ ̸f̵u̷n̴!̵
    U̴̡̲͋̾n̵̻̳͌ì̶̠̕c̴̭̈͘ǫ̷̯͋̊d̸͖̩̈̈́ḛ̴́ ̴̟͎͐̈i̴̦̓s̴̜̱͘ ̶̲̮̚s̶̙̞͘u̵͕̯̎̽c̵̛͕̜̓h̶̘̍̽ ̸̜̞̿f̵̤̽ṷ̴͇̎͘ń̷͓̒!̷͍̾̚
    U̷̢̢̧̨̼̬̰̪͓̞̠͔̗̼̙͕͕̭̻̗̮̮̥̣͉̫͉̬̲̺͍̺͊̂ͅ\#
    n̶̨̢̨̯͓̹̝̲̣̖̞̼̺̬̤̝̊̌́̑̋̋͜͝ͅ\#
    ḭ̸̦̺̺͉̳͎́͑\#
    c̵̛̘̥̮̙̥̟̘̝͙̤̮͉͔̭̺̺̅̀̽̒̽̏̊̆͒͌̂͌̌̓̈́̐̔̿̂͑͠͝͝ͅ\#
    ö̶̱̠̱̤̙͚͖̳̜̰̹̖̣̻͎͉̞̫̬̯͕̝͔̝̟̘͔̙̪̭̲́̆̂͑̌͂̉̀̓́̏̎̋͗͛͆̌̽͌̄̎̚͝͝͝͝ͅ\#
    d̶̨̨̡̡͙̟͉̱̗̝͙͍̮͍̘̮͔͑\#
    e̶̢͕̦̜͔̘̘̝͈̪̖̺̥̺̹͉͎͈̫̯̯̻͑͑̿̽͂̀̽͋́̎̈́̈̿͆̿̒̈́̽̔̇͐͛̀̓͆̏̾̀̌̈́̆̽̕ͅ
    """#

if #available(SwiftStdlib 5.8, *) {
  suite.test("Consistency with Swift String's behavior/hasBreak") {

    let expectedBreaks = Array(sampleString.indices)

    let u = sampleString.unicodeScalars

    var recognizer = Unicode._CharacterRecognizer()
    var actualBreaks: [String.Index] = []
    for i in u.indices {
      if recognizer.hasBreak(before: u[i]) {
        actualBreaks.append(i)
      }
    }
    expectEqual(actualBreaks, expectedBreaks,
      """
      actualBreaks: \(actualBreaks.map { $0._description })
      expectedBreaks: \(expectedBreaks.map { $0._description })
      """)
  }
}

if #available(SwiftStdlib 5.9, *) {
  suite.test("Equatable") {
    var r1 = Unicode._CharacterRecognizer()
    var r2 = Unicode._CharacterRecognizer()
    expectEqual(r1, r2)
    expectTrue(r1.hasBreak(before: "a"))
    expectNotEqual(r1, r2)
    expectTrue(r2.hasBreak(before: "a"))
    expectEqual(r1, r2)
    expectTrue(r2.hasBreak(before: "\u{1f44f}")) // CLAPPING HANDS SIGN
    expectNotEqual(r1, r2)
    expectTrue(r1.hasBreak(before: "b"))
    expectNotEqual(r1, r2)
    expectFalse(r2.hasBreak(before: "\u{1f3fc}")) // EMOJI MODIFIER FITZPATRICK TYPE-3
    expectNotEqual(r1, r2)
    expectTrue(r2.hasBreak(before: "b"))
    expectEqual(r1, r2) // breaks should reset state
  }
}

if #available(SwiftStdlib 5.9, *) {
  suite.test("CustomStringConvertible") {
    var r = Unicode._CharacterRecognizer()
    expectEqual("\(r)", "[]U+0")
    expectTrue(r.hasBreak(before: "\u{1F1FA}")) // REGIONAL INDICATOR SYMBOL LETTER U
    expectEqual("\(r)", "[]U+1F1FA")
    expectFalse(r.hasBreak(before: "\u{1F1F8}")) // REGIONAL INDICATOR SYMBOL LETTER S
    expectEqual("\(r)", "[R]U+1F1F8")
    expectTrue(r.hasBreak(before: "$"))
    expectEqual("\(r)", "[]U+24")
  }
}
