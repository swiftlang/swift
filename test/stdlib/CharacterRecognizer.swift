// RUN: %empty-directory(%t)
// RUN: %target-run-stdlib-swift %S/Inputs/

// REQUIRES: executable_test
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

if #available(SwiftStdlib 5.8, *) {
  suite.test("Consistency with Swift String's behavior") {
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
