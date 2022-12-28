//===--- StringEdits.swift ------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks: [BenchmarkInfo] = [
  BenchmarkInfo(
    name: "StringDistance.characters",
    runFunction: { n in
      run_characters(string: sampleString, ranges: sampleRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { _ = sampleRanges }),
  BenchmarkInfo(
    name: "StringDistance.scalars",
    runFunction: { n in
      run_scalars(string: sampleString, ranges: sampleRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { _ = sampleRanges }),
  BenchmarkInfo(
    name: "StringDistance.utf16",
    runFunction: { n in
      run_utf16(string: sampleString, ranges: sampleRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { _ = sampleRanges }),
  BenchmarkInfo(
    name: "StringDistance.utf8",
    runFunction: { n in
      run_utf8(string: sampleString, ranges: sampleRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { _ = sampleRanges }),
]


let sampleString =
    #"""
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

let sampleRanges = (
  generateRanges(for: sampleString, by: 1)
  + generateRanges(for: sampleString, by: 2)
  + generateRanges(for: sampleString, by: 4)
  + generateRanges(for: sampleString, by: 8)
  + generateRanges(for: sampleString, by: 16)
  + generateRanges(for: sampleString, by: 32)
  + generateRanges(for: sampleString, by: 64)
  + generateRanges(for: sampleString, by: 128)
  + generateRanges(for: sampleString, by: 256)
  + generateRanges(for: sampleString, by: 512))

func generateRanges(for string: String, by step: Int) -> [Range<String.Index>] {
  var remaining = step
  var i = string.startIndex
  var last = i

  var ranges: [Range<String.Index>] = []
  while i < string.endIndex {
    string.unicodeScalars.formIndex(after: &i)
    remaining -= 1
    if remaining == 0 {
      ranges.append(last ..< i)
      remaining = step
      last = i
    }
  }
  ranges.append(last ..< i)
  return ranges
}

func run_characters(string: String, ranges: [Range<String.Index>], n: Int) {
  var c = 0
  for _ in 0 ..< n {
    for r in ranges {
      c += string.distance(from: r.lowerBound, to: r.upperBound)
    }
  }
  blackHole(c)
}

func run_scalars(string: String, ranges: [Range<String.Index>], n: Int) {
  var c = 0
  for _ in 0 ..< n {
    for r in ranges {
      c += string.unicodeScalars.distance(from: r.lowerBound, to: r.upperBound)
    }
  }
  blackHole(c)
}

func run_utf16(string: String, ranges: [Range<String.Index>], n: Int) {
  var c = 0
  for _ in 0 ..< n {
    for r in ranges {
      c += string.utf16.distance(from: r.lowerBound, to: r.upperBound)
    }
  }
  blackHole(c)
}

func run_utf8(string: String, ranges: [Range<String.Index>], n: Int) {
  var c = 0
  for _ in 0 ..< n {
    for r in ranges {
      c += string.utf8.distance(from: r.lowerBound, to: r.upperBound)
    }
  }
  blackHole(c)
}
