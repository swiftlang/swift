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
    name: "StringDistance.characters.mixed",
    runFunction: { n in
      run_characters(string: mixedString, ranges: mixedRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(mixedRanges) }),
  BenchmarkInfo(
    name: "StringDistance.scalars.mixed",
    runFunction: { n in
      run_scalars(string: mixedString, ranges: mixedRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(mixedRanges) }),
  BenchmarkInfo(
    name: "StringDistance.utf16.mixed",
    runFunction: { n in
      run_utf16(string: mixedString, ranges: mixedRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(mixedRanges) }),
  BenchmarkInfo(
    name: "StringDistance.utf8.mixed",
    runFunction: { n in
      run_utf8(string: mixedString, ranges: mixedRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(mixedRanges) }),
  BenchmarkInfo(
    name: "StringDistance.characters.ascii",
    runFunction: { n in
      run_characters(string: asciiString, ranges: asciiRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(asciiRanges) }),
  BenchmarkInfo(
    name: "StringDistance.scalars.ascii",
    runFunction: { n in
      run_scalars(string: asciiString, ranges: asciiRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(asciiRanges) }),
  BenchmarkInfo(
    name: "StringDistance.utf16.ascii",
    runFunction: { n in
      run_utf16(string: asciiString, ranges: asciiRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(asciiRanges) }),
  BenchmarkInfo(
    name: "StringDistance.utf8.ascii",
    runFunction: { n in
      run_utf8(string: asciiString, ranges: asciiRanges, n: n)
    },
    tags: [.api, .String],
    setUpFunction: { blackHole(asciiRanges) }),
]


let mixedString = #"""
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

let mixedRanges = (
  generateRanges(for: mixedString, by: 1)
  + generateRanges(for: mixedString, by: 2)
  + generateRanges(for: mixedString, by: 4)
  + generateRanges(for: mixedString, by: 8)
  + generateRanges(for: mixedString, by: 16)
  + generateRanges(for: mixedString, by: 32)
  + generateRanges(for: mixedString, by: 64)
  + generateRanges(for: mixedString, by: 128)
  + generateRanges(for: mixedString, by: 256)
  + generateRanges(for: mixedString, by: 512))

let _asciiString = #"""
  Swift is a high-performance system programming language.  It has a clean
  and modern syntax, offers seamless access to existing C and Objective-C code
  and frameworks, and is memory safe by default.

  Although inspired by Objective-C and many other languages, Swift is not itself
  a C-derived language. As a complete and independent language, Swift packages
  core features like flow control, data structures, and functions, with
  high-level constructs like objects, protocols, closures, and generics. Swift
  embraces modules, eliminating the need for headers and the code duplication
  they entail.

  Swift toolchains are created using the script
  [build-toolchain](https://github.com/apple/swift/blob/main/utils/build-toolchain).
  This script is used by swift.org's CI to produce snapshots and can allow for
  one to locally reproduce such builds for development or distribution purposes.
  A typical invocation looks like the following:

  ```
    $ ./swift/utils/build-toolchain $BUNDLE_PREFIX
  ```

  where ``$BUNDLE_PREFIX`` is a string that will be prepended to the build date
  to give the bundle identifier of the toolchain's ``Info.plist``. For instance,
  if ``$BUNDLE_PREFIX`` was ``com.example``, the toolchain produced will have
  the bundle identifier ``com.example.YYYYMMDD``. It will be created in the
  directory you run the script with a filename of the form:
  ``swift-LOCAL-YYYY-MM-DD-a-osx.tar.gz``.
  """#
let asciiString = String(repeating: _asciiString, count: 10)

let asciiRanges = (
  generateRanges(for: asciiString, by: 1)
  + generateRanges(for: asciiString, by: 2)
  + generateRanges(for: asciiString, by: 4)
  + generateRanges(for: asciiString, by: 8)
  + generateRanges(for: asciiString, by: 16)
  + generateRanges(for: asciiString, by: 32)
  + generateRanges(for: asciiString, by: 64)
  + generateRanges(for: asciiString, by: 128)
  + generateRanges(for: asciiString, by: 256)
  + generateRanges(for: asciiString, by: 512))

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
