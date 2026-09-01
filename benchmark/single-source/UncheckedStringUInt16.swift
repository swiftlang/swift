//===--- UncheckedStringUInt16.swift ---------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Test UncheckedString<UInt16> performance over the operations it actually
// supports: element (raw UTF-16 code unit) iteration, byte-wise comparison
// and hashing, hasPrefix/hasSuffix, appending (small vs. dynamic storage),
// replaceSubrange, and encode/decode round-tripping with String. This
// module is compiled with -Xfrontend -disable-availability-checking (see
// benchmark/CMakeLists.txt / benchmark/Package.swift), since UncheckedString
// is still `@available(SwiftStdlib 9999, *)`.
//
// This is the UInt16 counterpart of UncheckedStringUInt8.swift -- same
// families and corpora, different concrete `Element` (which also means a
// different small-storage capacity: 7 UInt16s vs. 14 UInt8s on 64-bit).
//

import TestsUtils

private typealias UStr = UncheckedString<UInt16>

private let uncheckedStringTags: [BenchmarkCategory] =
  [.validation, .api, .UncheckedString]

public let benchmarks: [BenchmarkInfo] = [
  BenchmarkInfo(
    name: "UncheckedStr16.Walk.Ascii",
    runFunction: run_UncheckedStr16_Walk_Ascii,
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.Walk.Ascii.Indices",
    runFunction: run_UncheckedStr16_Walk_Ascii_Indices,
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.Walk.Ascii.Backwards",
    runFunction: run_UncheckedStr16_Walk_Ascii_Backwards,
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.Walk.Emoji",
    runFunction: run_UncheckedStr16_Walk_Emoji,
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.Walk.CJK",
    runFunction: run_UncheckedStr16_Walk_CJK,
    tags: uncheckedStringTags),

  BenchmarkInfo(
    name: "UncheckedStr16.Cmp.Ascii",
    runFunction: { compareLoop($0, comparisonAscii) },
    tags: uncheckedStringTags,
    setUpFunction: { blackHole(comparisonAscii) }),
  BenchmarkInfo(
    name: "UncheckedStr16.Cmp.Emoji",
    runFunction: { compareLoop($0, comparisonEmoji) },
    tags: uncheckedStringTags,
    setUpFunction: { blackHole(comparisonEmoji) }),
  BenchmarkInfo(
    name: "UncheckedStr16.Cmp.CJK",
    runFunction: { compareLoop($0, comparisonCJK) },
    tags: uncheckedStringTags,
    setUpFunction: { blackHole(comparisonCJK) }),
  BenchmarkInfo(
    name: "UncheckedStr16.Cmp.SharedPrefix",
    runFunction: { compareLoop($0, comparisonSharedPrefix) },
    tags: uncheckedStringTags,
    setUpFunction: { blackHole(comparisonSharedPrefix) }),
  BenchmarkInfo(
    name: "UncheckedStr16.Hash.Ascii",
    runFunction: { hashLoop($0, comparisonAscii) },
    tags: uncheckedStringTags,
    setUpFunction: { blackHole(comparisonAscii) }),
  BenchmarkInfo(
    name: "UncheckedStr16.Hash.Emoji",
    runFunction: { hashLoop($0, comparisonEmoji) },
    tags: uncheckedStringTags,
    setUpFunction: { blackHole(comparisonEmoji) }),

  BenchmarkInfo(
    name: "UncheckedStr16.HasPrefix.Ascii",
    runFunction: {
      hasPrefixLoop($0, hasPrefixAsciiString, hasPrefixAsciiPrefix, 10_000)
    },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.HasPrefix.Unicode",
    runFunction: {
      hasPrefixLoop($0, hasPrefixUnicodeString, hasPrefixUnicodePrefix, 100)
    },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.HasSuffix.Ascii",
    runFunction: {
      hasSuffixLoop($0, hasSuffixAsciiString, hasSuffixAsciiSuffix, 10_000)
    },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.HasSuffix.Unicode",
    runFunction: {
      hasSuffixLoop($0, hasSuffixUnicodeString, hasSuffixUnicodeSuffix, 100)
    },
    tags: uncheckedStringTags),

  BenchmarkInfo(
    name: "UncheckedStr16.Builder.Small",
    runFunction: run_UncheckedStr16_Builder_Small,
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.Builder.Dynamic",
    runFunction: run_UncheckedStr16_Builder_Dynamic,
    tags: uncheckedStringTags),

  BenchmarkInfo(
    name: "UncheckedStr16.replaceSubrange.Small",
    runFunction: { replaceSubrangeLoop($0, replaceSmallBase, replacement) },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.replaceSubrange.Large",
    runFunction: { replaceSubrangeLoop($0, replaceLargeBase, replacement) },
    tags: uncheckedStringTags),

  BenchmarkInfo(
    name: "UncheckedStr16.encode.Ascii",
    runFunction: { encodeLoop($0, encodeAsciiSource) },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.encode.Unicode",
    runFunction: { encodeLoop($0, encodeUnicodeSource) },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.decode.Ascii",
    runFunction: { decodeLoop($0, decodeAsciiSource) },
    tags: uncheckedStringTags),
  BenchmarkInfo(
    name: "UncheckedStr16.decode.Unicode",
    runFunction: { decodeLoop($0, decodeUnicodeSource) },
    tags: uncheckedStringTags),
]

// MARK: - Walk

private let asciiWalk: UStr =
  "siebenhundertsiebenundsiebzigtausendsiebenhundertsiebenundsiebzig"
private let emojiWalk: UStr = "😀🧀😃😄😁🤣😂😅😆👍🙌🎉"
private let cjkWalk: UStr =
  "今回のアップデートでSwiftに大幅な改良が施され、安定していてしかも直感的に使うことができるAppleプラットフォーム向けプログラミング言語になりました。"

private let walkMultiplier = 2000

@inline(never)
private func countForward(_ s: UStr) -> Int {
  var count = 0
  for _ in s {
    count += 1
  }
  return count
}

@inline(never)
private func countIndices(_ s: UStr) -> Int {
  var count = 0
  for i in s.indices {
    count |= Int(s[i])
  }
  return count
}

@inline(never)
private func countBackwards(_ s: UStr) -> Int {
  var count = 0
  for _ in s.reversed() {
    count += 1
  }
  return count
}

@inline(never)
public func run_UncheckedStr16_Walk_Ascii(_ n: Int) {
  for _ in 1...walkMultiplier*n {
    blackHole(countForward(asciiWalk))
  }
}

@inline(never)
public func run_UncheckedStr16_Walk_Ascii_Indices(_ n: Int) {
  for _ in 1...walkMultiplier*n {
    blackHole(countIndices(asciiWalk))
  }
}

@inline(never)
public func run_UncheckedStr16_Walk_Ascii_Backwards(_ n: Int) {
  for _ in 1...walkMultiplier*n {
    blackHole(countBackwards(asciiWalk))
  }
}

@inline(never)
public func run_UncheckedStr16_Walk_Emoji(_ n: Int) {
  for _ in 1...walkMultiplier*n {
    blackHole(countForward(emojiWalk))
  }
}

@inline(never)
public func run_UncheckedStr16_Walk_CJK(_ n: Int) {
  for _ in 1...walkMultiplier*n {
    blackHole(countForward(cjkWalk))
  }
}

// MARK: - Comparison / Hashing

private struct Workload {
  static let n = 100

  let name: String
  let payload: [UStr]
  var scaleMultiplier: Double = 1.0

  var tripCount: Int {
    return Int(Double(Workload.n) * scaleMultiplier)
  }
}

private let comparisonAscii = Workload(
  name: "Ascii",
  payload: [
    "woodshed", "lakism", "gastroperiodynia", "afetal", "Casearia",
    "ramsch", "Nickieben", "undutifulness", "decorticate", "neognathic",
    "mentionable", "tetraphenol", "pseudonymal", "dislegitimate",
    "Discoidea",
  ])

private let comparisonEmoji = Workload(
  name: "Emoji",
  payload: [
    "😀🧀😀😃😄😁🤣😂😅😆",
    "😺🎃🤖👾😸😹😻😼😾😿🙀😽🙌🙏🤝👍✌",
    "☺️😊😇🙂😍😌😉🙃😘😗😙😚😛😝😜",
    "😋🤑🤗🤓😎😒😏🤠🤡😞😔😟😕😖😣☹️🙁😫😩😤😠😑😐😶😡😯",
  ],
  scaleMultiplier: 1.0 / 4.0)

private let comparisonCJK = Workload(
  name: "CJK",
  payload: [
    "今回のアップデートでSwiftに大幅な改良が施され",
    "安定していてしかも直感的に使うことができる",
    "Appleプラットフォーム向けプログラミング言語になりました",
    "이번 업데이트에서는 강력하면서도 직관적인",
    "Apple 플랫폼용 프로그래밍 언어인 Swift를 완벽히 개선하였습니다",
  ],
  scaleMultiplier: 1.0 / 2.0)

private let comparisonSharedPrefix = Workload(
  name: "SharedPrefix",
  payload: [
    "http://www.dogbook.com/dog/239495828/friends/mutual/2939493815",
    "http://www.dogbook.com/dog/239495828/friends/mutual/3910583739",
    "http://www.dogbook.com/dog/239495828/friends/mutual/3910583739/shared",
    "http://www.dogbook.com/dog/239495828/friends/mutual/3910583739/shared2",
  ])

@inline(never)
private func compareLoop(_ n: Int, _ w: Workload) {
  let tripCount = w.tripCount
  let payload = w.payload
  for _ in 1...tripCount*n {
    for s1 in payload {
      for s2 in payload {
        blackHole(s1 < s2)
      }
    }
  }
}

@inline(never)
private func hashLoop(_ n: Int, _ w: Workload) {
  let tripCount = w.tripCount
  let payload = w.payload
  for _ in 1...tripCount*n {
    for s in payload {
      blackHole(s.hashValue)
    }
  }
}

// MARK: - hasPrefix / hasSuffix

private let hasPrefixAsciiPrefix: UStr = "prefix"
private let hasPrefixAsciiString: UStr = "prefixedString"
private let hasSuffixAsciiSuffix: UStr = "Suffixed"
private let hasSuffixAsciiString: UStr = "StringSuffixed"
private let hasPrefixUnicodePrefix: UStr = "❄️prefix"
private let hasPrefixUnicodeString: UStr = "❄️prefixedString"
private let hasSuffixUnicodeSuffix: UStr = "❄️Suffixed"
private let hasSuffixUnicodeString: UStr = "String❄️Suffixed"

@inline(never)
private func hasPrefixLoop(_ n: Int, _ s: UStr, _ p: UStr, _ iterations: Int) {
  for _ in 0 ..< n {
    for _ in 0 ..< iterations {
      blackHole(s.hasPrefix(p))
    }
  }
}

@inline(never)
private func hasSuffixLoop(_ n: Int, _ s: UStr, _ p: UStr, _ iterations: Int) {
  for _ in 0 ..< n {
    for _ in 0 ..< iterations {
      blackHole(s.hasSuffix(p))
    }
  }
}

// MARK: - Builder

private let builderSmallSeed: UStr = "a"
private let builderWordB: UStr = "b"
private let builderWordC: UStr = "c"
private let builderWordD: UStr = "d"

@inline(never)
private func buildSmall(_ seed: UStr) -> UStr {
  var sb = seed
  sb += builderWordB
  sb += builderWordC
  sb += builderWordD
  return sb
}

@inline(never)
public func run_UncheckedStr16_Builder_Small(_ n: Int) {
  for _ in 1...5000*n {
    blackHole(buildSmall(identity(builderSmallSeed)))
  }
}

private let builderDynamicSeed: UStr = "seed"
private let builderLongWord: UStr =
  "bumfuzzlebumfuzzlebumfuzzlebumfuzzlebumfuzzle"

@inline(never)
private func buildDynamic(_ seed: UStr) -> UStr {
  var sb = seed
  sb += builderLongWord
  return sb
}

@inline(never)
public func run_UncheckedStr16_Builder_Dynamic(_ n: Int) {
  for _ in 1...500*n {
    blackHole(buildDynamic(identity(builderDynamicSeed)))
  }
}

// MARK: - replaceSubrange

private let replaceSmallBase: UStr = "coffee"
private let replaceLargeBase: UStr =
  "coffeecoffeecoffeecoffeecoffeecoffeecoffeecoffee"
private let replacement: UStr = "T"

@inline(never)
private func replaceSubrangeLoop(_ n: Int, _ base: UStr, _ with: UStr) {
  var copy = base
  let range = base.startIndex..<base.index(after: base.startIndex)
  for _ in 0 ..< 500 * n {
    copy.replaceSubrange(range, with: with)
  }
}

// MARK: - encode / decode

private let encodeAsciiSource =
  "The quick brown fox jumps over the lazy dog, again and again and again."
private let encodeUnicodeSource =
  "Dagmar Karin Sørbøe visited 東京 for the show"

private let decodeAsciiSource: UStr =
  "The quick brown fox jumps over the lazy dog, again and again and again."
private let decodeUnicodeSource: UStr =
  "Dagmar Karin Sørbøe visited 東京 for the show"

@inline(never)
private func encodeLoop(_ n: Int, _ source: String) {
  for _ in 0 ..< 2000 * n {
    blackHole(
      getString(source).encode(as: UTF16.self, onUnsupportedEncoding: .substitute))
  }
}

@inline(never)
private func decodeLoop(_ n: Int, _ source: UStr) {
  for _ in 0 ..< 2000 * n {
    blackHole(identity(source).decode(as: UTF16.self, onInvalidEncoding: .substitute))
  }
}
