//===--- Diffing.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

let t: [BenchmarkCategory] = [.api]
public let benchmarks = [
  BenchmarkInfo(
    name: "Diffing.Same",
    runFunction: { diff($0, from: longPangram, to: longPangram) },
    tags: t,
    setUpFunction: { blackHole(longPangram) }),
  BenchmarkInfo(
    name: "Diffing.PangramToAlphabet",
    runFunction: { diff($0, from: longPangram, to: alphabets) },
    tags: t,
    setUpFunction: { blackHole((longPangram, alphabets)) }),
  BenchmarkInfo(
    name: "Diffing.Pangrams",
    runFunction: { diff($0, from:typingPangram, to: longPangram) },
    tags: t,
    setUpFunction: { blackHole((longPangram, typingPangram)) }),
  BenchmarkInfo(
    name: "Diffing.ReversedAlphabets",
    runFunction: { diff($0, from:alphabets, to: alphabetsReversed) },
    tags: t,
    setUpFunction: { blackHole((alphabets, alphabetsReversed)) }),
  BenchmarkInfo(
    name: "Diffing.ReversedLorem",
    runFunction: { diff($0, from: loremIpsum, to: loremReversed) },
    tags: t,
    setUpFunction: { blackHole((loremIpsum, loremReversed)) }),
  BenchmarkInfo(
    name: "Diffing.Disparate",
    runFunction: { diff($0, from: numbersAndSymbols, to: alphabets) },
    tags: t,
    setUpFunction: { blackHole((numbersAndSymbols, alphabets)) }),
  BenchmarkInfo(
    name: "Diffing.Similar",
    runFunction: { diff($0, from: unabridgedLorem, to: loremIpsum) },
    tags: t,
    setUpFunction: { blackHole((unabridgedLorem, loremIpsum)) }),
  BenchmarkInfo(
    name: "Diffing.VeryLarge",
    runFunction: { diff($0, from: bigUnabridgedLorem, to: bigLoremIpsum) },
    tags: t,
    setUpFunction: { blackHole((bigUnabridgedLorem, bigLoremIpsum)) }),
]

let numbersAndSymbols = Array("0123456789`~!@#$%^&*()+=_-\"'?/<,>.\\{}'")
let alphabets = Array("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
let alphabetsReversed = Array(alphabets.reversed())
let longPangram = Array("This pangram contains four As, one B, two Cs, one D, thirty Es, six Fs, five Gs, seven Hs, eleven Is, one J, one K, two Ls, two Ms, eighteen Ns, fifteen Os, two Ps, one Q, five Rs, twenty-seven Ss, eighteen Ts, two Us, seven Vs, eight Ws, two Xs, three Ys, & one Z")
let typingPangram = Array("The quick brown fox jumps over the lazy dog")
let loremIpsum = Array("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
let unabridgedLorem = Array("Lorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non-numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliqua.")
let loremReversed = Array(loremIpsum.reversed())
let bigLoremIpsum = Array(repeatElement(loremIpsum, count: 100).joined())
let bigUnabridgedLorem = Array(repeatElement(unabridgedLorem, count: 100).joined())

@inline(never) func diff(_ n: Int, from older: [Character], to newer: [Character]) {
  if #available(macOS 10.15, iOS 13, tvOS 13, watchOS 6, *) {
    for _ in 1...n {
      blackHole(newer.difference(from: older))
    }
  }
}
