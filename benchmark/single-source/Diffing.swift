//===--- Diffing.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

let t: [BenchmarkCategory] = [.api]
public let Diffing = [
  BenchmarkInfo(
    name: "DiffSame",
    runFunction: run_DiffSame,
    tags: t,
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DiffPangramToAlphabet",
    runFunction: run_DiffPangramToAlphabet,
    tags: t,
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DiffPangrams",
    runFunction: run_DiffPangrams,
    tags: t,
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DiffReversedAlphabets",
    runFunction: run_DiffReversedAlphabets,
    tags: t,
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DiffReversedLorem",
    runFunction: run_DiffReversedLorem,
    tags: t,
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DiffDisparate",
    runFunction: run_DiffDisparate,
    tags: t,
    legacyFactor: 10),
  BenchmarkInfo(
    name: "DiffSimilar",
    runFunction: run_DiffSimilar,
    tags: t,
    legacyFactor: 10),
]

let numbersAndSymbols = Array("0123456789`~!@#$%^&*()+=_-\"'?/<,>.\\{}'")
let alphabets = Array("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ")
let alphabetsReversed = Array("ZYXWVUTSRQPONMLKJIHGFEDCBAzyxwvutsrqponmlkjihgfedcba")
let longPangram = Array("This pangram contains four As, one B, two Cs, one D, thirty Es, six Fs, five Gs, seven Hs, eleven Is, one J, one K, two Ls, two Ms, eighteen Ns, fifteen Os, two Ps, one Q, five Rs, twenty-seven Ss, eighteen Ts, two Us, seven Vs, eight Ws, two Xs, three Ys, & one Z")
let typingPangram = Array("The quick brown fox jumps over the lazy dog")
let loremIpsum = Array("Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
let unabridgedLorem = Array("Lorem ipsum, quia dolor sit amet consectetur adipisci[ng] velit, sed quia non-numquam [do] eius modi tempora inci[di]dunt, ut labore et dolore magnam aliqua.")
let loremReverse = Array(".auqila angam erolod te erobal tu tnudidicni ropmet domsuie od des ,tile gnicsipida rutetcesnoc ,tema tis rolod muspi meroL")


@inline(never)
public func run_DiffSame(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = longPangram.difference(from: longPangram)
    }
  }
}

@inline(never)
public func run_DiffPangramToAlphabet(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = longPangram.difference(from: alphabets)
    }
  }
}

@inline(never)
public func run_DiffPangrams(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = longPangram.difference(from: typingPangram)
    }
  }
}

@inline(never)
public func run_DiffReversedAlphabets(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = alphabets.difference(from: alphabetsReversed)
    }
  }
}

@inline(never)
public func run_DiffReversedLorem(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = loremIpsum.difference(from: loremReverse)
    }
  }
}

@inline(never)
public func run_DiffDisparate(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = alphabets.difference(from: numbersAndSymbols)
    }
  }
}

@inline(never)
public func run_DiffSimilar(_ N: Int) {
  if #available(macOS 9999, iOS 9999, tvOS 9999, watchOS 9999, *) { // FIXME(availability-5.1)
    for _ in 1...N {
      let _ = loremIpsum.difference(from: unabridgedLorem)
    }
  }
}
