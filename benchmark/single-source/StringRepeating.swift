//===--- StringRepeating.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import TestsUtils

public let benchmarks = [
  BenchmarkInfo(name: "String.initRepeating.1AsciiChar.Count100",
                runFunction: run_singleAsciiCharacterCount100,
                tags: [.validation, .api, .String]),
  BenchmarkInfo(name: "String.initRepeating.26AsciiChar.Count2",
                runFunction: run_26AsciiCharactersCount2,
                tags: [.validation, .api, .String]),
  BenchmarkInfo(name: "String.initRepeating.33CyrillicChar.Count2",
                runFunction: run_33CyrillicCharactersCount2,
                tags: [.validation, .api, .String]),
  BenchmarkInfo(name: "String.initRepeating.longMixStr.Count100",
                runFunction: run_longMixedStringCount100,
                tags: [.validation, .api, .String])
]

@inline(never)
public func run_singleAsciiCharacterCount100(N: Int) {
  let string = "x"
  for _ in 1...200*N {
    blackHole(String(repeating: getString(string), count: 100))
  }
}

@inline(never)
public func run_26AsciiCharactersCount2(N: Int) {
  let string = "abcdefghijklmnopqrstuvwxyz"
  for _ in 1...200*N {
    blackHole(String(repeating: getString(string), count: 2))
  }
}

@inline(never)
public func run_33CyrillicCharactersCount2(N: Int) {
  let string = "абвгґдеєжзиіїйклмнопрстуфхцчшщьюя"
  for _ in 1...200*N {
    blackHole(String(repeating: getString(string), count: 2))
  }
}

@inline(never)
public func run_longMixedStringCount100(N: Int) {
  let string = """
    Swift is a multi-paradigm, compiled programming language created for
    iOS, OS X, watchOS, tvOS and Linux development by Apple Inc. Swift is
    designed to work with Apple's Cocoa and Cocoa Touch frameworks and the
    large body of existing Objective-C code written for Apple products. Swift
    is intended to be more resilient to erroneous code (\"safer\") than
    Objective-C and also more concise. It is built with the LLVM compiler
    framework included in Xcode 6 and later and uses the Objective-C runtime,
    which allows C, Objective-C, C++ and Swift code to run within a single
    program.
    Існує багато варіацій уривків з Lorem Ipsum, але більшість з них зазнала
    певних змін на кшталт жартівливих вставок або змішування слів, які навіть
    не виглядають правдоподібно.
    日本語の場合はランダムに生成された文章以外に、
    著作権が切れた小説などが利用されることもある。
    🦩
    """
  for _ in 1...200*N {
    blackHole(String(repeating: getString(string), count: 100))
  }
}
