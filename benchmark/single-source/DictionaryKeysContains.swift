//===--- DictionaryKeysContains.swift -------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

// This benchmark checks if keys.contains(key) is executed in O(1)
// even when wrapping a NSDictionary
import TestsUtils
import Foundation

#if _runtime(_ObjC)
public let DictionaryKeysContains = [
  BenchmarkInfo(
    name: "DictionaryKeysContainsNative",
    runFunction: run_DictionaryKeysContains,
    tags: [.validation, .api, .Dictionary],
    setUpFunction: setupNativeDictionary,
    tearDownFunction: teardownDictionary,
    unsupportedPlatforms: [.linux]),
  BenchmarkInfo(
    name: "DictionaryKeysContainsCocoa",
    runFunction: run_DictionaryKeysContains,
    tags: [.validation, .api, .Dictionary],
    setUpFunction: setupBridgedDictionary,
    tearDownFunction: teardownDictionary),
]
#else
public let DictionaryKeysContains = [
  BenchmarkInfo(
    name: "DictionaryKeysContainsNative",
    runFunction: run_DictionaryKeysContains,
    tags: [.validation, .api, .Dictionary],
    setUpFunction: setupNativeDictionary,
    tearDownFunction: teardownDictionary,
    unsupportedPlatforms: [.linux]),
]
#endif

private var dictionary: [NSString: NSString]!

private func setupNativeDictionary() {
#if os(Linux)
  fatalError("Unsupported benchmark")
#else
  let keyValuePair: (Int) -> (NSString, NSString) = {
    let n = "\($0)" as NSString; return (n, n)
  }
  dictionary = [NSString: NSString](uniqueKeysWithValues:
    (1...10_000).lazy.map(keyValuePair))
#endif
}

#if _runtime(_ObjC)
private func setupBridgedDictionary() {
  setupNativeDictionary()
  dictionary = (NSDictionary(dictionary: dictionary) as! [NSString: NSString])
}
#endif

private func teardownDictionary() {
  dictionary = nil
}

@inline(never)
public func run_DictionaryKeysContains(_ N: Int) {
#if os(Linux)
  fatalError("Unsupported benchmark")
#else
  for _ in 0..<(N * 100) {
    CheckResults(dictionary.keys.contains("42"))
    CheckResults(!dictionary.keys.contains("-1"))
  }
#endif
}
