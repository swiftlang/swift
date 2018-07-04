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
    setUpFunction: setup_DictionaryKeysContainsNative,
    tearDownFunction: teardown_DictionaryKeysContains),
  BenchmarkInfo(
    name: "DictionaryKeysContainsCocoa",
    runFunction: run_DictionaryKeysContains,
    tags: [.validation, .api, .Dictionary],
    setUpFunction: setup_DictionaryKeysContainsCocoa,
    tearDownFunction: teardown_DictionaryKeysContains),
]
#else
public let DictionaryKeysContains = [
  BenchmarkInfo(
    name: "DictionaryKeysContainsNative",
    runFunction: run_DictionaryKeysContains,
    tags: [.validation, .api, .Dictionary],
    setUpFunction: setup_DictionaryKeysContainsNative,
    tearDownFunction: teardown_DictionaryKeysContains,
    unsupportedPlatforms: [.linux]),
]
#endif

private var dictionary: [NSString: NSString]!

private func setup_DictionaryKeysContainsNative() {
#if os(Linux)
  fatalError("Unsupported benchmark")
#else
  let keyValuePairs = (1...1_000_000).map {
    ("\($0)" as NSString, "\($0)" as NSString)
  }
  dictionary = [NSString: NSString](uniqueKeysWithValues: keyValuePairs)
#endif
}

#if _runtime(_ObjC)
private func setup_DictionaryKeysContainsCocoa() {
  let keyValuePairs = (1...1_000_000).map {
    ("\($0)" as NSString, "\($0)" as NSString)
  }
  let nativeDictionary = [NSString: NSString](
    uniqueKeysWithValues: keyValuePairs)
  dictionary = (NSDictionary(dictionary: nativeDictionary)
    as! [NSString: NSString])
}
#endif

private func teardown_DictionaryKeysContains() {
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

