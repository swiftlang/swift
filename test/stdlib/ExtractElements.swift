//===--- ExtractElements.swift - tests for lazy filtering-------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let ExtractElements = TestSuite("ExtractElements")

extension RangeReplaceableCollection where Element: Equatable {
  mutating func extract(equalTo: Element) -> Self {
    return extractAll(where: { $0 == equalTo })
  }
}

ExtractElements.test("extracting-array-with-predicate") {
  var a = Array(0..<10)
  let e = a.extractAll(where: { $0 % 2 == 0 })
  expectEqualSequence([1,3,5,7,9], a)
  expectEqualSequence([0,2,4,6,8], e)
}

ExtractElements.test("extracting-array-nothing") {
  var a = Array(0..<5)
  let e = a.extractAll(where: { _ in false })
  expectEqualSequence(0..<5, a)
  expectEqualSequence([], e)
}

ExtractElements.test("extracting-array-everything") {
  var a = Array(0..<5)
  let e = a.extractAll(where: { _ in true })
  expectEqualSequence([], a)
  expectEqualSequence(0..<5, e)
}

ExtractElements.test("extracting-array-from-empty") {
  var a: [Int] = []
  let e = a.extractAll(where: { _ in true })
  expectEqualSequence([], a)
  expectEqualSequence([], e)
}

ExtractElements.test("extracting-array-with-equatable") {
  var a = Array(0..<5)
  var e = a.extract(equalTo: 6)
  expectEqualSequence([0,1,2,3,4], a)
  expectEqualSequence([], e)
  e = a.extract(equalTo: 3)
  expectEqualSequence([0,1,2,4], a)
  expectEqualSequence([3], e)
  e = a.extract(equalTo: 0)
  expectEqualSequence([1,2,4], a)
  expectEqualSequence([0], e)
  e = a.extract(equalTo: 4)
  expectEqualSequence([1,2], a)
  expectEqualSequence([4], e)
  e = a.extract(equalTo: 1)
  expectEqualSequence([2], a)
  expectEqualSequence([1], e)
  e = a.extract(equalTo: 2)
  expectEqualSequence([], a)
  expectEqualSequence([2], e)
}

ExtractElements.test("extracting-string-with-predicate") {
  var s = "0123456789"
  let e = s.extractAll(where: { Int(String($0))! % 2 == 0 })
  expectEqualSequence("13579", s)
  expectEqualSequence("02468", e)
}

ExtractElements.test("extracting-string-nothing") {
  var s = "01234"
  let e = s.extractAll(where: { _ in false })
  expectEqualSequence("01234", s)
  expectEqualSequence("", e)
}

ExtractElements.test("extracting-string-everything") {
  var s =  "01234"
  let e = s.extractAll(where: { _ in true })
  expectEqualSequence("", s)
  expectEqualSequence("01234", e)
}

ExtractElements.test("extracting-string-from-empty") {
  var s = ""
  let e = s.extractAll(where: { _ in true })
  expectEqualSequence("", s)
  expectEqualSequence("", e)
}

ExtractElements.test("extracting-string-with-equatable") {
  var s = "01234"
  var e = s.extract(equalTo: "6")
  expectEqualSequence("01234", s)
  expectEqualSequence("", e)
  e = s.extract(equalTo: "3")
  expectEqualSequence("0124", s)
  expectEqualSequence("3", e)
  e = s.extract(equalTo: "0")
  expectEqualSequence("124", s)
  expectEqualSequence("0", e)
  e = s.extract(equalTo: "4")
  expectEqualSequence("12", s)
  expectEqualSequence("4", e)
  e = s.extract(equalTo: "1")
  expectEqualSequence("2", s)
  expectEqualSequence("1", e)
  e = s.extract(equalTo: "2")
  expectEqualSequence("", s)
  expectEqualSequence("2", e)
}

runAllTests()

