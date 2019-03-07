//===--- RemoveElements.swift - tests for lazy filtering-------------------===//
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
// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


let RemoveElements = TestSuite("RemoveElements")

extension RangeReplaceableCollection where Element: Equatable {
  mutating func remove(equalTo: Element) {
    removeAll(where: { $0 == equalTo })
  }
}

RemoveElements.test("removing-array-with-predicate") {
  var a = Array(0..<10)
  a.removeAll(where: { $0 % 2 == 0 })
  expectEqualSequence([1,3,5,7,9], a)
}

RemoveElements.test("removing-array-nothing") {
  var a = Array(0..<5)
  a.removeAll(where: { _ in false })
  expectEqualSequence(0..<5, a)
}

RemoveElements.test("removing-array-everything") {
  var a = Array(0..<5)
  a.removeAll(where: { _ in true })
  expectEqualSequence([], a)
}

RemoveElements.test("removing-array-from-empty") {
  var a: [Int] = []
  a.removeAll(where: { _ in true })
  expectEqualSequence([], a)
}

RemoveElements.test("removing-array-with-equatable") {
  var a = Array(0..<5)
  a.remove(equalTo: 6)
  expectEqualSequence([0,1,2,3,4], a)
  a.remove(equalTo: 3)
  expectEqualSequence([0,1,2,4], a)
  a.remove(equalTo: 0)
  expectEqualSequence([1,2,4], a)
  a.remove(equalTo: 4)
  expectEqualSequence([1,2], a)
  a.remove(equalTo: 1)
  expectEqualSequence([2], a)
  a.remove(equalTo: 2)
  expectEqualSequence([], a)
}

RemoveElements.test("removing-string-with-predicate") {
  var s = "0123456789"
  s.removeAll(where: { Int(String($0))! % 2 == 0 })
  expectEqualSequence("13579", s)
}

RemoveElements.test("removing-string-nothing") {
  var s = "01234"
  s.removeAll(where: { _ in false })
  expectEqualSequence("01234", s)
}

RemoveElements.test("removing-string-everything") {
  var s =  "01234"
  s.removeAll(where: { _ in true })
  expectEqualSequence("", s)
}

RemoveElements.test("removing-string-from-empty") {
  var s = ""
  s.removeAll(where: { _ in true })
  expectEqualSequence("", s)
}

RemoveElements.test("removing-string-with-equatable") {
  var s = "01234"
  s.remove(equalTo: "6")
  expectEqualSequence("01234", s)
  s.remove(equalTo: "3")
  expectEqualSequence("0124", s)
  s.remove(equalTo: "0")
  expectEqualSequence("124", s)
  s.remove(equalTo: "4")
  expectEqualSequence("12", s)
  s.remove(equalTo: "1")
  expectEqualSequence("2", s)
  s.remove(equalTo: "2")
  expectEqualSequence("", s)
}

runAllTests()

