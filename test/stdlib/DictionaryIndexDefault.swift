// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import StdlibUnittest

var tests = TestSuite("DictionaryIndexDefault")
defer { runAllTests() }

tests.test("DefaultValueSubscript") {
  var d = ["a": [1], "c": [3]]
  let r = Int.random(in: 0..<1000)
  d["b", default: []].append(r)
  expectEqual([r], d["b"])
}

tests.test("DefaultValueIndexForKey") {
  var d = ["a": [1], "c": [3]]
  let r = Int.random(in: 0..<1000)
  let i = d._index(forKey: "b", default: [])
  d.values[i].append(r)
  expectEqual([r], d["b"])
}
