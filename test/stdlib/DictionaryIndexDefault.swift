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
  guard #available(SwiftStdlib 9999, *) else { return }

  var d = ["a": [1], "c": [3]]
  let r = Int.random(in: 0..<1000)
  let i = d._index(forKey: "b", default: [])
  d.values[i].append(r)
  expectEqual([r], d["b"])

  let j = d._index(forKey: "b", default: [])
  d.values[j].append(r)
  expectNotEqual([r], d["b"])
}

tests.test("DictionaryInsertIfNil") {
  guard #available(SwiftStdlib 9999, *) else { return }

  var d = ["a": [1], "c": [3]]
  var inserted: Bool
  let r = Int.random(in: 0..<1000)

  inserted = d._addValue([r], forKey: "b")
  expectTrue(inserted)
  expectEqual([r], d["b"])

  inserted = d._addValue([], forKey: "c")
  expectFalse(inserted)
  if let value = expectNotNil(d["c"]) {
    expectFalse(value.isEmpty)
  }
}
