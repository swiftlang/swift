// RUN: %target-swift-frontend -typecheck -verify -DVERIFY %s
// RUN: %target-run-stdlib-swift
// REQUIRES: executable_test

#if !VERIFY

import StdlibUnittest

var tests = TestSuite("ElementsEqual")

tests.test("Set.elementsEqual(_:)/OtherSet") {
  let s = Set([1, 2, 3])
  let r = Set(s.reversed())
  _ = s.elementsEqual(r) // result can be anything, but it should not trap
}

tests.test("Set.elementsEqual(_:)/Sequence") {
  let xs = [1,2,3]
  let s = Set(xs)
  _ = s.elementsEqual(xs) // result can be anything, but it should not trap
  expectFalse(s.elementsEqual(xs + [4]))
}

tests.test("Set.elementsEqual(_:by:)/Sequence") {
  let xs = [1,2,3]
  let s = Set(xs)
  _ = s.elementsEqual(xs, by: { $0 == $1 })
  // result can be anything, but it should not trap
}

tests.test("Sequence.elementsEqual(_:)/Set") {
  let xs = [1,2,3]
  let s = Set(xs)
  _ = xs.elementsEqual(s) // result can be anything, but it should not trap
  expectFalse((xs + [4]).elementsEqual(s))
}

tests.test("Sequence.elementsEqual(_:by:)/Seq") {
  let xs = [1,2,3]
  let s = Set(xs)
  _ = xs.elementsEqual(s, by: { $0 == $1 })
  // result can be anything, but it should not trap
}

tests.test("Dictionary.elementsEqual(_:, by:)/Sequence") {
  let d = [1 : "foo", 2 : "bar"]
  let kvs = [(key: 1, value: "foo"), (key: 2, value: "bar")]
  _ = (d.elementsEqual(kvs, by: { _ in true }))
  // result can be anything, but it should not trap
}

tests.test("Sequence.elementsEqual(_:, by:)/Dictionary") {
  let d = [1 : "foo", 2 : "bar"]
  let kvs = [(key: 1, value: "foo"), (key: 2, value: "bar")]
  _ = (kvs.elementsEqual(d, by: { _ in true }))
  // result can be anything, but it should not trap
}

runAllTests()

#else

let s = Set([1, 2, 3])
let r = Set(s.reversed())

_ = s.elementsEqual(r) // expected-warning {{unordered}}
_ = s.elementsEqual(r, by: { _ in true }) // expected-warning {{unordered}}
_ = [1,2,3].elementsEqual(s) // expected-warning {{unordered}}
_ = [1,2,3].elementsEqual(s, by: { _ in true }) // expected-warning {{unordered}}

let d = [1 : "foo"]
let kvs = [(key: 1, value: "foo")]
_ = d.elementsEqual(kvs, by: { _ in true }) // expected-warning {{unordered}}
_ = kvs.elementsEqual(d, by: { _ in true }) // expected-warning {{unordered}}

#endif
