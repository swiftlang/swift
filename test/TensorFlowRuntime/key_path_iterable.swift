// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 -g %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
//
// `KeyPathIterable` tests.

import TensorFlow
import StdlibUnittest

var KeyPathIterableTests = TestSuite("KeyPathIterable")

struct Simple : KeyPathIterable, Equatable {
  var w, b: Float
}

struct Mixed : KeyPathIterable, Equatable {
  // Mutable.
  var string: String
  var float: Float
  // Immutable.
  let int: Int
}

struct Nested : KeyPathIterable, Equatable {
  // Immutable.
  let simple: Simple
  // Mutable.
  var mixed: Mixed
}

struct ComplexNested : KeyPathIterable, Equatable {
  var float: Float
  let simple: Simple
  let array: [Simple]
  var dictionary: [String : Simple]
}

KeyPathIterableTests.test("Simple") {
  var x = Simple(w: 1, b: 2)
  expectEqual([\Simple.w, \Simple.b], x.allKeyPaths)
  expectEqual([\Simple.w, \Simple.b], x.allKeyPaths(to: Float.self))
  expectEqual([\Simple.w, \Simple.b], x.allWritableKeyPaths(to: Float.self))
  expectEqual([\Simple.w, \Simple.b], x.recursivelyAllKeyPaths)
  expectEqual([\Simple.w, \Simple.b], x.recursivelyAllKeyPaths(to: Float.self))
  expectEqual([\Simple.w, \Simple.b], x.recursivelyAllWritableKeyPaths(to: Float.self))
  expectEqual([], x.allKeyPaths(to: Int.self))
  expectEqual([], x.recursivelyAllKeyPaths(to: Double.self))

  for kp in x.allWritableKeyPaths(to: Float.self) {
    x[keyPath: kp] += x[keyPath: kp]
  }
  expectEqual(Simple(w: 2, b: 4), x)
}

KeyPathIterableTests.test("Mixed") {
  var x = Mixed(string: "hello", float: .pi, int: 0)
  expectEqual([\Mixed.string, \Mixed.float, \Mixed.int], x.allKeyPaths)
  expectEqual([\Mixed.string, \Mixed.float, \Mixed.int], x.recursivelyAllKeyPaths)

  expectEqual([\Mixed.string], x.allKeyPaths(to: String.self))
  expectEqual([\Mixed.string], x.allWritableKeyPaths(to: String.self))
  expectEqual([\Mixed.string], x.recursivelyAllKeyPaths(to: String.self))
  expectEqual([\Mixed.string], x.recursivelyAllWritableKeyPaths(to: String.self))

  expectEqual([\Mixed.float], x.allKeyPaths(to: Float.self))
  expectEqual([\Mixed.float], x.allWritableKeyPaths(to: Float.self))
  expectEqual([\Mixed.float], x.recursivelyAllKeyPaths(to: Float.self))
  expectEqual([\Mixed.float], x.recursivelyAllWritableKeyPaths(to: Float.self))

  expectEqual([\Mixed.int], x.allKeyPaths(to: Int.self))
  expectEqual([], x.allWritableKeyPaths(to: Int.self))
  expectEqual([\Mixed.int], x.recursivelyAllKeyPaths(to: Int.self))
  expectEqual([], x.recursivelyAllWritableKeyPaths(to: Int.self))

  for kp in x.allWritableKeyPaths(to: String.self) {
    x[keyPath: kp] = x[keyPath: kp] + " world"
  }
  expectEqual(Mixed(string: "hello world", float: .pi, int: 0), x)
}

KeyPathIterableTests.test("SimpleNested") {
  var x = Nested(simple: Simple(w: 1, b: 2),
                 mixed: Mixed(string: "foo", float: 3, int: 0))

  expectEqual([\Nested.simple, \Nested.mixed], x.allKeyPaths)
  expectEqual([\Nested.simple, \Nested.simple.w, \Nested.simple.b,
               \Nested.mixed, \Nested.mixed.string,
               \Nested.mixed.float, \Nested.mixed.int],
              x.recursivelyAllKeyPaths)

  expectEqual([], x.allKeyPaths(to: Float.self))
  expectEqual([], x.allKeyPaths(to: Int.self))
  expectEqual([], x.allKeyPaths(to: String.self))

  expectEqual([], x.allWritableKeyPaths(to: Float.self))
  expectEqual([], x.allWritableKeyPaths(to: Int.self))
  expectEqual([], x.allWritableKeyPaths(to: String.self))

  expectEqual([\Nested.simple.w, \Nested.simple.b, \Nested.mixed.float],
              x.recursivelyAllKeyPaths(to: Float.self))
  expectEqual([\Nested.mixed.int], x.recursivelyAllKeyPaths(to: Int.self))
  expectEqual([\Nested.mixed.string], x.recursivelyAllKeyPaths(to: String.self))

  expectEqual([\Nested.mixed.float], x.recursivelyAllWritableKeyPaths(to: Float.self))
  expectEqual([], x.recursivelyAllWritableKeyPaths(to: Int.self))
  expectEqual([\Nested.mixed.string], x.recursivelyAllWritableKeyPaths(to: String.self))

  expectEqual([], x.recursivelyAllKeyPaths(to: Double.self))

  for kp in x.recursivelyAllWritableKeyPaths(to: Float.self) {
    x[keyPath: kp] *= 100
  }
  let expected = Nested(simple: Simple(w: 1, b: 2),
                        mixed: Mixed(string: "foo", float: 300, int: 0))
  expectEqual(expected, x)
}

KeyPathIterableTests.test("ComplexNested") {
  var x = ComplexNested(float: 1, simple: Simple(w: 3, b: 4),
                        array: [Simple(w: 5, b: 6), Simple(w: 7, b: 8)],
                        dictionary: ["foo" : Simple(w: 1, b: 2),
                                     "bar" : Simple(w: 3, b: 4)])
  expectEqual([\ComplexNested.float, \ComplexNested.simple,
               \ComplexNested.array, \ComplexNested.dictionary],
              x.allKeyPaths)
  expectEqual([\ComplexNested.float,
               \ComplexNested.simple,
               \ComplexNested.simple.w,
               \ComplexNested.simple.b,
               \ComplexNested.array,
               \ComplexNested.array[0],
               \ComplexNested.array[0].w,
               \ComplexNested.array[0].b,
               \ComplexNested.array[1],
               \ComplexNested.array[1].w,
               \ComplexNested.array[1].b,
               \ComplexNested.dictionary,
               \ComplexNested.dictionary["bar"]!,
               \ComplexNested.dictionary["bar"]!.w,
               \ComplexNested.dictionary["bar"]!.b,
               \ComplexNested.dictionary["foo"]!,
               \ComplexNested.dictionary["foo"]!.w,
               \ComplexNested.dictionary["foo"]!.b],
              x.recursivelyAllKeyPaths)
  expectEqual([\ComplexNested.float,
               \ComplexNested.simple.w,
               \ComplexNested.simple.b,
               \ComplexNested.array[0].w,
               \ComplexNested.array[0].b,
               \ComplexNested.array[1].w,
               \ComplexNested.array[1].b,
               \ComplexNested.dictionary["bar"]!.w,
               \ComplexNested.dictionary["bar"]!.b,
               \ComplexNested.dictionary["foo"]!.w,
               \ComplexNested.dictionary["foo"]!.b],
              x.recursivelyAllKeyPaths(to: Float.self))
  expectEqual([\ComplexNested.float,
               \ComplexNested.dictionary["bar"]!.w,
               \ComplexNested.dictionary["bar"]!.b,
               \ComplexNested.dictionary["foo"]!.w,
               \ComplexNested.dictionary["foo"]!.b],
              x.recursivelyAllWritableKeyPaths(to: Float.self))

  for kp in x.recursivelyAllWritableKeyPaths(to: Float.self) {
    x[keyPath: kp] += 1
  }
  let expected = ComplexNested(float: 2, simple: Simple(w: 3, b: 4),
                               array: [Simple(w: 5, b: 6), Simple(w: 7, b: 8)],
                               dictionary: ["foo" : Simple(w: 2, b: 3),
                                            "bar" : Simple(w: 4, b: 5)])
  expectEqual(expected, x)
}

runAllTests()
