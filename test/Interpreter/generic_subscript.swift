//===--- generic_subscript.swift ------------------------------------------===//
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
//

import StdlibUnittest


var GenericSubscriptTestSuite = TestSuite("GenericSubscript")

struct S<T> : P {
  typealias Element = T
  var t: T

  subscript<U>(a: (T) -> U, b: (U) -> T) -> U {
    get {
      print(T.self)
      print(U.self)

      return a(t)
    }
    set {
      print(T.self)
      print(U.self)

      t = b(newValue)
    }
  }
}

protocol P {
  associatedtype Element
  subscript<U>(a: (Element) -> U, b: (U) -> Element) -> U { get set }
}

func increment<T : P>(p: inout T) where T.Element == String {
  p[{Int($0)!}, {String($0)}] += 1
}

GenericSubscriptTestSuite.test("Basic") {
  var s = S<String>(t: "0")
  increment(p: &s)
  expectEqual(s.t, "1")
}

protocol AnySubscript {
  subscript(k: AnyHashable) -> Any? { get set }
}

struct AnyDictionary : AnySubscript {
  var dict: [AnyHashable : Any] = [:]

  subscript(k: AnyHashable) -> Any? {
    get {
      return dict[k]
    }
    set {
      dict[k] = newValue
    }
  }
}

extension AnySubscript {
  subscript<K : Hashable, V>(k k: K) -> V? {
    get {
      return self[k] as! V?
    }
    set {
      self[k] = newValue
    }
  }
}

GenericSubscriptTestSuite.test("ProtocolExtensionConcrete") {
  var dict = AnyDictionary()

  func doIt(dict: inout AnyDictionary) {
    dict["a" ] = 0
    dict[k: "a"]! += 1
  }

  doIt(dict: &dict)

  expectEqual(dict["a"]! as! Int, 1)
  expectEqual(dict[k: "a"]!, 1)
}

GenericSubscriptTestSuite.test("ProtocolExtensionAbstract") {
  var dict = AnyDictionary()

  func doIt<T : AnySubscript>(dict: inout T) {
    dict["a" ] = 0
    dict[k: "a"]! += 1
  }

  doIt(dict: &dict)

  expectEqual(dict["a"]! as! Int, 1)
  expectEqual(dict[k: "a"]!, 1)
}

protocol GenericSubscript : AnySubscript {
  subscript<K : Hashable, V>(k k: K) -> V? { get set }
}

extension AnyDictionary : GenericSubscript { }

GenericSubscriptTestSuite.test("ProtocolExtensionWitness") {
  var dict = AnyDictionary()

  func doIt<T : GenericSubscript>(dict: inout T) {
    dict["a" ] = 0
    dict[k: "a"]! += 1
  }

  doIt(dict: &dict)

  expectEqual(dict["a"]! as! Int, 1)
  expectEqual(dict[k: "a"]!, 1)
}

runAllTests()
