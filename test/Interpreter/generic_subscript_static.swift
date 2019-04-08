//===--- generic_subscript.swift ------------------------------------------===//
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
//

import StdlibUnittest


var GenericSubscriptTestSuite = TestSuite("GenericSubscriptStatic")

var ts: [ObjectIdentifier: Any] = [:]

struct S<T> : P {
  typealias Element = T
  static var t: T {
    get {
      return ts[ObjectIdentifier(self)] as! T
    }
    set {
      ts[ObjectIdentifier(self)] = newValue
    }
  }

  static subscript<U>(a: (T) -> U, b: (U) -> T) -> U {
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
  static subscript<U>(a: (Element) -> U, b: (U) -> Element) -> U { get set }
}

func increment<T : P>(p: T.Type) where T.Element == String {
  p[{Int($0)!}, {String($0)}] += 1
}

GenericSubscriptTestSuite.test("Basic") {
  var s = S<String>.self
  s.t = "0"
  increment(p: s)
  expectEqual(s.t, "1")
}

protocol AnySubscript {
  static subscript(k: AnyHashable) -> Any? { get set }
}

struct AnyDictionary : AnySubscript {
  static var dict: [AnyHashable : Any] = [:]

  static subscript(k: AnyHashable) -> Any? {
    get {
      return dict[k]
    }
    set {
      dict[k] = newValue
    }
  }
}

extension AnySubscript {
  static subscript<K : Hashable, V>(k k: K) -> V? {
    get {
      return self[k] as! V?
    }
    set {
      self[k] = newValue
    }
  }
}

GenericSubscriptTestSuite.test("ProtocolExtensionConcrete") {
  var dict = AnyDictionary.self

  func doIt(dict: AnyDictionary.Type) {
    dict["a" ] = 0
    dict[k: "a"]! += 1
  }

  doIt(dict: dict)

  expectEqual(dict["a"]! as! Int, 1)
  expectEqual(dict[k: "a"]!, 1)
}

GenericSubscriptTestSuite.test("ProtocolExtensionAbstract") {
  var dict = AnyDictionary.self

  func doIt<T : AnySubscript>(dict: T.Type) {
    dict["a" ] = 0
    dict[k: "a"]! += 1
  }

  doIt(dict: dict)

  expectEqual(dict["a"]! as! Int, 1)
  expectEqual(dict[k: "a"]!, 1)
}

protocol GenericSubscript : AnySubscript {
  static subscript<K : Hashable, V>(k k: K) -> V? { get set }
}

extension AnyDictionary : GenericSubscript { }

GenericSubscriptTestSuite.test("ProtocolExtensionWitness") {
  var dict = AnyDictionary.self

  func doIt<T : GenericSubscript>(dict: T.Type) {
    dict["a" ] = 0
    dict[k: "a"]! += 1
  }

  doIt(dict: dict)

  expectEqual(dict["a"]! as! Int, 1)
  expectEqual(dict[k: "a"]!, 1)
}

class EchoBase<T: SignedNumeric> {
  // In EchoDerived, subscript(a:) will be overridden.
  class subscript(a a: T) -> String {
    get {
      return "EchoBase.Type.subscript(a: \(a))"
    }
  }
  
  // In EchoDerived, subscript(b:) will be overridden with a super call.
  class subscript(b b: T) -> String {
    get {
      return "EchoBase.Type.subscript(b: \(b))"
    }
  }
  
  // In EchoDerived, subscript(c:) will not be overridden.
  class subscript(c c: T) -> String {
    get {
      return "EchoBase.Type.subscript(c: \(c))"
    }
  }
}

class EchoDerived<T: SignedNumeric>: EchoBase<T> {
  override class subscript(a a: T) -> String {
    get {
      return  "EchoDerived.Type.subscript(a: \(a))"
    }
  }
  
  override class subscript(b b: T) -> String {
    get {
      return "\(super[b: -b]) EchoDerived.Type.subscript(b: \(b))"
    }
  }
  
  class subscript(d d: T) -> String {
    return  "EchoDerived.Type.subscript(d: \(d))"
  }
}

GenericSubscriptTestSuite.test("Overrides") {
  let base = EchoBase<Int>.self
  let derived = EchoDerived<Int>.self
  
  expectEqual(base[a: 42], "EchoBase.Type.subscript(a: 42)")
  expectEqual(base[b: 42], "EchoBase.Type.subscript(b: 42)")
  expectEqual(base[c: 42], "EchoBase.Type.subscript(c: 42)")
  
  expectEqual(derived[a: 42], "EchoDerived.Type.subscript(a: 42)")
  expectEqual(derived[b: 42], "EchoBase.Type.subscript(b: -42) EchoDerived.Type.subscript(b: 42)")
  expectEqual(derived[c: 42], "EchoBase.Type.subscript(c: 42)")
  expectEqual(derived[d: 42], "EchoDerived.Type.subscript(d: 42)")
}

runAllTests()
