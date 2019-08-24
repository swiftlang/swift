// RUN: %empty-directory(%t)
// RUN: %target-build-swift -swift-version 5 %s -o %t/a.out
// RUN: %target-codesign %t/a.out
//
// RUN: %target-run %t/a.out
// REQUIRES: executable_test

import StdlibUnittest

var ProtocolInitTestSuite = TestSuite("ProtocolInit")

func mustThrow<T>(_ f: () throws -> T) {
  do {
    _ = try f()
    preconditionFailure("Didn't throw")
  } catch {}
}

func mustFail<T>(f: () -> T?) {
  if f() != nil {
    preconditionFailure("Didn't fail")
  }
}

enum E : Error { case X }

protocol TriviallyConstructible {
  init(x: LifetimeTracked)
  init(x: LifetimeTracked, throwsDuring: Bool) throws
  init?(x: LifetimeTracked, failsDuring: Bool)
}

extension TriviallyConstructible {
  init(x: LifetimeTracked, throwsBefore: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    self.init(x: x)
  }

  init(x: LifetimeTracked, throwsAfter: Bool) throws {
    self.init(x: x)
    if throwsAfter {
      throw E.X
    }
  }

  init(x: LifetimeTracked, throwsBefore: Bool, throwsDuring: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    try self.init(x: x, throwsDuring: throwsDuring)
  }

  init(x: LifetimeTracked, throwsBefore: Bool, throwsAfter: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    self.init(x: x)
    if throwsAfter {
      throw E.X
    }
  }

  init(x: LifetimeTracked, throwsDuring: Bool, throwsAfter: Bool) throws {
    try self.init(x: x, throwsDuring: throwsDuring)
    if throwsAfter {
      throw E.X
    }
  }

  init(x: LifetimeTracked, throwsBefore: Bool, throwsDuring: Bool, throwsAfter: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    try self.init(x: x, throwsDuring: throwsDuring)
    if throwsAfter {
      throw E.X
    }
  }

  init?(x: LifetimeTracked, failsBefore: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: x)
  }

  init?(x: LifetimeTracked, failsAfter: Bool) {
    self.init(x: x)
    if failsAfter {
      return nil
    }
  }

  init?(x: LifetimeTracked, failsBefore: Bool, failsDuring: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: x, failsDuring: failsDuring)
  }

  init?(x: LifetimeTracked, failsBefore: Bool, failsAfter: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: x)
    if failsAfter {
      return nil
    }
  }

  init?(x: LifetimeTracked, failsDuring: Bool, failsAfter: Bool) {
    self.init(x: x, failsDuring: failsDuring)
    if failsAfter {
      return nil
    }
  }

  init?(x: LifetimeTracked, failsBefore: Bool, failsDuring: Bool, failsAfter: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: x, failsDuring: failsDuring)
    if failsAfter {
      return nil
    }
  }
}

class TrivialClass : TriviallyConstructible {
  let tracker: LifetimeTracked

  // Protocol requirements
  required init(x: LifetimeTracked) {
    self.tracker = x
  }

  required convenience init(x: LifetimeTracked, throwsDuring: Bool) throws {
    self.init(x: x)
    if throwsDuring {
      throw E.X
    }
  }

  required convenience init?(x: LifetimeTracked, failsDuring: Bool) {
    self.init(x: x)
    if failsDuring {
      return nil
    }
  }

  // Class initializers delegating to protocol initializers
  convenience init(throwsBefore: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    self.init(x: LifetimeTracked(0))
  }

  convenience init(throwsAfter: Bool) throws {
    self.init(x: LifetimeTracked(0))
    if throwsAfter {
      throw E.X
    }
  }

  convenience init(throwsBefore: Bool, throwsDuring: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsDuring)
  }

  convenience init(throwsBefore: Bool, throwsAfter: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    self.init(x: LifetimeTracked(0))
    if throwsAfter {
      throw E.X
    }
  }

  convenience init(throwsDuring: Bool, throwsAfter: Bool) throws {
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsDuring)
    if throwsAfter {
      throw E.X
    }
  }

  convenience init(throwsBefore: Bool, throwsDuring: Bool, throwsAfter: Bool) throws {
    if throwsBefore {
      throw E.X
    }
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsDuring)
    if throwsAfter {
      throw E.X
    }
  }

  convenience init?(failsBefore: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: LifetimeTracked(0))
  }

  convenience init?(failsAfter: Bool) {
    self.init(x: LifetimeTracked(0))
    if failsAfter {
      return nil
    }
  }

  convenience init?(failsBefore: Bool, failsDuring: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: LifetimeTracked(0), failsDuring: failsDuring)
  }

  convenience init?(failsBefore: Bool, failsAfter: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: LifetimeTracked(0))
    if failsAfter {
      return nil
    }
  }

  convenience init?(failsDuring: Bool, failsAfter: Bool) {
    self.init(x: LifetimeTracked(0), failsDuring: failsDuring)
    if failsAfter {
      return nil
    }
  }

  convenience init?(failsBefore: Bool, failsDuring: Bool, failsAfter: Bool) {
    if failsBefore {
      return nil
    }
    self.init(x: LifetimeTracked(0), failsDuring: failsDuring)
    if failsAfter {
      return nil
    }
  }
}

ProtocolInitTestSuite.test("ExtensionInit_Success") {
  _ = try! TrivialClass(x: LifetimeTracked(0), throwsBefore: false)
  _ = try! TrivialClass(x: LifetimeTracked(0), throwsAfter: false)
  _ = try! TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsDuring: false)
  _ = try! TrivialClass(x: LifetimeTracked(0), throwsDuring: false, throwsAfter: false)
  _ = try! TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsAfter: false)
  _ = try! TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsDuring: false, throwsAfter: false)

  _ = TrivialClass(x: LifetimeTracked(0), failsBefore: false)!
  _ = TrivialClass(x: LifetimeTracked(0), failsAfter: false)!
  _ = TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsDuring: false)!
  _ = TrivialClass(x: LifetimeTracked(0), failsDuring: false, failsAfter: false)!
  _ = TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsAfter: false)!
  _ = TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsDuring: false, failsAfter: false)!
}

ProtocolInitTestSuite.test("ClassInit_Success") {
  _ = try! TrivialClass(throwsBefore: false)
  _ = try! TrivialClass(throwsAfter: false)
  _ = try! TrivialClass(throwsBefore: false, throwsDuring: false)
  _ = try! TrivialClass(throwsDuring: false, throwsAfter: false)
  _ = try! TrivialClass(throwsBefore: false, throwsAfter: false)
  _ = try! TrivialClass(throwsBefore: false, throwsDuring: false, throwsAfter: false)

  _ = TrivialClass(failsBefore: false)!
  _ = TrivialClass(failsAfter: false)!
  _ = TrivialClass(failsBefore: false, failsDuring: false)!
  _ = TrivialClass(failsDuring: false, failsAfter: false)!
  _ = TrivialClass(failsBefore: false, failsAfter: false)!
  _ = TrivialClass(failsBefore: false, failsDuring: false, failsAfter: false)!
}

ProtocolInitTestSuite.test("ExtensionInit_Failure") {
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: true) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsAfter: true) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: true, throwsDuring: false) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsDuring: true) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsDuring: true, throwsAfter: false) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsDuring: false, throwsAfter: true) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: true, throwsAfter: false) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsAfter: true) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: true, throwsDuring: false, throwsAfter: false) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsDuring: true, throwsAfter: false) }
  mustThrow { try TrivialClass(x: LifetimeTracked(0), throwsBefore: false, throwsDuring: false, throwsAfter: true) }

  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: true) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsAfter: true) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: true, failsDuring: false) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsDuring: true) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsDuring: true, failsAfter: false) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsDuring: false, failsAfter: true) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: true, failsAfter: false) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsAfter: true) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: true, failsDuring: false, failsAfter: false) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsDuring: true, failsAfter: false) }
  mustFail { TrivialClass(x: LifetimeTracked(0), failsBefore: false, failsDuring: false, failsAfter: true) }
}

ProtocolInitTestSuite.test("ClassInit_Failure") {
  mustThrow { try TrivialClass(throwsBefore: true) }
  mustThrow { try TrivialClass(throwsAfter: true) }
  mustThrow { try TrivialClass(throwsBefore: true, throwsDuring: false) }
  mustThrow { try TrivialClass(throwsBefore: false, throwsDuring: true) }
  mustThrow { try TrivialClass(throwsDuring: true, throwsAfter: false) }
  mustThrow { try TrivialClass(throwsDuring: false, throwsAfter: true) }
  mustThrow { try TrivialClass(throwsBefore: true, throwsAfter: false) }
  mustThrow { try TrivialClass(throwsBefore: false, throwsAfter: true) }
  mustThrow { try TrivialClass(throwsBefore: true, throwsDuring: false, throwsAfter: false) }
  mustThrow { try TrivialClass(throwsBefore: false, throwsDuring: true, throwsAfter: false) }
  mustThrow { try TrivialClass(throwsBefore: false, throwsDuring: false, throwsAfter: true) }

  mustFail { TrivialClass(failsBefore: true) }
  mustFail { TrivialClass(failsAfter: true) }
  mustFail { TrivialClass(failsBefore: true, failsDuring: false) }
  mustFail { TrivialClass(failsBefore: false, failsDuring: true) }
  mustFail { TrivialClass(failsDuring: true, failsAfter: false) }
  mustFail { TrivialClass(failsDuring: false, failsAfter: true) }
  mustFail { TrivialClass(failsBefore: true, failsAfter: false) }
  mustFail { TrivialClass(failsBefore: false, failsAfter: true) }
  mustFail { TrivialClass(failsBefore: true, failsDuring: false, failsAfter: false) }
  mustFail { TrivialClass(failsBefore: false, failsDuring: true, failsAfter: false) }
  mustFail { TrivialClass(failsBefore: false, failsDuring: false, failsAfter: true) }
}

struct TrivialStruct : TriviallyConstructible {
  let x: LifetimeTracked

  init(x: LifetimeTracked) {
    self.x = x
  }

  init(x: LifetimeTracked, throwsDuring: Bool) throws {
    self.init(x: x)
    if throwsDuring {
      throw E.X
    }
  }

  init?(x: LifetimeTracked, failsDuring: Bool) {
    self.init(x: x)
    if failsDuring {
      return nil
    }
  }
}

struct AddrOnlyStruct : TriviallyConstructible {
  let x: LifetimeTracked
  let y: Any

  init(x: LifetimeTracked) {
    self.x = x
    self.y = "Hello world"
  }

  init(x: LifetimeTracked, throwsDuring: Bool) throws {
    self.init(x: x)
    if throwsDuring {
      throw E.X
    }
  }

  init?(x: LifetimeTracked, failsDuring: Bool) {
    self.init(x: x)
    if failsDuring {
      return nil
    }
  }
}

extension TriviallyConstructible {
  init(throwsFirst: Bool, throwsSecond: Bool, initThenInit: ()) throws {
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsFirst)
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsSecond)
  }

  init(throwsFirst: Bool, throwsSecond: Bool, initThenAssign: ()) throws {
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsFirst)
    self = try Self(x: LifetimeTracked(0), throwsDuring: throwsSecond)
  }

  init(throwsFirst: Bool, throwsSecond: Bool, assignThenInit: ()) throws {
    self = try Self(x: LifetimeTracked(0), throwsDuring: throwsFirst)
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsSecond)
  }

  init(throwsFirst: Bool, throwsSecond: Bool, assignThenAssign: ()) throws {
    self = try Self(x: LifetimeTracked(0), throwsDuring: throwsFirst)
    try self.init(x: LifetimeTracked(0), throwsDuring: throwsSecond)
  }
}

ProtocolInitTestSuite.test("Struct_Success") {
  _ = try! TrivialStruct(throwsFirst: false, throwsSecond: false, initThenInit: ())
  _ = try! TrivialStruct(throwsFirst: false, throwsSecond: false, initThenAssign: ())
  _ = try! TrivialStruct(throwsFirst: false, throwsSecond: false, assignThenInit: ())
  _ = try! TrivialStruct(throwsFirst: false, throwsSecond: false, assignThenAssign: ())
}

ProtocolInitTestSuite.test("Struct_Failure") {
  mustThrow { try TrivialStruct(throwsFirst: true, throwsSecond: false, initThenInit: ()) }
  mustThrow { try TrivialStruct(throwsFirst: false, throwsSecond: true, initThenInit: ()) }
  mustThrow { try TrivialStruct(throwsFirst: true, throwsSecond: false, initThenAssign: ()) }
  mustThrow { try TrivialStruct(throwsFirst: false, throwsSecond: true, initThenAssign: ()) }
  mustThrow { try TrivialStruct(throwsFirst: true, throwsSecond: false, assignThenInit: ()) }
  mustThrow { try TrivialStruct(throwsFirst: false, throwsSecond: true, assignThenInit: ()) }
  mustThrow { try TrivialStruct(throwsFirst: true, throwsSecond: false, assignThenAssign: ()) }
  mustThrow { try TrivialStruct(throwsFirst: false, throwsSecond: true, assignThenAssign: ()) }
}

ProtocolInitTestSuite.test("AddrOnlyStruct_Success") {
  _ = try! AddrOnlyStruct(throwsFirst: false, throwsSecond: false, initThenInit: ())
  _ = try! AddrOnlyStruct(throwsFirst: false, throwsSecond: false, initThenAssign: ())
  _ = try! AddrOnlyStruct(throwsFirst: false, throwsSecond: false, assignThenInit: ())
  _ = try! AddrOnlyStruct(throwsFirst: false, throwsSecond: false, assignThenAssign: ())
}

ProtocolInitTestSuite.test("AddrOnlyStruct_Failure") {
  mustThrow { try AddrOnlyStruct(throwsFirst: true, throwsSecond: false, initThenInit: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: false, throwsSecond: true, initThenInit: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: true, throwsSecond: false, initThenAssign: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: false, throwsSecond: true, initThenAssign: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: true, throwsSecond: false, assignThenInit: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: false, throwsSecond: true, assignThenInit: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: true, throwsSecond: false, assignThenAssign: ()) }
  mustThrow { try AddrOnlyStruct(throwsFirst: false, throwsSecond: true, assignThenAssign: ()) }
}

runAllTests()
