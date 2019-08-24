// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ObjCClasses
import StdlibUnittest

var ThrowingInitTestSuite = TestSuite("ThrowingInitObjC")

enum E : Error {
  case X
}

func unwrap(_ b: Bool) throws -> Int {
  if b {
    throw E.X
  }
  return 0
}

class Bear : NSLifetimeTracked {
  let x: LifetimeTracked

  /* Designated */
  init(n: Int) {
    x = LifetimeTracked(0)
  }

  init(n: Int, before: Bool) throws {
    if before {
      throw E.X
    }
    self.x = LifetimeTracked(0)
  }

  init(n: Int, after: Bool) throws {
    self.x = LifetimeTracked(0)
    if after {
      throw E.X
    }
  }

  init(n: Int, before: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    self.x = LifetimeTracked(0)
    if after {
      throw E.X
    }
  }

  /* Convenience */
  convenience init(before: Bool) throws {
    try unwrap(before)
    self.init(n: 0)
  }

  convenience init(before2: Bool) throws {
    try self.init(n: unwrap(before2))
  }

  convenience init(before: Bool, before2: Bool) throws {
    try unwrap(before)
    try self.init(n: unwrap(before2))
  }

  convenience init(during: Bool) throws {
    try self.init(n: 0, after: during)
  }

  convenience init(before: Bool, during: Bool) throws {
    try unwrap(before)
    try self.init(n: 0, after: during)
  }

  convenience init(after: Bool) throws {
    self.init(n: 0)
    try unwrap(after)
  }

  convenience init(before: Bool, after: Bool) throws {
    try unwrap(before)
    self.init(n: 0)
    try unwrap(after)
  }

  convenience init(during: Bool, after: Bool) throws {
    try self.init(n: 0, after: during)
    try unwrap(after)
  }

  convenience init(before: Bool, during: Bool, after: Bool) throws {
    try unwrap(before)
    try self.init(n: 0, after: during)
    try unwrap(after)
  }

  convenience init(before: Bool, before2: Bool, during: Bool, after: Bool) throws {
    try unwrap(before)
    try self.init(n: unwrap(before2), after: during)
    try unwrap(after)
  }
}

class PolarBear : Bear {
  let y: LifetimeTracked

  /* Designated */
  override init(n: Int) {
    self.y = LifetimeTracked(0)
    super.init(n: n)
  }

  override init(n: Int, before: Bool) throws {
    if before {
      throw E.X
    }
    self.y = LifetimeTracked(0)
    super.init(n: n)
  }

  init(n: Int, during: Bool) throws {
    self.y = LifetimeTracked(0)
    try super.init(n: n, before: during)
  }

  init(n: Int, before: Bool, during: Bool) throws {
    self.y = LifetimeTracked(0)
    if before {
      throw E.X
    }
    try super.init(n: n, before: during)
  }

  override init(n: Int, after: Bool) throws {
    self.y = LifetimeTracked(0)
    super.init(n: n)
    if after {
      throw E.X
    }
  }

  init(n: Int, during: Bool, after: Bool) throws {
    self.y = LifetimeTracked(0)
    try super.init(n: n, before: during)
    if after {
      throw E.X
    }
  }

  override init(n: Int, before: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    self.y = LifetimeTracked(0)
    super.init(n: n)
    if after {
      throw E.X
    }
  }

  init(n: Int, before: Bool, during: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    self.y = LifetimeTracked(0)
    try super.init(n: n, before: during)
    if after {
      throw E.X
    }
  }
}

class GuineaPig<T> : Bear {
  let y: LifetimeTracked
  let t: T

  init(t: T, during: Bool) throws {
    self.y = LifetimeTracked(0)
    self.t = t
    try super.init(n: 0, before: during)
  }
}

func mustThrow<T>(_ f: () throws -> T) {
  do {
    try f()
    preconditionFailure("Didn't throw")
  } catch {}
}

ThrowingInitTestSuite.test("DesignatedInitSuccess_Root") {
  _ = try! Bear(n: 0, before: false)
  _ = try! Bear(n: 0, after: false)
  _ = try! Bear(n: 0, before: false, after: false)
  _ = try! Bear(n: 0, before: false, after: false)
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("DesignatedInitFailure_Root") {
  mustThrow { try Bear(n: 0, before: true) }
  mustThrow { try Bear(n: 0, after: true) }
  mustThrow { try Bear(n: 0, before: true, after: false) }
  mustThrow { try Bear(n: 0, before: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("DesignatedInitSuccess_Derived") {
  _ = try! PolarBear(n: 0, before: false)
  _ = try! PolarBear(n: 0, during: false)
  _ = try! PolarBear(n: 0, before: false, during: false)
  _ = try! PolarBear(n: 0, after: false)
  _ = try! PolarBear(n: 0, during: false, after: false)
  _ = try! PolarBear(n: 0, before: false, after: false)
  _ = try! PolarBear(n: 0, before: false, during: false, after: false)
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("DesignatedInitFailure_Derived") {
  mustThrow { try PolarBear(n: 0, before: true) }
  mustThrow { try PolarBear(n: 0, during: true) }
  mustThrow { try PolarBear(n: 0, before: true, during: false) }
  mustThrow { try PolarBear(n: 0, before: false, during: true) }
  mustThrow { try PolarBear(n: 0, after: true) }
  mustThrow { try PolarBear(n: 0, during: true, after: false) }
  mustThrow { try PolarBear(n: 0, during: false, after: true) }
  mustThrow { try PolarBear(n: 0, before: true, after: false) }
  mustThrow { try PolarBear(n: 0, before: false, after: true) }
  mustThrow { try PolarBear(n: 0, before: true, during: false, after: false) }
  mustThrow { try PolarBear(n: 0, before: false, during: true, after: false) }
  mustThrow { try PolarBear(n: 0, before: false, during: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("DesignatedInitSuccess_DerivedGeneric") {
  _ = try! GuineaPig(t: LifetimeTracked(0), during: false)
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("DesignatedInitFailure_DerivedGeneric") {
  mustThrow { try GuineaPig(t: LifetimeTracked(0), during: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("ConvenienceInitSuccess_Root") {
  _ = try! Bear(before: false)
  _ = try! Bear(before2: false)
  _ = try! Bear(before: false, before2: false)
  _ = try! Bear(during: false)
  _ = try! Bear(before: false, during: false)
  _ = try! Bear(after: false)
  _ = try! Bear(before: false, after: false)
  _ = try! Bear(during: false, after: false)
  _ = try! Bear(before: false, during: false, after: false)
  _ = try! Bear(before: false, before2: false, during: false, after: false)
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("ConvenienceInitFailure_Root") {
  mustThrow { try Bear(before: true) }
  mustThrow { try Bear(before2: true) }
  mustThrow { try Bear(before: true, before2: false) }
  mustThrow { try Bear(before: false, before2: true) }
  mustThrow { try Bear(during: true) }
  mustThrow { try Bear(before: true, during: false) }
  mustThrow { try Bear(before: false, during: true) }
  mustThrow { try Bear(after: true) }
  mustThrow { try Bear(before: true, after: false) }
  mustThrow { try Bear(before: false, after: true) }
  mustThrow { try Bear(during: true, after: false) }
  mustThrow { try Bear(during: false, after: true) }
  mustThrow { try Bear(before: true, during: false, after: false) }
  mustThrow { try Bear(before: false, during: true, after: false) }
  mustThrow { try Bear(before: false, during: false, after: true) }
  mustThrow { try Bear(before: true, before2: false, during: false, after: false) }
  mustThrow { try Bear(before: false, before2: true, during: false, after: false) }
  mustThrow { try Bear(before: false, before2: false, during: true, after: false) }
  mustThrow { try Bear(before: false, before2: false, during: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("ConvenienceInitSuccess_Derived") {
  _ = try! PolarBear(before: false)
  _ = try! PolarBear(before2: false)
  _ = try! PolarBear(before: false, before2: false)
  _ = try! PolarBear(during: false)
  _ = try! PolarBear(before: false, during: false)
  _ = try! PolarBear(after: false)
  _ = try! PolarBear(before: false, after: false)
  _ = try! PolarBear(during: false, after: false)
  _ = try! PolarBear(before: false, during: false, after: false)
  _ = try! PolarBear(before: false, before2: false, during: false, after: false)
  expectEqual(NSLifetimeTracked.count(), 0)
}

ThrowingInitTestSuite.test("ConvenienceInitFailure_Derived") {
  mustThrow { try PolarBear(before: true) }
  mustThrow { try PolarBear(before2: true) }
  mustThrow { try PolarBear(before: true, before2: false) }
  mustThrow { try PolarBear(before: false, before2: true) }
  mustThrow { try PolarBear(during: true) }
  mustThrow { try PolarBear(before: true, during: false) }
  mustThrow { try PolarBear(before: false, during: true) }
  mustThrow { try PolarBear(after: true) }
  mustThrow { try PolarBear(before: true, after: false) }
  mustThrow { try PolarBear(before: false, after: true) }
  mustThrow { try PolarBear(during: true, after: false) }
  mustThrow { try PolarBear(during: false, after: true) }
  mustThrow { try PolarBear(before: true, during: false, after: false) }
  mustThrow { try PolarBear(before: false, during: true, after: false) }
  mustThrow { try PolarBear(before: false, during: false, after: true) }
  mustThrow { try PolarBear(before: true, before2: false, during: false, after: false) }
  mustThrow { try PolarBear(before: false, before2: true, during: false, after: false) }
  mustThrow { try PolarBear(before: false, before2: false, during: true, after: false) }
  mustThrow { try PolarBear(before: false, before2: false, during: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

// Specific regression tests:

// <rdar://problem/28687665> - "overreleased while already deallocating", for "throws" convenience init in extension
extension NSRegularExpression {
  convenience init(foo: String, options: NSRegularExpression.Options) throws {
    try self.init(pattern: foo, options: options)
  }
}

ThrowingInitTestSuite.test("ConvenienceInitFailure_Dynamic") {
  mustThrow { try NSRegularExpression(foo: "(", options: []) }
}

runAllTests()
