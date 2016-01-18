// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

// Also import modules which are used by StdlibUnittest internally. This
// workaround is needed to link all required libraries in case we compile
// StdlibUnittest with -sil-serialize-all.
import SwiftPrivate
#if _runtime(_ObjC)
import ObjectiveC
#endif

var ThrowingInitTestSuite = TestSuite("ThrowingInit")

enum E : ErrorType {
  case X
}

func unwrap(b: Bool) throws -> Int {
  if b {
    throw E.X
  }
  return 0
}

class Canary {
  static var count: Int = 0

  init() {
    Canary.count += 1
  }

  deinit {
    Canary.count -= 1
  }
}

class Bear {
  let x: Canary

  /* Designated */
  init(n: Int) {
    x = Canary()
  }

  init(n: Int, before: Bool) throws {
    if before {
      throw E.X
    }
    self.x = Canary()
  }

  init(n: Int, after: Bool) throws {
    self.x = Canary()
    if after {
      throw E.X
    }
  }

  init(n: Int, before: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    self.x = Canary()
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
  let y: Canary

  /* Designated */
  override init(n: Int) {
    self.y = Canary()
    super.init(n: n)
  }

  override init(n: Int, before: Bool) throws {
    if before {
      throw E.X
    }
    self.y = Canary()
    super.init(n: n)
  }

  init(n: Int, during: Bool) throws {
    self.y = Canary()
    try super.init(n: n, before: during)
  }

  init(n: Int, before: Bool, during: Bool) throws {
    self.y = Canary()
    if before {
      throw E.X
    }
    try super.init(n: n, before: during)
  }

  override init(n: Int, after: Bool) throws {
    self.y = Canary()
    super.init(n: n)
    if after {
      throw E.X
    }
  }

  init(n: Int, during: Bool, after: Bool) throws {
    self.y = Canary()
    try super.init(n: n, before: during)
    if after {
      throw E.X
    }
  }

  override init(n: Int, before: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    self.y = Canary()
    super.init(n: n)
    if after {
      throw E.X
    }
  }

  init(n: Int, before: Bool, during: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    self.y = Canary()
    try super.init(n: n, before: during)
    if after {
      throw E.X
    }
  }
}

class GuineaPig<T> : Bear {
  let y: Canary
  let t: T

  init(t: T, during: Bool) throws {
    self.y = Canary()
    self.t = t
    try super.init(n: 0, before: during)
  }
}

struct Chimera {
  let x: Canary
  let y: Canary

  init(before: Bool) throws {
    if before {
      throw E.X
    }
    x = Canary()
    y = Canary()
  }

  init(during: Bool) throws {
    x = Canary()
    if during {
      throw E.X
    }
    y = Canary()
  }

  init(before: Bool, during: Bool) throws {
    if before {
      throw E.X
    }
    x = Canary()
    if during {
      throw E.X
    }
    y = Canary()
  }

  init(after: Bool) throws {
    x = Canary()
    y = Canary()
    if after {
      throw E.X
    }
  }

  init(before: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    x = Canary()
    y = Canary()
    if after {
      throw E.X
    }
  }

  init(during: Bool, after: Bool) throws {
    x = Canary()
    if during {
      throw E.X
    }
    y = Canary()
    if after {
      throw E.X
    }
  }

  init(before: Bool, during: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    x = Canary()
    if during {
      throw E.X
    }
    y = Canary()
    if after {
      throw E.X
    }
  }
}

func mustThrow<T>(f: () throws -> T) {
  do {
    try f()
    preconditionFailure("Didn't throw")
  } catch {}
}

ThrowingInitTestSuite.test("DesignatedInitFailure_Root") {
  mustThrow { try Bear(n: 0, before: true) }
  mustThrow { try Bear(n: 0, after: true) }
  mustThrow { try Bear(n: 0, before: true, after: false) }
  mustThrow { try Bear(n: 0, before: false, after: true) }

  expectEqual(0, Canary.count)
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

  expectEqual(Canary.count, 0)
}

ThrowingInitTestSuite.test("DesignatedInitFailure_DerivedGeneric") {
  mustThrow { try GuineaPig<Canary>(t: Canary(), during: true) }

  expectEqual(Canary.count, 0)
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

  expectEqual(Canary.count, 0)
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

  expectEqual(Canary.count, 0)
}

ThrowingInitTestSuite.test("InitFailure_Struct") {
  mustThrow { try Chimera(before: true) }
  mustThrow { try Chimera(during: true) }
  mustThrow { try Chimera(before: true, during: false) }
  mustThrow { try Chimera(before: false, during: true) }
  mustThrow { try Chimera(after: true) }
  mustThrow { try Chimera(before: true, after: false) }
  mustThrow { try Chimera(before: false, after: true) }
  mustThrow { try Chimera(during: true, after: false) }
  mustThrow { try Chimera(during: false, after: true) }
  mustThrow { try Chimera(before: true, during: false, after: false) }
  mustThrow { try Chimera(before: false, during: true, after: false) }
  mustThrow { try Chimera(before: false, during: false, after: true) }

  expectEqual(Canary.count, 0)
}

runAllTests()
