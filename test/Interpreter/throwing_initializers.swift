// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest


var ThrowingInitTestSuite = TestSuite("ThrowingInit")

enum E : Error {
  case X
}

func unwrap(_ b: Bool) throws -> Int {
  if b {
    throw E.X
  }
  return 0
}

class Bear {
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

struct Chimera {
  let x: LifetimeTracked
  let y: LifetimeTracked

  init(before: Bool) throws {
    if before {
      throw E.X
    }
    x = LifetimeTracked(0)
    y = LifetimeTracked(0)
  }

  init(during: Bool) throws {
    x = LifetimeTracked(0)
    if during {
      throw E.X
    }
    y = LifetimeTracked(0)
  }

  init(before: Bool, during: Bool) throws {
    if before {
      throw E.X
    }
    x = LifetimeTracked(0)
    if during {
      throw E.X
    }
    y = LifetimeTracked(0)
  }

  init(after: Bool) throws {
    x = LifetimeTracked(0)
    y = LifetimeTracked(0)
    if after {
      throw E.X
    }
  }

  init(before: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    x = LifetimeTracked(0)
    y = LifetimeTracked(0)
    if after {
      throw E.X
    }
  }

  init(during: Bool, after: Bool) throws {
    x = LifetimeTracked(0)
    if during {
      throw E.X
    }
    y = LifetimeTracked(0)
    if after {
      throw E.X
    }
  }

  init(before: Bool, during: Bool, after: Bool) throws {
    if before {
      throw E.X
    }
    x = LifetimeTracked(0)
    if during {
      throw E.X
    }
    y = LifetimeTracked(0)
    if after {
      throw E.X
    }
  }
}

func mustThrow<T>(_ f: () throws -> T) {
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
}

ThrowingInitTestSuite.test("DesignatedInitFailure_DerivedGeneric") {
  mustThrow { try GuineaPig(t: LifetimeTracked(0), during: true) }
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
}

runAllTests()
