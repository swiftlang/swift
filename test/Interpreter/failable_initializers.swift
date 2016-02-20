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

var FailableInitTestSuite = TestSuite("FailableInit")

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

  init?(n: Int, before: Bool) {
    if before {
      return nil
    }
    self.x = Canary()
  }

  init?(n: Int, after: Bool) {
    self.x = Canary()
    if after {
      return nil
    }
  }

  init?(n: Int, before: Bool, after: Bool) {
    if before {
      return nil
    }
    self.x = Canary()
    if after {
      return nil
    }
  }

  /* Convenience */
  convenience init?(before: Bool) {
    if before {
      return nil
    }
    self.init(n: 0)
  }

  convenience init?(during: Bool) {
    self.init(n: 0, after: during)
  }

  convenience init?(before: Bool, during: Bool) {
    if before {
      return nil
    }
    self.init(n: 0, after: during)
  }

  convenience init?(after: Bool) {
    self.init(n: 0)
    if after {
      return nil
    }
  }

  convenience init?(before: Bool, after: Bool) {
    if before {
      return nil
    }
    self.init(n: 0)
    if after {
      return nil
    }
  }

  convenience init?(during: Bool, after: Bool) {
    self.init(n: 0, after: during)
    if after {
      return nil
    }
  }

  convenience init?(before: Bool, during: Bool, after: Bool) {
    if before {
      return nil
    }
    self.init(n: 0, after: during)
    if after {
      return nil
    }
  }

  /* Exotic */
  convenience init!(IUO: Bool) {
    self.init(before: IUO)
  }

  convenience init(force: Bool) {
    self.init(before: force)!
  }
}

class PolarBear : Bear {
  let y: Canary

  /* Designated */
  override init(n: Int) {
    self.y = Canary()
    super.init(n: n)
  }

  override init?(n: Int, before: Bool) {
    if before {
      return nil
    }
    self.y = Canary()
    super.init(n: n)
  }

  init?(n: Int, during: Bool) {
    self.y = Canary()
    super.init(n: n, before: during)
  }

  init?(n: Int, before: Bool, during: Bool) {
    self.y = Canary()
    if before {
      return nil
    }
    super.init(n: n, before: during)
  }

  override init?(n: Int, after: Bool) {
    self.y = Canary()
    super.init(n: n)
    if after {
      return nil
    }
  }

  init?(n: Int, during: Bool, after: Bool) {
    self.y = Canary()
    super.init(n: n, before: during)
    if after {
      return nil
    }
  }

  override init?(n: Int, before: Bool, after: Bool) {
    if before {
      return nil
    }
    self.y = Canary()
    super.init(n: n)
    if after {
      return nil
    }
  }

  init?(n: Int, before: Bool, during: Bool, after: Bool) {
    if before {
      return nil
    }
    self.y = Canary()
    super.init(n: n, before: during)
    if after {
      return nil
    }
  }
}

class GuineaPig<T> : Bear {
  let y: Canary
  let t: T

  init?(t: T, during: Bool) {
    self.y = Canary()
    self.t = t
    super.init(n: 0, before: during)
  }
}

struct Chimera {
  let x: Canary
  let y: Canary

  init?(before: Bool) {
    if before {
      return nil
    }
    x = Canary()
    y = Canary()
  }

  init?(during: Bool) {
    x = Canary()
    if during {
      return nil
    }
    y = Canary()
  }

  init?(before: Bool, during: Bool) {
    if before {
      return nil
    }
    x = Canary()
    if during {
      return nil
    }
    y = Canary()
  }

  init?(after: Bool) {
    x = Canary()
    y = Canary()
    if after {
      return nil
    }
  }

  init?(before: Bool, after: Bool) {
    if before {
      return nil
    }
    x = Canary()
    y = Canary()
    if after {
      return nil
    }
  }

  init?(during: Bool, after: Bool) {
    x = Canary()
    if during {
      return nil
    }
    y = Canary()
    if after {
      return nil
    }
  }

  init?(before: Bool, during: Bool, after: Bool) {
    if before {
      return nil
    }
    x = Canary()
    if during {
      return nil
    }
    y = Canary()
    if after {
      return nil
    }
  }
}

func mustFail<T>(f: () -> T?) {
  if f() != nil {
    preconditionFailure("Didn't fail")
  }
}

FailableInitTestSuite.test("FailableInitFailure_Root") {
  mustFail { Bear(n: 0, before: true) }
  mustFail { Bear(n: 0, after: true) }
  mustFail { Bear(n: 0, before: true, after: false) }
  mustFail { Bear(n: 0, before: false, after: true) }

  expectEqual(0, Canary.count)
}

FailableInitTestSuite.test("FailableInitFailure_Derived") {
  mustFail { PolarBear(n: 0, before: true) }
  mustFail { PolarBear(n: 0, during: true) }
  mustFail { PolarBear(n: 0, before: true, during: false) }
  mustFail { PolarBear(n: 0, before: false, during: true) }
  mustFail { PolarBear(n: 0, after: true) }
  mustFail { PolarBear(n: 0, during: true, after: false) }
  mustFail { PolarBear(n: 0, during: false, after: true) }
  mustFail { PolarBear(n: 0, before: true, after: false) }
  mustFail { PolarBear(n: 0, before: false, after: true) }
  mustFail { PolarBear(n: 0, before: true, during: false, after: false) }
  mustFail { PolarBear(n: 0, before: false, during: true, after: false) }
  mustFail { PolarBear(n: 0, before: false, during: false, after: true) }

  expectEqual(0, Canary.count)
}

FailableInitTestSuite.test("DesignatedInitFailure_DerivedGeneric") {
  mustFail { GuineaPig<Canary>(t: Canary(), during: true) }

  expectEqual(0, Canary.count)
}

FailableInitTestSuite.test("ConvenienceInitFailure_Root") {
  mustFail { Bear(before: true) }
  mustFail { Bear(during: true) }
  mustFail { Bear(before: true, during: false) }
  mustFail { Bear(before: false, during: true) }
  mustFail { Bear(after: true) }
  mustFail { Bear(before: true, after: false) }
  mustFail { Bear(before: false, after: true) }
  mustFail { Bear(during: true, after: false) }
  mustFail { Bear(during: false, after: true) }
  mustFail { Bear(before: true, during: false, after: false) }
  mustFail { Bear(before: false, during: true, after: false) }
  mustFail { Bear(before: false, during: false, after: true) }

  _ = Bear(IUO: false)
  _ = Bear(force: false)

  expectEqual(0, Canary.count)
}

FailableInitTestSuite.test("ConvenienceInitFailure_Derived") {
  mustFail { PolarBear(before: true) }
  mustFail { PolarBear(during: true) }
  mustFail { PolarBear(before: true, during: false) }
  mustFail { PolarBear(before: false, during: true) }
  mustFail { PolarBear(after: true) }
  mustFail { PolarBear(before: true, after: false) }
  mustFail { PolarBear(before: false, after: true) }
  mustFail { PolarBear(during: true, after: false) }
  mustFail { PolarBear(during: false, after: true) }
  mustFail { PolarBear(before: true, during: false, after: false) }
  mustFail { PolarBear(before: false, during: true, after: false) }
  mustFail { PolarBear(before: false, during: false, after: true) }

  expectEqual(0, Canary.count)
}

FailableInitTestSuite.test("InitFailure_Struct") {
  mustFail { Chimera(before: true) }
  mustFail { Chimera(during: true) }
  mustFail { Chimera(before: true, during: false) }
  mustFail { Chimera(before: false, during: true) }
  mustFail { Chimera(after: true) }
  mustFail { Chimera(before: true, after: false) }
  mustFail { Chimera(before: false, after: true) }
  mustFail { Chimera(during: true, after: false) }
  mustFail { Chimera(during: false, after: true) }
  mustFail { Chimera(before: true, during: false, after: false) }
  mustFail { Chimera(before: false, during: true, after: false) }
  mustFail { Chimera(before: false, during: false, after: true) }

  expectEqual(0, Canary.count)
}

runAllTests()
