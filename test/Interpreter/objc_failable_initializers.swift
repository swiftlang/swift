// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/../Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/../Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

import Foundation
import ObjCClasses
import StdlibUnittest

var FailableInitTestSuite = TestSuite("FailableInitObjC")

class Bear : NSLifetimeTracked {
  let x: LifetimeTracked

  /* Designated */
  init(n: Int) {
    x = LifetimeTracked(0)
  }

  init?(n: Int, before: Bool) {
    if before {
      return nil
    }
    self.x = LifetimeTracked(0)
  }

  init?(n: Int, after: Bool) {
    self.x = LifetimeTracked(0)
    if after {
      return nil
    }
  }

  init?(n: Int, before: Bool, after: Bool) {
    if before {
      return nil
    }
    self.x = LifetimeTracked(0)
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
  let y: LifetimeTracked

  /* Designated */
  override init(n: Int) {
    self.y = LifetimeTracked(0)
    super.init(n: n)
  }

  override init?(n: Int, before: Bool) {
    if before {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: n)
  }

  init?(n: Int, during: Bool) {
    self.y = LifetimeTracked(0)
    super.init(n: n, before: during)
  }

  init?(n: Int, before: Bool, during: Bool) {
    self.y = LifetimeTracked(0)
    if before {
      return nil
    }
    super.init(n: n, before: during)
  }

  override init?(n: Int, after: Bool) {
    self.y = LifetimeTracked(0)
    super.init(n: n)
    if after {
      return nil
    }
  }

  init?(n: Int, during: Bool, after: Bool) {
    self.y = LifetimeTracked(0)
    super.init(n: n, before: during)
    if after {
      return nil
    }
  }

  override init?(n: Int, before: Bool, after: Bool) {
    if before {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: n)
    if after {
      return nil
    }
  }

  init?(n: Int, before: Bool, during: Bool, after: Bool) {
    if before {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: n, before: during)
    if after {
      return nil
    }
  }
}

class GuineaPig<T> : Bear {
  let y: LifetimeTracked
  let t: T

  init?(t: T, during: Bool) {
    self.y = LifetimeTracked(0)
    self.t = t
    super.init(n: 0, before: during)
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
  expectEqual(NSLifetimeTracked.count(), 0)
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
  expectEqual(NSLifetimeTracked.count(), 0)
}

FailableInitTestSuite.test("DesignatedInitFailure_DerivedGeneric") {
  mustFail { GuineaPig<LifetimeTracked>(t: LifetimeTracked(0), during: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
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

  expectEqual(NSLifetimeTracked.count(), 0)
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
  expectEqual(NSLifetimeTracked.count(), 0)
}

runAllTests()
