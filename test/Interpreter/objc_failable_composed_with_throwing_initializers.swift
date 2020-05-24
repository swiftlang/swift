// RUN: %empty-directory(%t)
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// RUN: %empty-directory(%t)
//
// target-build-swift assumes we want -swift-version 4. Behavior in initializers
// changed in swift 5, so we want to explicitly check it as well.
//
// RUN: %target-clang -fobjc-arc %S/Inputs/ObjCClasses/ObjCClasses.m -c -o %t/ObjCClasses.o
// RUN: %target-build-swift -I %S/Inputs/ObjCClasses/ -Xlinker %t/ObjCClasses.o %s -o %t/a.out -swift-version 5
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: objc_interop

// These tests are failable tests that compose with throwing initializers. They
// all catch the throw on failure and return None.

import Foundation
import ObjCClasses
import StdlibUnittest

var FailableComposedWithThrowingInitTestSuite = TestSuite("FailableInitObjCComposedWithThrowingInits")
defer { runAllTests() }

enum E : Error {
  case X
}

func maybeThrow(_ shouldThrow: Bool, _ result: Int? = nil) throws -> Int {
  if shouldThrow {
    throw E.X
  }
  return result ?? 0
}

func mustFail<T>(f: () -> T?) {
  if f() != nil {
    preconditionFailure("Didn't fail")
  }
}

class Bear : NSLifetimeTracked {
  let x: LifetimeTracked

  /* Designated */
  init(n: Int) {
    x = LifetimeTracked(0)
  }

  init?(n: Int, before: Bool) {
    let v: Int
    do {
      v = try maybeThrow(before)
    } catch {
      return nil
    }
    self.x = LifetimeTracked(v)
  }

  init?(n: Int, after: Bool) {
    self.x = LifetimeTracked(0)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  init?(n: Int, before: Bool, after: Bool) {
    let v: Int
    do {
      v = try maybeThrow(before)
    } catch {
      return nil
    }
    self.x = LifetimeTracked(v)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  /* Convenience */
  convenience init?(before: Bool) {
    let v: Int
    do {
      v = try maybeThrow(before)
    } catch {
      return nil
    }
    self.init(n: v)
  }

  convenience init?(during: Bool) {
    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  convenience init?(before: Bool, during: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }
    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  convenience init?(after: Bool) {
    self.init(n: 0)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  convenience init?(before: Bool, after: Bool) {
    let value: Int
    do {
      value = try maybeThrow(before)
    } catch {
      return nil
    }
    self.init(n: value)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  convenience init?(during: Bool, after: Bool) {
    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  convenience init?(before: Bool, during: Bool, after: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }

    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }

    do {
      let _ = try maybeThrow(after)
    } catch {
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
    let value: Int
    do {
      value = try maybeThrow(before)
    } catch {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: value)
  }

  init?(n: Int, during: Bool) {
    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  init?(n: Int, before: Bool, during: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }

    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  override init?(n: Int, after: Bool) {
    self.y = LifetimeTracked(0)
    super.init(n: n)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  init?(n: Int, during: Bool, after: Bool) {
    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }

    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  override init?(n: Int, before: Bool, after: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: n)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  init?(n: Int, before: Bool, during: Bool, after: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }
    
    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }

    do {
      let _ = try maybeThrow(after)
    } catch {
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

FailableComposedWithThrowingInitTestSuite.test("FailableInitFailure_Root") {
  mustFail { Bear(n: 0, before: true) }
  mustFail { Bear(n: 0, after: true) }
  mustFail { Bear(n: 0, before: true, after: false) }
  mustFail { Bear(n: 0, before: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

FailableComposedWithThrowingInitTestSuite.test("FailableInitFailure_Derived") {
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

FailableComposedWithThrowingInitTestSuite.test("DesignatedInitFailure_DerivedGeneric") {
  mustFail { GuineaPig<LifetimeTracked>(t: LifetimeTracked(0), during: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

FailableComposedWithThrowingInitTestSuite.test("ConvenienceInitFailure_Root") {
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

FailableComposedWithThrowingInitTestSuite.test("ConvenienceInitFailure_Derived") {
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

// @objc

class AtObjCBear : NSLifetimeTracked {
  let x: LifetimeTracked

  /* Designated */
  @objc init(n: Int) {
    x = LifetimeTracked(0)
  }

  @objc init?(n: Int, before: Bool) {
    let v: Int
    do {
      v = try maybeThrow(before)
    } catch {
      return nil
    }
    self.x = LifetimeTracked(v)
  }

  @objc init?(n: Int, after: Bool) {
    self.x = LifetimeTracked(0)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc init?(n: Int, before: Bool, after: Bool) {
    let v: Int
    do {
      v = try maybeThrow(before)
    } catch {
      return nil
    }
    self.x = LifetimeTracked(v)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  /* Convenience */
  @objc convenience init?(before: Bool) {
    let v: Int
    do {
      v = try maybeThrow(before)
    } catch {
      return nil
    }
    self.init(n: v)
  }

  @objc convenience init?(during: Bool) {
    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  @objc convenience init?(before: Bool, during: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }
    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  @objc convenience init?(after: Bool) {
    self.init(n: 0)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc convenience init?(before: Bool, after: Bool) {
    let value: Int
    do {
      value = try maybeThrow(before)
    } catch {
      return nil
    }
    self.init(n: value)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc convenience init?(during: Bool, after: Bool) {
    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc convenience init?(before: Bool, during: Bool, after: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }

    do {
      try self.init(n: maybeThrow(during))
    } catch {
      return nil
    }

    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  /* Exotic */
  @objc convenience init!(IUO: Bool) {
    self.init(before: IUO)
  }

  @objc convenience init(force: Bool) {
    self.init(before: force)!
  }
}

class AtObjCPolarBear : AtObjCBear {
  let y: LifetimeTracked

  /* Designated */
  @objc override init(n: Int) {
    self.y = LifetimeTracked(0)
    super.init(n: n)
  }

  @objc override init?(n: Int, before: Bool) {
    let value: Int
    do {
      value = try maybeThrow(before)
    } catch {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: value)
  }

  @objc init?(n: Int, during: Bool) {
    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  @objc init?(n: Int, before: Bool, during: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }

    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }
  }

  @objc override init?(n: Int, after: Bool) {
    self.y = LifetimeTracked(0)
    super.init(n: n)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc init?(n: Int, during: Bool, after: Bool) {
    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }

    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc override init?(n: Int, before: Bool, after: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }
    self.y = LifetimeTracked(0)
    super.init(n: n)
    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }

  @objc init?(n: Int, before: Bool, during: Bool, after: Bool) {
    do {
      let _ = try maybeThrow(before)
    } catch {
      return nil
    }
    
    self.y = LifetimeTracked(0)
    do {
      try super.init(n: maybeThrow(during))
    } catch {
      return nil
    }

    do {
      let _ = try maybeThrow(after)
    } catch {
      return nil
    }
  }
}

FailableComposedWithThrowingInitTestSuite.test("FailableInitFailure_Root") {
  mustFail { AtObjCBear(n: 0, before: true) }
  mustFail { AtObjCBear(n: 0, after: true) }
  mustFail { AtObjCBear(n: 0, before: true, after: false) }
  mustFail { AtObjCBear(n: 0, before: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

FailableComposedWithThrowingInitTestSuite.test("FailableInitFailure_Derived") {
  mustFail { AtObjCPolarBear(n: 0, before: true) }
  mustFail { AtObjCPolarBear(n: 0, during: true) }
  mustFail { AtObjCPolarBear(n: 0, before: true, during: false) }
  mustFail { AtObjCPolarBear(n: 0, before: false, during: true) }
  mustFail { AtObjCPolarBear(n: 0, after: true) }
  mustFail { AtObjCPolarBear(n: 0, during: true, after: false) }
  mustFail { AtObjCPolarBear(n: 0, during: false, after: true) }
  mustFail { AtObjCPolarBear(n: 0, before: true, after: false) }
  mustFail { AtObjCPolarBear(n: 0, before: false, after: true) }
  mustFail { AtObjCPolarBear(n: 0, before: true, during: false, after: false) }
  mustFail { AtObjCPolarBear(n: 0, before: false, during: true, after: false) }
  mustFail { AtObjCPolarBear(n: 0, before: false, during: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}

FailableComposedWithThrowingInitTestSuite.test("ConvenienceInitFailure_Root") {
  mustFail { AtObjCBear(before: true) }
  mustFail { AtObjCBear(during: true) }
  mustFail { AtObjCBear(before: true, during: false) }
  mustFail { AtObjCBear(before: false, during: true) }
  mustFail { AtObjCBear(after: true) }
  mustFail { AtObjCBear(before: true, after: false) }
  mustFail { AtObjCBear(before: false, after: true) }
  mustFail { AtObjCBear(during: true, after: false) }
  mustFail { AtObjCBear(during: false, after: true) }
  mustFail { AtObjCBear(before: true, during: false, after: false) }
  mustFail { AtObjCBear(before: false, during: true, after: false) }
  mustFail { AtObjCBear(before: false, during: false, after: true) }

  _ = AtObjCBear(IUO: false)
  _ = AtObjCBear(force: false)

  expectEqual(NSLifetimeTracked.count(), 0)
}

FailableComposedWithThrowingInitTestSuite.test("ConvenienceInitFailure_Derived") {
  mustFail { AtObjCPolarBear(before: true) }
  mustFail { AtObjCPolarBear(during: true) }
  mustFail { AtObjCPolarBear(before: true, during: false) }
  mustFail { AtObjCPolarBear(before: false, during: true) }
  mustFail { AtObjCPolarBear(after: true) }
  mustFail { AtObjCPolarBear(before: true, after: false) }
  mustFail { AtObjCPolarBear(before: false, after: true) }
  mustFail { AtObjCPolarBear(during: true, after: false) }
  mustFail { AtObjCPolarBear(during: false, after: true) }
  mustFail { AtObjCPolarBear(before: true, during: false, after: false) }
  mustFail { AtObjCPolarBear(before: false, during: true, after: false) }
  mustFail { AtObjCPolarBear(before: false, during: false, after: true) }
  expectEqual(NSLifetimeTracked.count(), 0)
}
