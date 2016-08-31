// RUN: %target-swift-frontend -emit-sil -disable-objc-attr-requires-foundation-module -verify %s

// High-level tests that DI rejects certain invalid idioms for early
// return from initializers.

// <rdar://problem/19267795> failable initializers that call noreturn function produces bogus diagnostics
class FailableInitThatFailsReallyHard {
  init?() {   // no diagnostics generated.
    fatalError("bad")
  }
}


class BaseClass {}
final class DerivedClass : BaseClass {
  init(x : ()) {
    fatalError("bad")  // no diagnostics.
  }
}

func something(_ x: Int) {}

func something(_ x: inout Int) {}

func something(_ x: AnyObject) {}

func something(_ x: Any.Type) {}

// <rdar://problem/22946400> DI needs to diagnose self usages in error block
//
// FIXME: crappy QoI
class ErrantBaseClass {
  init() throws {}
}

class ErrantClass : ErrantBaseClass {
  let x: Int
  var y: Int

  override init() throws {
    x = 10
    y = 10
    try super.init()
  }

  init(invalidEscapeDesignated: ()) {
    x = 10
    y = 10
    do {
      try super.init()
    } catch {}
  } // expected-error {{'self' used inside 'catch' block reachable from super.init call}}

  convenience init(invalidEscapeConvenience: ()) {
    do {
      try self.init()
    } catch {}
  } // expected-error {{'self' used inside 'catch' block reachable from self.init call}}

  init(noEscapeDesignated: ()) throws {
    x = 10
    y = 10
    do {
      try super.init()
    } catch let e {
      throw e  // ok
    }
  }

  convenience init(noEscapeConvenience: ()) throws {
    do {
      try self.init()
    } catch let e {
      throw e  // ok
    }
  }

  convenience init(invalidAccess: ()) throws {
    do {
      try self.init()
    } catch let e {
      something(x) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}
      something(self.x) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}

      something(y) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}
      something(self.y) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}

      something(&y) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}
      something(&self.y) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}

      something(self) // expected-error {{'self' used inside 'catch' block reachable from self.init call}}

      // FIXME: not diagnosed
      something(type(of: self))

      throw e
    }
  }
}
