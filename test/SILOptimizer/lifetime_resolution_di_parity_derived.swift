// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -verify -verify-additional-prefix lr- %s
// RUN: %target-swift-frontend -emit-sil -disable-lifetime-resolution -verify -verify-additional-prefix di- %s

// Use-before-init, `super.init` and let-reinit diagnostics for `self` in a derived class initializer, checked
// against DefiniteInitialization. A `lr-` or `di-` prefix marks a diagnostic only one of them emits:
//   - A return that skips `super.init` on some paths is reported at the `return`, rather than the end of
//     the initializer.
//   - A use of `self` after a failed `super.init` is reported once, at the use, rather than also at the
//     end of the initializer.

struct E: Error {}
func use(_ x: AnyObject) {}
func useInt(_ x: Int) {}

class B {
  var v: Int = 0
  final var f: Int = 0
  init() {}
  init(t: Bool) throws {}
  init?(n: Int) {}
  func m() {}
}

class NotInitAtSuper: B {
  var x: Int
  var y: Int
  override init() {
    super.init() // expected-error {{property 'self.x' not initialized at super.init call}}
  }
}

class NotInitAtImplicitSuper: A {
  var x: Int
  var y: Int
  init(a: Int) { // expected-error {{property 'self.x' not initialized at implicitly generated super.init call}}
  }
}

class A {
  init() {}
}

class NoSuperInit: B {
  var x: Int
  init(a: Int) {
    x = a
  } // expected-error {{'super.init' isn't called on all paths before returning from initializer}}
}

class MethodBeforeSuper: B {
  var x: Int
  override init() {
    x = 0
    m() // expected-error {{'self' used in method call 'm' before 'super.init' call}}
    super.init()
  }
}

class OwnMethodBeforeSuper: B {
  var x: Int
  func own() {}
  override init() {
    x = 0
    own() // expected-error {{'self' used in method call 'own' before 'super.init' call}}
    super.init()
  }
}

class PropertyBeforeSuper: B {
  var x: Int
  override init() {
    x = 0
    v = 1 // expected-error {{'self' used in property access 'v' before 'super.init' call}}
    super.init()
  }
}

class FinalPropertyBeforeSuper: B {
  var x: Int
  override init() {
    x = 0
    f = 1 // expected-error {{'self' used in property access 'f' before 'super.init' call}}
    super.init()
  }
}

class PassSelfBeforeSuper: B {
  var x: Int
  override init() {
    x = 0
    use(self) // expected-error {{'self' used before 'super.init' call}}
    super.init()
  }
}

// FIXME: LR should emit a better diagnostic here, specifying that it's a capture before 'super.init'.
class CaptureBeforeSuper: B {
  var x: Int
  override init() {
    x = 0
    let f = { _ = self } // expected-error {{'self' captured by a closure before all members were initialized}}
    f()
    super.init()
  }
}

class MethodBeforeFieldsAndSuper: B {
  var x: Int
  override init() {
    m() // expected-error {{'self' used in method call 'm' before 'super.init' call}}
    x = 0
    super.init()
  }
}

class OwnFieldReadBeforeInit: B {
  var x: Int
  override init() {
    useInt(x) // expected-error {{variable 'self.x' used before being initialized}}
    x = 0
    super.init()
  }
}

class OwnFieldReadBeforeSuper: B {
  var x: Int
  override init() {
    x = 0
    useInt(x)
    super.init()
  }
}

class SuperTwice: B {
  var x: Int
  override init() {
    x = 0
    super.init()
    super.init() // expected-error {{'super.init' called multiple times in initializer}}
  }
}

class ReturnWithoutSuper: B {
  var x: Int
  override init() {
    x = 0
    return // expected-error {{'super.init' isn't called on all paths before returning from initializer}}
  }
}

// FIXME: Since LR actually points to the early return path where the call was missing, we could rephrase this diagnostic
class ReturnWithoutSuperSomePaths: B {
  var x: Int
  init(b: Bool) {
    x = 0
    if b { return } // expected-lr-error {{'super.init' isn't called on all paths before returning from initializer}}
    super.init()
  } // expected-di-error {{'super.init' isn't called on all paths before returning from initializer}}
}

class CatchAfterSuper: B {
  var x: Int
  init(t: Int) {
    x = 0
    do {
      try super.init(t: true)
    } catch {
      use(self) // expected-error {{'self' used inside 'catch' block reachable from super.init call}}
    }
  } // expected-di-error {{'self' used inside 'catch' block reachable from super.init call}}
}

class CatchAfterSuperEmpty: B {
  init(t: Int) {
    do {
      try super.init(t: true)
    } catch {
      use(self) // expected-error {{'self' used inside 'catch' block reachable from super.init call}}
    }
  } // expected-di-error {{'self' used inside 'catch' block reachable from super.init call}}
}

class LetReinitDerived: B {
  let y: Int // expected-note {{change 'let' to 'var' to make it mutable}}
  override init() {
    y = 0
    y = 1 // expected-error {{immutable value 'self.y' may only be initialized once}}
    super.init()
  }
}

class EmptyDerived: B {
  override init() {
    m() // expected-error {{'self' used in method call 'm' before 'super.init' call}}
    super.init()
  }
}

class EmptyDerivedReturnWithoutSuper: B {
  override init() {
    return // expected-error {{'super.init' isn't called on all paths before returning from initializer}}
  }
}

class G<T> {
  var t: T
  init(_ t: T) { self.t = t }
}

class DerivedFromGeneric: G<Int> {
  var u: Int
  init() {
    super.init(0) // expected-error {{property 'self.u' not initialized at super.init call}}
  }
  init(b: Bool) {
    u = 0
    t = 1 // expected-error {{'self' used in property access 't' before 'super.init' call}}
    super.init(0)
  }
}

class ThrowingSuper: B {
  var x: Int
  init(q: Int) throws {
    try super.init(t: true) // expected-error {{property 'self.x' not initialized at super.init call}}
  }
}

class FailableSuper: B {
  var x: Int
  init?(q: Int) {
    super.init(n: q) // expected-error {{property 'self.x' not initialized at super.init call}}
  }
}

class Dynamic {
  dynamic init() {}
}

class DerivedFromDynamic: Dynamic {
  var x: Int
  override init() {
    super.init() // expected-error {{property 'self.x' not initialized at super.init call}}
  }
}

// No errors.

class FineAfterSuper: B {
  var x: Int
  let y: Int
  override init() {
    x = 0
    y = 1
    super.init()
    m()
    v = 2
    f = 3
    use(self)
    x = 4
  }
}

class ImplicitSuper: A {
  var x: Int
  init(a: Int) {
    x = a
  }
}

class FineEmpty: B {
  override init() {
    super.init()
    m()
  }
}

class FineThrowingAndFailable: B {
  var x: Int
  init(q: Int) throws {
    x = q
    try super.init(t: true)
    use(self)
  }
  init?(q: Bool) {
    x = 0
    super.init(n: 1)
    use(self)
  }
}
