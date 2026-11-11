// RUN: %target-swift-frontend -emit-sil -enable-lifetime-resolution -verify -verify-additional-prefix lr- %s
// RUN: %target-swift-frontend -emit-sil -disable-lifetime-resolution -verify -verify-additional-prefix di- %s

// Use-before-init and let-reinit diagnostics for `self` in a root class initializer, checked
// against DefiniteInitialization. A `lr-` or `di-` prefix marks a diagnostic only one of them emits.

class Link {}

func use(_ x: AnyObject) {}
func use(_ x: Link) {}
func use(_ p: (Int, Link)) {}
func cond() -> Bool { true }

class PassSelf {
  var count: Int
  var l: Link
  init() {
    count = 0
    use(self) // expected-error {{variable 'self.l' used before being initialized}}
    l = Link()
  }
}

class PassSelfDirectly {
  var l: Link // expected-note {{'self.l' not initialized}}
  init() {
    takeSelf(self) // expected-error {{'self' used in method call 'takeSelf' before all stored properties are initialized}}
    l = Link()
  }
}
func takeSelf(_ x: PassSelfDirectly) {}

class MethodCall {
  var l: Link // expected-note {{'self.l' not initialized}}
  func m() {}
  init() {
    m() // expected-error {{'self' used in method call 'm' before all stored properties are initialized}}
    l = Link()
  }
}

class FinalMethodCall {
  var l: Link // expected-note {{'self.l' not initialized}}
  final func m() {}
  init() {
    m() // expected-error {{'self' used in method call 'm' before all stored properties are initialized}}
    l = Link()
  }
}

class ComputedProperty {
  var l: Link // expected-note {{'self.l' not initialized}}
  var c: Int { 1 }
  init() {
    _ = c // expected-error {{'self' used in property access 'c' before all stored properties are initialized}}
    l = Link()
  }
}

class CapturedSelf {
  var l: Link // expected-note {{'self.l' not initialized}}
  init() {
    let f = { use(self) } // expected-error {{'self' captured by a closure before all members were initialized}}
    f()
    l = Link()
  }
}

class ReadField {
  var l: Link
  init() {
    use(l) // expected-error {{variable 'self.l' used before being initialized}}
    l = Link()
  }
}

class ReadPartialTuple {
  var p: (Int, Link) // expected-di-note {{'self.p.1' not initialized}}
  init() {
    p.0 = 1
    use(p) // expected-error {{variable 'self.p.1' used before being initialized}}
  } // expected-di-error {{return from initializer without initializing all stored properties}}
}

class ReadAfterConditionalInit {
  var l: Link // expected-di-note {{'self.l' not initialized}}
  init() {
    if cond() { l = Link() }
    use(l) // expected-error {{variable 'self.l' used before being initialized}}
  } // expected-di-error {{return from initializer without initializing all stored properties}}
}

class ReturnWithoutInit {
  var count: Int
  var l: Link // expected-note {{'self.l' not initialized}}
  init() {
    count = 1
  } // expected-error {{return from initializer without initializing all stored properties}}
}

class ReturnAfterConditionalInit {
  var l: Link // expected-note {{'self.l' not initialized}}
  init() {
    if cond() { l = Link() }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

class ExplicitReturn {
  var l: Link // expected-note {{'self.l' not initialized}}
  init() {
    if cond() { return } // expected-lr-error {{return from initializer without initializing all stored properties}}
    l = Link()
  } // expected-di-error {{return from initializer without initializing all stored properties}}
}

class FailableExplicitReturn {
  var l: Link // expected-note {{'self.l' not initialized}}
  init?() {
    if cond() { return } // expected-error {{return from failable initializer before initializing all stored properties; did you mean to return 'nil'?}}
    l = Link()
  }
}

class TupleProperty {
  var p: (Int, Link) // expected-note {{'self.p.1' not initialized}}
  init() {
    p.0 = 1
  } // expected-error {{return from initializer without initializing all stored properties}}
}

class GenericProperty<T> {
  var t: T
  var u: T // expected-note {{'self.u' not initialized}}
  init(t: T) {
    self.t = t
  } // expected-error {{return from initializer without initializing all stored properties}}
}

class LetTwice {
  let l: Link // expected-note {{change 'let' to 'var' to make it mutable}}
  init() {
    l = Link()
    l = Link() // expected-error {{immutable value 'self.l' may only be initialized once}}
  }
}

class LetInitialValue {
  let n: Int = 1 // expected-note {{initial value already provided in 'let' declaration}}
                 // expected-note@-1 {{change 'let' to 'var' to make it mutable}}
  init() {
    n = 2 // expected-error {{immutable value 'self.n' may only be initialized once}}
  }
}

class LetMaybeTwice {
  let l: Link // expected-note {{change 'let' to 'var' to make it mutable}}
  init() {
    if cond() { l = Link() }
    l = Link() // expected-error {{immutable value 'self.l' may only be initialized once}}
  }
}

class LetInLoop {
  let n: Int // expected-note {{change 'let' to 'var' to make it mutable}}
             // expected-note@-1 {{'self.n' not initialized}}
  init() {
    for _ in 0..<2 {
      n = 1 // expected-error {{immutable value 'self.n' may only be initialized once}}
    }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// Fine: each `let` is initialized exactly once on every path.
class LetBothBranches {
  let l: Link
  init() {
    if cond() { l = Link() } else { l = Link() }
  }
}

// Fine: `self` is fully initialized when it is used.
class UseAfterInit {
  var l: Link
  let n: Int
  init() {
    l = Link()
    n = 1
    use(self)
    l = Link()
  }
}
