// RUN: %target-swift-frontend -emit-silgen-ossa -enable-lifetime-resolution -verify %s

// Use-before-init diagnostics from LifetimeResolution, which match what
// DefiniteInitialization emits for the same code.

func use<T>(_ t: T) {}
func mayThrow() throws {}
func mutate(_ x: inout Int) {}

// Tuple element initialized in an `if` with no `else`.
func tupleElementInIf(_ b: Bool) {
  var t: (Int, Int) // expected-note {{variable defined here}}
  t.0 = 1
  if b { t.1 = 2 }
  use(t) // expected-error {{variable 't.1' used before being initialized}}
}

func nestedTupleElement() {
  var m: (a: Int, b: (Int, Int)) // expected-note {{variable defined here}}
  m.a = 1
  m.b.0 = 2
  use(m) // expected-error {{variable 'm.b.1' used before being initialized}}
}

func labeledTupleElement() {
  var m: (a: Int, b: Int) // expected-note {{variable defined here}}
  m.b = 1
  use(m) // expected-error {{variable 'm.a' used before being initialized}}
}

// Field initialized in an `if` with no `else`.
struct IfNoElse {
  var a: Int
  var b: Int // expected-note {{'self.b' not initialized}}
  init(_ c: Bool) {
    a = 0
    if c { b = 1 }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// Field initialized only inside a loop body that may run zero times.
struct ForLoopBody {
  var a: Int
  var b: Int // expected-note {{'self.b' not initialized}}
  init(_ n: Int) {
    a = 0
    for i in 0..<n { b = i }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// An early `return` in a failable init skips a later initialization.
struct EarlyReturn {
  var a: Int
  var b: Int // expected-note {{'self.b' not initialized}}
  init?(_ c: Bool) {
    a = 0
    if !c { return nil }
    if c { return } // expected-error {{return from failable initializer before initializing all stored properties; did you mean to return 'nil'?}} {{12-18=return nil}}
    b = 1
  }
}

// An early `return` in a non-failable init is reported at the `return`.
struct EarlyReturnNonFailable {
  var a: Int
  var b: Int // expected-note {{'self.b' not initialized}}
  init(_ c: Bool) {
    a = 0
    if c { return } // expected-error {{return from initializer without initializing all stored properties}}
    b = 1
  }
}

// One `switch` case leaves the field untouched.
struct SwitchDefault {
  var a: Int
  var b: Int // expected-note {{'self.b' not initialized}}
  init(_ k: Int) {
    a = 0
    switch k {
    case 0: b = 0
    case 1: b = 1
    default: break
    }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// The `catch` path falls through without initializing the field.
struct EmptyCatch {
  var a: Int
  var b: Int // expected-note {{'self.b' not initialized}}
  init() {
    a = 0
    do { try mayThrow(); b = 1 } catch { }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// Each branch initializes a different field.
struct SplitBranches {
  var a: Int // expected-note {{'self.a' not initialized}}
  var b: Int // expected-note {{'self.b' not initialized}}
  init(_ c: Bool) {
    if c { a = 1 } else { b = 2 }
  } // expected-error {{return from initializer without initializing all stored properties}}
}

// A partially initialized tuple-typed stored property names the missing element.
struct TupleProperty {
  var p: (Int, Int) // expected-note {{'self.p.1' not initialized}}
  var q: Int
  init() {
    p.0 = 1
    q = 2
  } // expected-error {{return from initializer without initializing all stored properties}}
}

struct SelfUsedEarly {
  var a: Int
  var b: Int // expected-note 2 {{'self.b' not initialized}}
  init(method: ()) {
    a = 1
    foo() // expected-error {{'self' used before all stored properties are initialized}}
    b = 2
  }
  init(escape: ()) {
    a = 1
    use(self) // expected-error {{'self' used before all stored properties are initialized}}
    b = 2
  }
  init(delegating: Bool) {
    if delegating { self.init(method: ()) }
  } // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
  func foo() {}
}

enum EmptyInit {
  case x
  init() {} // expected-error {{'self.init' isn't called on all paths before returning from initializer}}
}

// `let` initialized on only one path out of a loop via `break`.
func letInitializedBeforeBreak(_ n: Int) {
  let x: Int // expected-note {{constant defined here}}
  for i in 0..<n {
    if i == 3 { x = i; break }
  }
  use(x) // expected-error {{constant 'x' used before being initialized}}
}

// `let` assigned in a loop body: both a missing and a repeated initialization.
func letAssignedInLoop(_ n: Int) {
  let x: Int // expected-note {{constant defined here}} expected-note {{change 'let' to 'var' to make it mutable}} {{3-6=var}}
  for i in 0..<n { x = i } // expected-error {{immutable value 'x' may only be initialized once}}
  use(x) // expected-error {{constant 'x' used before being initialized}}
}

func letAssignedTwice() {
  let x: Int // expected-note {{change 'let' to 'var' to make it mutable}}
  x = 1
  x = 2 // expected-error {{immutable value 'x' may only be initialized once}}
  use(x)
}

struct LetProperties {
  let x: Int // expected-note {{change 'let' to 'var' to make it mutable}}
  let y: Int = 0 // expected-note {{initial value already provided in 'let' declaration}} expected-note {{change 'let' to 'var' to make it mutable}}
  init(_ b: Bool) {
    x = 1
    if b { x = 2 } // expected-error {{immutable value 'self.x' may only be initialized once}}
    y = 3 // expected-error {{immutable value 'self.y' may only be initialized once}}
  }

  // Assigning all of `self` is permitted even after initializing a `let` property.
  init(whole: ()) {
    x = 1
    self = LetProperties(true)
  }
}

// Initializing the only stored property is not an assignment of all of `self`.
struct OnlyLetProperty {
  let x: Int // expected-note {{change 'let' to 'var' to make it mutable}}
  init() {
    x = 1
    x = 2 // expected-error {{immutable value 'self.x' may only be initialized once}}
  }
  init(whole: OnlyLetProperty) {
    x = 1
    self = whole
  }
}

// Redeclaring `x` on the next iteration, after a `continue`, does not use it.
func uninitializedAfterContinue(_ n: Int, _ a: Bool, _ b: Bool) {
  for i in 0..<n {
    var x: Int // expected-note {{variable defined here}}
    if i == 0 { continue }
    if i > 1 { x = 1 }
    if a { if b { use(1) } else { use(2) } } else { use(3) }
    if a { if b { use(1) } else { use(2) } } else { use(3) }
    use(x) // expected-error {{variable 'x' used before being initialized}}
  }
}

// The use is in a `defer` that runs on a path where `x` is uninitialized.
func useInDefer(_ c: Bool) {
  let x: Int // expected-note {{constant defined here}}
  defer { use(x) } // expected-error {{constant 'x' used before being initialized}}
  if c { x = 1 }
}

func capturedByClosure() {
  var x: Int // expected-note {{variable defined here}}
  let g = { use(x) } // expected-error {{variable 'x' captured by a closure before being initialized}}
  x = 1
  g()
}

func capturedByNestedFunction() {
  var x: Int // expected-note {{variable defined here}}
  func f() { use(x) }
  f() // expected-error {{variable 'x' used by function definition before being initialized}}
  x = 1
}

func passedInout() {
  var x: Int // expected-note {{variable defined here}}
  mutate(&x) // expected-error {{variable 'x' passed by reference before being initialized}}
}

// Unlike DI, which names the first element, an entirely uninitialized variable is named as a whole.
func entirelyUninitialized() {
  let m: (Int, Int) // expected-note {{constant defined here}}
  use(m) // expected-error {{constant 'm' used before being initialized}}
}
