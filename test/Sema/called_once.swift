// RUN: %target-typecheck-verify-swift -enable-experimental-feature CalledAttribute

// REQUIRES: swift_feature_CalledAttribute

// contextual type
do {
  func fn() {}

  // To @called(once)
  let _: @called(once) () -> Void = fn // Ok
  // Closure assumes `@called(once)`
  var once: @called(once) () -> Void = { } // Ok

  // From called(once)
  var _: () -> Void = once
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}

  let onceOpt: (@called(once) () -> Void)? = fn // Ok
  let _: (() -> Void)? = onceOpt
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
}

// Argument conversions
func argumentConversions(fn: @escaping () -> Void, once: @called(once) () -> Void) {
  func onceFn(_ f: @called(once) () -> Void) {}
  func plainFn(_ f: () -> Void) {}

  onceFn(fn) // Ok
  onceFn(once) // Ok

  plainFn(once) // expected-error {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
  onceFn({ }) // Ok
}

func contravariant() {
  func consumes(_: () -> Void) {}
  func consumesCalledOnce(_: @called(once) () -> Void) {}

  var wrapperFn: (@escaping () -> Void) -> Void = consumes
  var wrapperOnce: (@escaping @called(once) () -> Void) -> Void = consumesCalledOnce

  wrapperFn = wrapperOnce // Ok
  wrapperOnce = wrapperFn
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
}

func impliedEscaping(onceFn: @escaping @called(once) () -> Void) {
  func takesEscaping(_ f: @escaping () -> Void) {}
  takesEscaping(onceFn) // no diagnostics about `@escaping`
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> Void' to function type '() -> Void'}}
}

protocol P {
  func run(_: @called(once) () -> Void)
  // expected-note@-1 {{protocol requires function 'run' with type '(consuming @called(once) () -> Void) -> ()'}}
}

struct S1: P { // expected-error {{type 'S1' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  func run(_: () -> Void) {} // expected-note {{candidate has non-matching type '(() -> Void) -> ()'}}
}

struct S2: P {
  func run(_: @called(once) () -> Void) {} // Ok
}

protocol Q {
  func run(_: @escaping () -> Void)
}

struct S3: Q {
  func run(_: @called(once) () -> Void) {} // Ok (because `@called(once)` is more narrow then plain escaping type.
}

func testClosures() {
  let _: @called(once) () -> Void = { } // Ok

  let fn = { @called(once) in }
  let _: () -> Void = fn
  // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> ()' to function type '() -> Void'}}

  func once(_: consuming (@called(once) () -> Void)?) {}

  once { } // Ok
  once { @called(once) in // Ok
  }

  func plain(_: () -> Void) {}
  func plainEscaping(_: @escaping () -> Void) {}

  plain { @called(once) in
    // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> ()' to function type '() -> Void'}}
  }
  plainEscaping { @called(once) in
    // expected-error@-1 {{invalid conversion from '@called(once)' function of type '@called(once) () -> ()' to function type '() -> Void'}}
  }

  func generic<T>(_: T) {} // expected-note 2 {{required by local function 'generic' where 'T' = '@called(once) () -> ()'}}

  generic(fn)
  // expected-error@-1 {{type '@called(once) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  generic { @called(once) in }
  // expected-error@-1 {{type '@called(once) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  func genericNC<T: ~Copyable>(_: consuming T) {}

  genericNC(fn) // Ok
  genericNC { @called(once) in } // Ok
}

protocol PA {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  func f(_: A)
}

struct TestAssociatedTypeInference : PA {
  func f(_: @called(once) (Int) -> Int) { } // Ok
}

struct TestExplicitAssociatedType : PA {
  typealias A = (Int) -> Int
  func f(_: @called(once) (Int) -> Int) { } // Ok
}

struct TestWitnessAndAssociatedTypeMismatch : PA { // expected-error {{type 'TestWitnessAndAssociatedTypeMismatch' does not conform to protocol 'PA'}}
  // expected-note@-1 {{add stubs for conformance}}
  typealias A = @called(once) (Int) -> Int
  // expected-note@-1 {{possibly intended match 'TestWitnessAndAssociatedTypeMismatch.A' (aka '@called(once) (Int) -> Int') does not conform to 'Copyable'}}
  func f(_: (Int) -> Int) { }
}

protocol PR {
  func f(_: (Int) -> Int)
  func g(_: @called(once) (Int) -> Int)
  // expected-note@-1 {{protocol requires function 'g' with type '(consuming @called(once) (Int) -> Int) -> ()'}}
}

struct TestDifferentWitnesses : PR { // expected-error {{type 'TestDifferentWitnesses' does not conform to protocol 'PR'}}
// expected-note@-1 {{add stubs for conformance}}
  func f(_: (Int) -> Int) { }
  func g(_: (Int) -> Int) { }
  // expected-note@-1 {{candidate has non-matching type '((Int) -> Int) -> ()'}}
}

struct TestSameWitnesses : PR {
  func f(_: @called(once) (Int) -> Int) { } // Ok
  func g(_: @called(once) (Int) -> Int) { } // Ok
}

protocol PA_Contravariant {
  associatedtype A
  func f(_: (A) -> Void)
  // expected-note@-1 {{protocol requires function 'f' with type '((@escaping (Int) -> Int) -> Void) -> ()'}}
}

struct TestContravariantAssociatedTypeInference : PA_Contravariant { // expected-error {{type 'TestContravariantAssociatedTypeInference' does not conform to protocol 'PA_Contravariant'}}
  // expected-note@-1 {{add stubs for conformance}}
  func f(_: (@called(once) (Int) -> Int) -> Void) { }
  // expected-note@-1 {{candidate has non-matching type '((consuming @called(once) (Int) -> Int) -> Void) -> ()' [with A = (Int) -> Int]}}
}

protocol P_PlainResult {
  func f() -> () -> Void // expected-note {{protocol requires function 'f()' with type '() -> () -> Void'}}
}

protocol P_CalledOnceResult {
  func f() -> @called(once) () -> Void
}

struct TestCalledOnceResultWitness : P_PlainResult { // expected-error {{type 'TestCalledOnceResultWitness' does not conform to protocol 'P_PlainResult'}}
  // expected-note@-1 {{add stubs for conformance}}
  func f() -> @called(once) () -> Void { { } }
  // expected-note@-1 {{candidate has non-matching type '() -> @called(once) () -> Void'}}
}

struct TestPlainResultWitness : P_CalledOnceResult {
  func f() -> () -> Void { { } } // Ok
}

func testSendingCaptures() {
  class NS {
    func test() {
      _ = { @called(once) [sending self] in
        _ = self
      }
    }
  }

  let ns = NS()
  _ = { @called(once) [sending ns] in
    ns
  }
  _ = { @called(once) [x = 42, sending ns = NS()] in
    _ = x
    _ = ns
  }

  _ = { [sending ns] in ns }
  // expected-error@-1 {{'sending' capture may only be declared in a '@called(once)' closure}}

  func calledOnce(_: @called(once) () -> Void) {}
  func manyTimes(_: () -> Void) {}

  calledOnce { [sending x = NS()] in
    _ = x // Ok
  }

  manyTimes { [sending x = NS()] in
    // expected-error@-1 {{'sending' capture may only be declared in a '@called(once)' closure}}
    _ = x
  }
}

// `@escaping @called(once)` implies `@_implicitSelfCapture`
do {
  func takeFn(fn: @escaping @called(once) () -> Int) { }

  class C {
    var property: Int = 0

    func method() { }

    func testMethod() {
      takeFn { // Ok
        method()
        return property
      }

      let _ = { @called(once) in // Ok
        method()
        return property
      }
    }
  }
}
