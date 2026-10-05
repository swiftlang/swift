// RUN: %target-typecheck-verify-swift -enable-experimental-feature CalledAttribute

// REQUIRES: swift_feature_CalledAttribute

// contextual type
do {
  func fn() {}

  // To @called(atMostOnce)
  let _: @called(atMostOnce) () -> Void = fn // Ok
  // Closure assumes `@called(atMostOnce)`
  var atMostOnce: @called(atMostOnce) () -> Void = { } // Ok

  // From called(atMostOnce)
  var _: () -> Void = atMostOnce
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}

  let atMostOnceOpt: (@called(atMostOnce) () -> Void)? = fn // Ok
  let _: (() -> Void)? = atMostOnceOpt
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}
}

struct Storage {
  // expected-note@-1 {{consider adding '~Copyable' to struct 'Storage'}}

  let atMostOnce: @called(atMostOnce) () -> Void
  // expected-error@-1 {{stored property 'atMostOnce' of 'Copyable'-conforming struct 'Storage' has non-Copyable type '@called(atMostOnce) () -> Void'}}
}

struct NCStorage: ~Copyable {
  let atMostOnce: @called(atMostOnce) () -> Void

  init(atMostOnce: @escaping @called(atMostOnce) () -> Void) {
    self.atMostOnce = atMostOnce
  }
}

// Argument conversions
func argumentConversions(fn: @escaping () -> Void, atMostOnce: @called(atMostOnce) () -> Void) {
  func atMostOnceFn(_ f: @called(atMostOnce) () -> Void) {}
  func plainFn(_ f: () -> Void) {}

  atMostOnceFn(fn) // Ok
  atMostOnceFn(atMostOnce) // Ok

  plainFn(atMostOnce) // expected-error {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}
  atMostOnceFn({ }) // Ok
}

func contravariant() {
  func consumes(_: () -> Void) {}
  func consumesCalledAtMostOnce(_: @called(atMostOnce) () -> Void) {}

  var wrapperFn: (@escaping () -> Void) -> Void = consumes
  var wrapperAtMostOnce: (@escaping @called(atMostOnce) () -> Void) -> Void = consumesCalledAtMostOnce

  wrapperFn = wrapperAtMostOnce // Ok
  wrapperAtMostOnce = wrapperFn
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}
}

func impliedEscaping(atMostOnceFn: @escaping @called(atMostOnce) () -> Void) {
  func takesEscaping(_ f: @escaping () -> Void) {}
  takesEscaping(atMostOnceFn) // no diagnostics about `@escaping`
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}
}

protocol P {
  func run(_: @called(atMostOnce) () -> Void)
  // expected-note@-1 {{protocol requires function 'run' with type '(consuming @called(atMostOnce) () -> Void) -> ()'}}
}

struct S1: P { // expected-error {{type 'S1' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  func run(_: () -> Void) {} // expected-note {{candidate has non-matching type '(() -> Void) -> ()'}}
}

struct S2: P {
  func run(_: @called(atMostOnce) () -> Void) {} // Ok
}

protocol Q {
  func run(_: @escaping () -> Void)
}

struct S3: Q {
  func run(_: @called(atMostOnce) () -> Void) {} // Ok (because `@called(atMostOnce)` is more narrow then plain escaping type)
}

func testClosures() {
  let _: @called(atMostOnce) () -> Void = { } // Ok

  let fn = { @called(atMostOnce) in }
  let _: () -> Void = fn
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> ()' to function type '() -> Void'}}

  func atMostOnce(_: consuming (@called(atMostOnce) () -> Void)?) {}

  atMostOnce { } // Ok
  atMostOnce { @called(atMostOnce) in // Ok
  }

  func plain(_: () -> Void) {}
  func plainEscaping(_: @escaping () -> Void) {}

  plain { @called(atMostOnce) in
    // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> ()' to function type '() -> Void'}}
  }
  plainEscaping { @called(atMostOnce) in
    // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> ()' to function type '() -> Void'}}
  }

  func generic<T>(_: T) {} // expected-note 2 {{required by local function 'generic' where 'T' = '@called(atMostOnce) () -> ()'}}

  generic(fn)
  // expected-error@-1 {{type '@called(atMostOnce) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  generic { @called(atMostOnce) in }
  // expected-error@-1 {{type '@called(atMostOnce) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  func genericNC<T: ~Copyable>(_: consuming T) {}

  genericNC(fn) // Ok
  genericNC { @called(atMostOnce) in } // Ok
}

protocol PA {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  func f(_: A)
}

struct TestAssociatedTypeInference : PA {
  func f(_: @called(atMostOnce) (Int) -> Int) { } // Ok
}

struct TestExplicitAssociatedType : PA {
  typealias A = (Int) -> Int
  func f(_: @called(atMostOnce) (Int) -> Int) { } // Ok
}

struct TestWitnessAndAssociatedTypeMismatch : PA { // expected-error {{type 'TestWitnessAndAssociatedTypeMismatch' does not conform to protocol 'PA'}}
  // expected-note@-1 {{add stubs for conformance}}
  typealias A = @called(atMostOnce) (Int) -> Int
  // expected-note@-1 {{possibly intended match 'TestWitnessAndAssociatedTypeMismatch.A' (aka '@called(atMostOnce) (Int) -> Int') does not conform to 'Copyable'}}
  func f(_: (Int) -> Int) { }
}

protocol PR {
  func f(_: (Int) -> Int)
  func g(_: @called(atMostOnce) (Int) -> Int)
  // expected-note@-1 {{protocol requires function 'g' with type '(consuming @called(atMostOnce) (Int) -> Int) -> ()'}}
}

struct TestDifferentWitnesses : PR { // expected-error {{type 'TestDifferentWitnesses' does not conform to protocol 'PR'}}
// expected-note@-1 {{add stubs for conformance}}
  func f(_: (Int) -> Int) { }
  func g(_: (Int) -> Int) { }
  // expected-note@-1 {{candidate has non-matching type '((Int) -> Int) -> ()'}}
}

struct TestSameWitnesses : PR {
  func f(_: @called(atMostOnce) (Int) -> Int) { } // Ok
  func g(_: @called(atMostOnce) (Int) -> Int) { } // Ok
}

protocol PA_Contravariant {
  associatedtype A
  func f(_: (A) -> Void)
  // expected-note@-1 {{protocol requires function 'f' with type '((@escaping (Int) -> Int) -> Void) -> ()'}}
}

struct TestContravariantAssociatedTypeInference : PA_Contravariant { // expected-error {{type 'TestContravariantAssociatedTypeInference' does not conform to protocol 'PA_Contravariant'}}
  // expected-note@-1 {{add stubs for conformance}}
  func f(_: (@called(atMostOnce) (Int) -> Int) -> Void) { }
  // expected-note@-1 {{candidate has non-matching type '((consuming @called(atMostOnce) (Int) -> Int) -> Void) -> ()' [with A = (Int) -> Int]}}
}

protocol P_PlainResult {
  func f() -> () -> Void // expected-note {{protocol requires function 'f()' with type '() -> () -> Void'}}
}

protocol P_CalledAtMostOnceResult {
  func f() -> @called(atMostOnce) () -> Void
}

struct TestCalledAtMostOnceResultWitness : P_PlainResult { // expected-error {{type 'TestCalledAtMostOnceResultWitness' does not conform to protocol 'P_PlainResult'}}
  // expected-note@-1 {{add stubs for conformance}}
  func f() -> @called(atMostOnce) () -> Void { { } }
  // expected-note@-1 {{candidate has non-matching type '() -> @called(atMostOnce) () -> Void'}}
}

struct TestPlainResultWitness : P_CalledAtMostOnceResult {
  func f() -> () -> Void { { } } // Ok
}

func testSendingCaptures() {
  class NS {
    func test() {
      _ = { @called(atMostOnce) [sending self] in
        _ = self
      }
    }
  }

  let ns = NS()
  _ = { @called(atMostOnce) [sending ns] in
    ns
  }
  _ = { @called(atMostOnce) [x = 42, sending ns = NS()] in
    _ = x
    _ = ns
  }

  _ = { [sending ns] in ns }
  // expected-error@-1 {{'sending' capture may only be declared in a '@called(atMostOnce)' closure}}

  func calledAtMostOnce(_: @called(atMostOnce) () -> Void) {}
  func manyTimes(_: () -> Void) {}

  calledAtMostOnce { [sending x = NS()] in
    _ = x // Ok
  }

  manyTimes { [sending x = NS()] in
    // expected-error@-1 {{'sending' capture may only be declared in a '@called(atMostOnce)' closure}}
    _ = x
  }
}

// `@escaping @called(atMostOnce)` implies `@_implicitSelfCapture`
do {
  func takeFn(fn: @escaping @called(atMostOnce) () -> Int) { }

  class C {
    var property: Int = 0

    func method() { }

    func testMethod() {
      takeFn { // Ok
        method()
        return property
      }

      let _ = { @called(atMostOnce) in // Ok
        method()
        return property
      }
    }
  }
}

// MARK: - @called(exactlyOnce)

do {
  func fn() {}

  let _: @called(exactlyOnce) () -> Void = fn // Ok
  let exactlyOnce: @called(exactlyOnce) () -> Void = { } // Ok

  let _: () -> Void = exactlyOnce
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '() -> Void'}}
}

struct ExactlyOnceStorage {
  // expected-note@-1 {{consider adding '~Copyable' to struct 'ExactlyOnceStorage'}}

  let exactlyOnce: @called(exactlyOnce) () -> Void
  // expected-error@-1 {{stored property 'exactlyOnce' of 'Copyable'-conforming struct 'ExactlyOnceStorage' has non-Copyable type '@called(exactlyOnce) () -> Void'}}
}

func exactlyOnceArgumentConversions(fn: @escaping () -> Void, exactlyOnce: @called(exactlyOnce) () -> Void) {
  func exactlyOnceFn(_ f: @called(exactlyOnce) () -> Void) {}
  func plainFn(_ f: () -> Void) {}

  exactlyOnceFn(fn) // Ok
  exactlyOnceFn({ }) // Ok

  plainFn(exactlyOnce) // expected-error {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '() -> Void'}}
}

func exactlyOnceOwnership(
  _: borrowing @called(exactlyOnce) () -> Void,
  // expected-error@-1 {{'@called(exactlyOnce)' cannot be used together with 'borrowing'}}
  _: consuming @called(exactlyOnce) () -> Void, // Ok
  _: inout @called(exactlyOnce) () -> Void // Ok
) {}

func exactlyOnceTupleOwnership(
  _: (borrowing @called(exactlyOnce) () -> Void) -> Void
  // expected-error@-1 {{'@called(exactlyOnce)' cannot be used together with 'borrowing'}}
) {}

// `@called(exactlyOnce)` parameters are `consuming` by default.
protocol P_ExactlyOnce {
  func run(_: @called(exactlyOnce) () -> Void)
  // expected-note@-1 {{protocol requires function 'run' with type '(consuming @called(exactlyOnce) () -> Void) -> ()'}}
  func runNested(_: (@called(exactlyOnce) () -> Void) -> Void)
}

struct TestExactlyOnceWitnesses: P_ExactlyOnce { // expected-error {{type 'TestExactlyOnceWitnesses' does not conform to protocol 'P_ExactlyOnce'}}
  // expected-note@-1 {{add stubs for conformance}}
  func run(_: () -> Void) {}
  // expected-note@-1 {{candidate has non-matching type '(() -> Void) -> ()'}}
  func runNested(_: (consuming @called(exactlyOnce) () -> Void) -> Void) {} // Ok
}

func testExactlyOnceClosures() {
  let _: @called(exactlyOnce) () -> Void = { } // Ok

  let fn = { @called(exactlyOnce) in }
  let _: () -> Void = fn
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> ()' to function type '() -> Void'}}

  func exactlyOnce(_: @called(exactlyOnce) () -> Void) {}

  exactlyOnce { } // Ok
  exactlyOnce { @called(exactlyOnce) in } // Ok

  func plain(_: () -> Void) {}

  plain { @called(exactlyOnce) in
    // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> ()' to function type '() -> Void'}}
  }

  func generic<T>(_: T) {} // expected-note {{required by local function 'generic' where 'T' = '@called(exactlyOnce) () -> ()'}}

  generic(fn)
  // expected-error@-1 {{type '@called(exactlyOnce) () -> ()' cannot conform to 'Copyable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
}
