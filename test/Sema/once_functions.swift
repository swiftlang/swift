// RUN: %target-typecheck-verify-swift -enable-experimental-feature OnceFunctions
// RUN: %target-typecheck-verify-swift -enable-experimental-feature OnceFunctions -solver-enable-promote-supertypes

// REQUIRES: swift_feature_OnceFunctions

// contextual type
do {
  func fn() {}

  // To at-most-once
  let _: @callable(once) () -> Void = fn // Ok
  // Closure assumes at-most-once
  var atMostOnce: @callable(once) () -> Void = { } // Ok

  // From called(atMostOnce)
  var _: () -> Void = atMostOnce
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}

  let atMostOnceOpt: (@callable(once) () -> Void)? = fn // Ok
  let _: (() -> Void)? = atMostOnceOpt
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}
}

struct Storage {
  // expected-note@-1 {{consider adding '~Copyable' to struct 'Storage'}}

  let atMostOnce: @callable(once) () -> Void
  // expected-error@-1 {{stored property 'atMostOnce' of 'Copyable'-conforming struct 'Storage' has non-'Copyable' type '@callable(once) () -> Void'}}
}

struct NCStorage: ~Copyable {
  let atMostOnce: @callable(once) () -> Void

  init(atMostOnce: @escaping @callable(once) () -> Void) {
    self.atMostOnce = atMostOnce
  }
}

// Argument conversions
func argumentConversions(fn: @escaping () -> Void, atMostOnce: @callable(once) () -> Void) {
  func atMostOnceFn(_ f: @callable(once) () -> Void) {}
  func plainFn(_ f: () -> Void) {}

  atMostOnceFn(fn) // Ok
  atMostOnceFn(atMostOnce) // Ok

  plainFn(atMostOnce) // expected-error {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}
  atMostOnceFn({ }) // Ok
}

func contravariant() {
  func consumes(_: () -> Void) {}
  func consumesCalledAtMostOnce(_: @callable(once) () -> Void) {}

  var wrapperFn: (@escaping () -> Void) -> Void = consumes
  var wrapperAtMostOnce: (@escaping @callable(once) () -> Void) -> Void = consumesCalledAtMostOnce

  wrapperFn = wrapperAtMostOnce // Ok
  wrapperAtMostOnce = wrapperFn
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}
}

// An `inout` argument must have exactly the parameter's type, so here only the
// destination has execution semantics.
func inoutArgument() {
  func mutates(_: inout @callable(once) () -> Void) {}
  func mutatesOnce(_: inout @once () -> Void) {}

  var plain: () -> Void = {}
  mutates(&plain)
  // expected-error@-1 {{invalid conversion from function of type '() -> Void' to '@callable(once)' function of type '@callable(once) () -> Void'}}
  mutatesOnce(&plain)
  // expected-error@-1 {{invalid conversion from function of type '() -> Void' to '@once' function of type '@once () -> Void'}}
}

func impliedEscaping(atMostOnceFn: @escaping @callable(once) () -> Void) {
  func takesEscaping(_ f: @escaping () -> Void) {}
  takesEscaping(atMostOnceFn) // no diagnostics about `@escaping`
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}
}

protocol P {
  func run(_: @callable(once) () -> Void)
  // expected-note@-1 {{protocol requires function 'run' with type '(@callable(once) () -> Void) -> ()'}}
}

struct S1: P { // expected-error {{type 'S1' does not conform to protocol 'P'}} expected-note {{add stubs for conformance}}
  func run(_: () -> Void) {} // expected-note {{candidate has non-matching type '(() -> Void) -> ()'}}
}

struct S2: P {
  func run(_: @callable(once) () -> Void) {} // Ok
}

protocol Q {
  func run(_: @escaping () -> Void)
}

struct S3: Q {
  func run(_: @callable(once) () -> Void) {} // Ok (because `@callable(once)` is more narrow then plain escaping type)
}

func testClosures() {
  let _: @callable(once) () -> Void = { } // Ok

  let fn = { @callable(once) in }
  let _: () -> Void = fn
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> ()' to function type '() -> Void'}}

  func atMostOnce(_: consuming (@callable(once) () -> Void)?) {}

  atMostOnce { } // Ok
  atMostOnce { @callable(once) in // Ok
  }

  func plain(_: () -> Void) {}
  func plainEscaping(_: @escaping () -> Void) {}

  plain { @callable(once) in
    // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> ()' to function type '() -> Void'}}
  }
  plainEscaping { @callable(once) in
    // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> ()' to function type '() -> Void'}}
  }

  func generic<T>(_: T) {} // expected-note 2 {{required by local function 'generic' where 'T' = '@callable(once) () -> ()'}}

  generic(fn)
  // expected-error@-1 {{type '@callable(once) () -> ()' cannot conform to 'Copyable'}}

  generic { @callable(once) in }
  // expected-error@-1 {{type '@callable(once) () -> ()' cannot conform to 'Copyable'}}

  func genericNC<T: ~Copyable>(_: consuming T) {}

  genericNC(fn) // Ok
  genericNC { @callable(once) in } // Ok
}

protocol PA {
  associatedtype A // expected-note {{protocol requires nested type 'A'}}
  func f(_: A)
}

struct TestAssociatedTypeInference : PA {
  func f(_: @callable(once) (Int) -> Int) { } // Ok
}

struct TestExplicitAssociatedType : PA {
  typealias A = (Int) -> Int
  func f(_: @callable(once) (Int) -> Int) { } // Ok
}

struct TestWitnessAndAssociatedTypeMismatch : PA { // expected-error {{type 'TestWitnessAndAssociatedTypeMismatch' does not conform to protocol 'PA'}}
  // expected-note@-1 {{add stubs for conformance}}
  typealias A = @callable(once) (Int) -> Int
  // expected-note@-1 {{possibly intended match 'TestWitnessAndAssociatedTypeMismatch.A' (aka '@callable(once) (Int) -> Int') does not conform to 'Copyable'}}
  func f(_: (Int) -> Int) { }
}

protocol PR {
  func f(_: (Int) -> Int)
  func g(_: @callable(once) (Int) -> Int)
  // expected-note@-1 {{protocol requires function 'g' with type '(@callable(once) (Int) -> Int) -> ()'}}
}

struct TestDifferentWitnesses : PR { // expected-error {{type 'TestDifferentWitnesses' does not conform to protocol 'PR'}}
// expected-note@-1 {{add stubs for conformance}}
  func f(_: (Int) -> Int) { }
  func g(_: (Int) -> Int) { }
  // expected-note@-1 {{candidate has non-matching type '((Int) -> Int) -> ()'}}
}

struct TestSameWitnesses : PR {
  func f(_: @callable(once) (Int) -> Int) { } // Ok
  func g(_: @callable(once) (Int) -> Int) { } // Ok
}

protocol PA_Contravariant {
  associatedtype A
  func f(_: (A) -> Void)
  // expected-note@-1 {{protocol requires function 'f' with type '((@escaping (Int) -> Int) -> Void) -> ()'}}
}

struct TestContravariantAssociatedTypeInference : PA_Contravariant { // expected-error {{type 'TestContravariantAssociatedTypeInference' does not conform to protocol 'PA_Contravariant'}}
  // expected-note@-1 {{add stubs for conformance}}
  func f(_: (@callable(once) (Int) -> Int) -> Void) { }
  // expected-note@-1 {{candidate has non-matching type '((@callable(once) (Int) -> Int) -> Void) -> ()' [with A = (Int) -> Int]}}
}

protocol P_PlainResult {
  func f() -> () -> Void // expected-note {{protocol requires function 'f()' with type '() -> () -> Void'}}
}

protocol P_CalledAtMostOnceResult {
  func f() -> @callable(once) () -> Void
}

struct TestCalledAtMostOnceResultWitness : P_PlainResult { // expected-error {{type 'TestCalledAtMostOnceResultWitness' does not conform to protocol 'P_PlainResult'}}
  // expected-note@-1 {{add stubs for conformance}}
  func f() -> @callable(once) () -> Void { { } }
  // expected-note@-1 {{candidate has non-matching type '() -> @callable(once) () -> Void'}}
}

struct TestPlainResultWitness : P_CalledAtMostOnceResult {
  func f() -> () -> Void { { } } // Ok
}

func testSendingCaptures() {
  class NS {
    func test() {
      _ = { @callable(once) [sending self] in
        _ = self
      }
    }
  }

  let ns = NS()
  _ = { @callable(once) [sending ns] in
    ns
  }
  _ = { @callable(once) [x = 42, sending ns = NS()] in
    _ = x
    _ = ns
  }

  _ = { [sending ns] in ns }
  // expected-error@-1 {{'sending' capture may only be declared in a '@once' or '@callable(once)' closure}}

  func calledAtMostOnce(_: @callable(once) () -> Void) {}
  func manyTimes(_: () -> Void) {}

  calledAtMostOnce { [sending x = NS()] in
    _ = x // Ok
  }

  manyTimes { [sending x = NS()] in
    // expected-error@-1 {{'sending' capture may only be declared in a '@once' or '@callable(once)' closure}}
    _ = x
  }

  func calledExactlyOnce(_: @once () -> Void) {}

  _ = { @once [sending ns] in
    ns
  }

  calledExactlyOnce { [sending x = NS()] in
    _ = x // Ok
  }
}

// Escaping at-most-once implies `@_implicitSelfCapture`
do {
  func takeFn(fn: @escaping @callable(once) () -> Int) { }

  class C {
    var property: Int = 0

    func method() { }

    func testMethod() {
      takeFn { // Ok
        method()
        return property
      }

      let _ = { @callable(once) in // Ok
        method()
        return property
      }
    }
  }
}

// MARK: - exactly-once

do {
  func fn() {}

  let _: @once () -> Void = fn // Ok
  let exactlyOnce: @once () -> Void = { } // Ok

  let _: () -> Void = exactlyOnce
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '() -> Void'}}
}

struct ExactlyOnceStorage {
  // expected-note@-1 {{consider adding '~Copyable' to struct 'ExactlyOnceStorage'}}

  let exactlyOnce: @once () -> Void
  // expected-error@-1 {{stored property 'exactlyOnce' of 'Copyable'-conforming struct 'ExactlyOnceStorage' has non-'Copyable' type '@once () -> Void'}}
  // expected-error@-2 {{stored property 'exactlyOnce' of 'Deinitable'-conforming struct 'ExactlyOnceStorage' has non-'Deinitable' type '@once () -> Void'}}
}

func exactlyOnceArgumentConversions(fn: @escaping () -> Void, exactlyOnce: @once () -> Void) {
  func exactlyOnceFn(_ f: @once () -> Void) {}
  func plainFn(_ f: () -> Void) {}

  exactlyOnceFn(fn) // Ok
  exactlyOnceFn({ }) // Ok

  plainFn(exactlyOnce) // expected-error {{invalid conversion from '@once' function of type '@once () -> Void' to function type '() -> Void'}}
}

func exactlyOnceOwnership(
  _: borrowing @once () -> Void,
  // expected-error@-1 {{'@once' cannot be used together with 'borrowing'}}
  _: consuming @once () -> Void, // Ok
  _: inout @once () -> Void // Ok
) {}

func exactlyOnceTupleOwnership(
  _: (borrowing @once () -> Void) -> Void
  // expected-error@-1 {{'@once' cannot be used together with 'borrowing'}}
) {}

// exactly-once parameters are `consuming` by default.
protocol P_ExactlyOnce {
  func run(_: @once () -> Void)
  // expected-note@-1 {{protocol requires function 'run' with type '(@once () -> Void) -> ()'}}
  func runNested(_: (@once () -> Void) -> Void)
}

struct TestExactlyOnceWitnesses: P_ExactlyOnce { // expected-error {{type 'TestExactlyOnceWitnesses' does not conform to protocol 'P_ExactlyOnce'}}
  // expected-note@-1 {{add stubs for conformance}}
  func run(_: () -> Void) {}
  // expected-note@-1 {{candidate has non-matching type '(() -> Void) -> ()'}}
  func runNested(_: (consuming @once () -> Void) -> Void) {} // Ok
}

func testExactlyOnceClosures() {
  let _: @once () -> Void = { } // Ok

  let fn = { @once in }
  let _: () -> Void = fn
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> ()' to function type '() -> Void'}}

  func exactlyOnce(_: @once () -> Void) {}

  exactlyOnce { } // Ok
  exactlyOnce { @once in } // Ok

  func plain(_: () -> Void) {}

  plain { @once in
    // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> ()' to function type '() -> Void'}}
  }

  func generic<T>(_: T) {} // expected-note {{required by local function 'generic' where 'T' = '@once () -> ()'}}

  generic(fn)
  // expected-error@-1 {{type '@once () -> ()' cannot conform to 'Copyable'}}
}

// MARK: - Conversions

func conversionMatrix(
  plain: @escaping () -> Void,
  atMostOnce: @escaping @callable(once) () -> Void,
  exactlyOnce: @escaping @once () -> Void
) {
  func takesPlain(_: @escaping () -> Void) {}
  func takesAtMostOnce(_: @escaping @callable(once) () -> Void) {}
  func takesExactlyOnce(_: @escaping @once () -> Void) {}

  takesPlain(plain) // Ok
  takesAtMostOnce(plain) // Ok
  takesExactlyOnce(plain) // Ok

  takesPlain(atMostOnce)
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}
  takesAtMostOnce(atMostOnce) // Ok
  takesExactlyOnce(atMostOnce) // Ok

  takesPlain(exactlyOnce)
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '() -> Void'}}
  takesAtMostOnce(exactlyOnce)
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '@callable(once) () -> Void'}}
  takesExactlyOnce(exactlyOnce) // Ok

  let _: @once () -> Void = plain // Ok
  let _: @once () -> Void = atMostOnce // Ok
  let _: @callable(once) () -> Void = exactlyOnce
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '@callable(once) () -> Void'}}
}

func conversionVariance() {
  func takesAtMostOnce(_: @callable(once) () -> Void) {}
  func takesExactlyOnce(_: @once () -> Void) {}

  // Parameters are contravariant.
  let _: (@escaping @callable(once) () -> Void) -> Void = takesExactlyOnce // Ok
  let _: (@escaping @once () -> Void) -> Void = takesAtMostOnce
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '@callable(once) () -> Void'}}

  func makePlain() -> () -> Void { {} }
  func makeAtMostOnce() -> @callable(once) () -> Void { {} }
  func makeExactlyOnce() -> @once () -> Void { {} }

  // Results are covariant.
  let _: () -> @once () -> Void = makePlain // Ok
  let _: () -> @once () -> Void = makeAtMostOnce // Ok
  let _: () -> @callable(once) () -> Void = makeExactlyOnce
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '@callable(once) () -> Void'}}
  let _: () -> () -> Void = makeExactlyOnce
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '() -> Void'}}
}

func conversionClosures() {
  func takesExactlyOnce(_: @once () -> Void) {}
  func takesAtMostOnce(_: @callable(once) () -> Void) {}

  takesExactlyOnce { @callable(once) in } // Ok
  takesAtMostOnce { @once in }
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> ()' to function type '@callable(once) () -> Void'}}
}

func conversionJoin(
  _ flag: Bool,
  plain: @escaping () -> Void,
  atMostOnce: @escaping @callable(once) () -> Void,
  exactlyOnce: @escaping @once () -> Void
) {
  // The join of two function types has the more restrictive semantics.
  let a = flag ? plain : exactlyOnce
  let _: () -> Void = a
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '() -> Void'}}
  let b = flag ? atMostOnce : exactlyOnce
  let _: @callable(once) () -> Void = b
  // expected-error@-1 {{invalid conversion from '@once' function of type '@once () -> Void' to function type '@callable(once) () -> Void'}}
  let c = flag ? plain : atMostOnce
  let _: () -> Void = c
  // expected-error@-1 {{invalid conversion from '@callable(once)' function of type '@callable(once) () -> Void' to function type '() -> Void'}}
}

// MARK: - Witness matching

// Parameters
protocol Completable {
  func onCompletion(_ body: @once () -> Void)
  // expected-note@-1 2 {{protocol requires function 'onCompletion' with type '(@once () -> Void) -> ()'}}
}

struct Request: Completable {
  func onCompletion(_ body: @once () -> Void) {} // Ok
}

struct Broadcast: Completable { // expected-error {{type 'Broadcast' does not conform to protocol 'Completable'}}
  // expected-note@-1 {{add stubs for conformance}}
  func onCompletion(_ body: () -> Void) {}
  // expected-note@-1 {{candidate has non-matching type '(() -> Void) -> ()'}}
}

struct Retry: Completable { // expected-error {{type 'Retry' does not conform to protocol 'Completable'}}
  // expected-note@-1 {{add stubs for conformance}}
  func onCompletion(_ body: @callable(once) () -> Void) {}
  // expected-note@-1 {{candidate has non-matching type '(@callable(once) () -> Void) -> ()'}}
}

protocol CompletableAtMostOnce {
  func onCompletion(_ body: @callable(once) () -> Void)
}

struct ExactlyOnceRequest: CompletableAtMostOnce {
  func onCompletion(_ body: @once () -> Void) {} // Ok
}

protocol CompletablePlain {
  func onCompletion(_ body: () -> Void)
}

struct ExactlyOncePlainRequest: CompletablePlain {
  func onCompletion(_ body: @once () -> Void) {} // Ok
}

// Results
protocol SingleResponse {
  func callback() -> @once () -> Void
}

struct Success: SingleResponse {
  func callback() -> () -> Void { {} } // Ok
}

struct AtMostOnceSuccess: SingleResponse {
  func callback() -> @callable(once) () -> Void { {} } // Ok
}

struct ExactlyOnceSuccess: SingleResponse {
  func callback() -> @once () -> Void { {} } // Ok
}

protocol AtMostOnceResponse {
  func callback() -> @callable(once) () -> Void
  // expected-note@-1 {{protocol requires function 'callback()' with type '() -> @callable(once) () -> Void'}}
}

struct ExactlyOnceFailure: AtMostOnceResponse { // expected-error {{type 'ExactlyOnceFailure' does not conform to protocol 'AtMostOnceResponse'}}
  // expected-note@-1 {{add stubs for conformance}}
  func callback() -> @once () -> Void { {} }
  // expected-note@-1 {{candidate has non-matching type '() -> @once () -> Void'}}
}

protocol Response {
  func callback() -> () -> Void
  // expected-note@-1 {{protocol requires function 'callback()' with type '() -> () -> Void'}}
}

struct Failure: Response { // expected-error {{type 'Failure' does not conform to protocol 'Response'}}
  // expected-note@-1 {{add stubs for conformance}}
  func callback() -> @once () -> Void { {} }
  // expected-note@-1 {{candidate has non-matching type '() -> @once () -> Void'}}
}

// Overrides must match the execution semantics of the overridden method
// exactly.
class OverrideBase {
  func plainParam(_: @escaping () -> Void) {}
  // expected-note@-1 {{potential overridden instance method 'plainParam' here}}
  func atMostOnceParam(_: @escaping @callable(once) () -> Void) {}
  // expected-note@-1 {{potential overridden instance method 'atMostOnceParam' here}}
  func exactlyOnceResult() -> @once () -> Void { {} }
  // expected-note@-1 {{potential overridden instance method 'exactlyOnceResult()' here}}
}

class OverrideDerivedBad: OverrideBase {
  override func plainParam(_: @escaping @once () -> Void) {}
  // expected-error@-1 {{method does not override any method from its superclass}}
  override func atMostOnceParam(_: @escaping @once () -> Void) {}
  // expected-error@-1 {{method does not override any method from its superclass}}
  override func exactlyOnceResult() -> () -> Void { {} }
  // expected-error@-1 {{method does not override any method from its superclass}}
}

class OverrideDerivedGood: OverrideBase {
  override func plainParam(_: @escaping () -> Void) {}
  override func atMostOnceParam(_: @escaping @callable(once) () -> Void) {}
  override func exactlyOnceResult() -> @once () -> Void { {} }
}

// MARK: - Overload resolution

func overloadRanking(
  plain: @escaping () -> Void,
  atMostOnce: @escaping @callable(once) () -> Void,
  exactlyOnce: @escaping @once () -> Void
) {
  func overloaded(_: @escaping @callable(once) () -> Void) -> Int { 0 }
  func overloaded(_: @escaping @once () -> Void) -> String { "" }

  // The closest execution semantics win.
  let _: Int = overloaded(plain)
  let _: Int = overloaded(atMostOnce)
  let _: String = overloaded(exactlyOnce)

  func plainOverloaded(_: @escaping () -> Void) -> Int { 0 }
  func plainOverloaded(_: @escaping @once () -> Void) -> String { "" }

  let _: Int = plainOverloaded(plain)
  let _: String = plainOverloaded(atMostOnce)
  let _: String = plainOverloaded(exactlyOnce)

  // A closure literal adopts either contextual type, so the less restrictive
  // overload wins because it's more specialized.
  let _: Int = overloaded { }
  let _: Int = plainOverloaded { }
  let _: Int = overloaded { @callable(once) in }
  let _: String = overloaded { @once in }
}

// MARK: - Existential conversions

protocol ExistentialProto {}
extension Int: ExistentialProto {}

func existentialConversions(
  atMostOnce: @escaping @callable(once) () -> Void,
  makeInt: @escaping () -> Int
) {
  // Calling a function doesn't help if the function value itself lacks an
  // invertible protocol that the existential requires.
  let _: any ~Escapable = atMostOnce
  // expected-error@-1 {{value of type '@callable(once) () -> Void' does not conform to specified type 'Copyable'}}

  let _: any ExistentialProto = makeInt
  // expected-error@-1 {{function produces expected type 'Int'; did you mean to call it with '()'?}}
}

// MARK: - Generic arguments

// An exactly-once function type isn't `Deinitable`, so it can't be a
// generic argument unless the parameter suppresses `Deinitable`.

struct NCBox<T: ~Copyable>: ~Copyable {
  // expected-note@-1 {{required by generic struct 'NCBox' where 'T' = '@once () -> Void'}}
  var value: T
}

@available(SwiftStdlib 5.9, *)
struct PackHolder<each T> {}

@available(SwiftStdlib 5.9, *)
func genericArgumentPack(
  _: PackHolder<Int, @once () -> Void>
  // expected-error@-1 {{type '@once () -> Void' does not conform to protocol 'Copyable'}}
) {}

func genericArgumentSpelled(
  _: Optional<@once () -> Void>,
  // expected-error@-1 {{type '@once () -> Void' does not conform to protocol 'Deinitable'}}
  _: (@once () -> Void)?,
  // expected-error@-1 {{type '@once () -> Void' does not conform to protocol 'Deinitable'}}
  _: [@once () -> Void],
  // expected-error@-1 {{type '@once () -> Void' does not conform to protocol 'Copyable'}}
  _: [Int: @once () -> Void],
  // expected-error@-1 {{type '@once () -> Void' does not conform to protocol 'Copyable'}}
  _: consuming NCBox<@once () -> Void>,
  // expected-error@-1 {{type '@once () -> Void' does not conform to protocol 'Deinitable'}}
  _: consuming (@callable(once) () -> Void)?, // Ok
  _: consuming NCBox<@callable(once) () -> Void> // Ok
) {}

func genericArgumentInferred(
  exactlyOnce: @escaping @once () -> Void,
  atMostOnce: @escaping @callable(once) () -> Void
) {
  func genericNC<T: ~Copyable>(_: consuming T) {}
  // expected-note@-1 2{{required by local function 'genericNC' where 'T' = '@once () -> Void'}}
  // expected-note@-2 {{required by local function 'genericNC' where 'T' = '@once () -> ()'}}

  genericNC(exactlyOnce)
  // expected-error@-1 {{type '@once () -> Void' cannot conform to 'Deinitable'}}
  genericNC { @once in }
  // expected-error@-1 {{type '@once () -> ()' cannot conform to 'Deinitable'}}
  genericNC(atMostOnce) // Ok

  _ = NCBox(value: exactlyOnce)
  // expected-error@-1 {{type '@once () -> Void' cannot conform to 'Deinitable'}}

  // FIXME: [deinitable] Optional's payload must be Deinitable.
  let _: _? = exactlyOnce
  let _: _? = atMostOnce // Ok

  let _ = Optional.some(exactlyOnce)
  // expected-error@-1 {{type '@once () -> Void' cannot conform to 'Deinitable'}}
  // expected-note@-2 {{required by generic enum 'Optional' where 'Wrapped' = '@once () -> Void'}}

  let _: any ~Copyable = exactlyOnce
  // expected-error@-1 {{value of type '@once () -> Void' does not conform to specified type 'Deinitable'}}
  let _: any ~Copyable = atMostOnce // Ok

  _ = { (f: @escaping @once () -> Void) in
    genericNC(f)
    // expected-error@-1 {{type '@once () -> Void' cannot conform to 'Deinitable'}}
    return 0
  }
}

struct ConditionallyCopyableBox<T: ~Copyable>: ~Copyable {
  init(_: consuming T) {}
  consuming func finish() {}
}

extension ConditionallyCopyableBox: Copyable where T: Copyable {}

func partiallyAppliedWithTypeDependence() {
  struct NC: ~Copyable {}

  func generic<T>(_: T) {}
  // expected-note@-1 {{required by local function 'generic' where 'T' = '@callable(once) () -> ()'}}
  func genericNC<T: ~Copyable>(_: consuming T) {}

  generic(ConditionallyCopyableBox(42).finish) // Ok
  generic(ConditionallyCopyableBox(NC()).finish)
  // expected-error@-1 {{type '@callable(once) () -> ()' cannot conform to 'Copyable'}}
  genericNC(ConditionallyCopyableBox(NC()).finish) // Ok
}

// MARK: - Captures

// A closure owns its capture list entries and the captures that it consumes,
// and only an exactly-once closure isn't `Deinitable`. So it's the
// only kind of closure that can own an exactly-once value. Consuming
// a capture in a closure that might run more than once is diagnosed after
// type checking; see test/SIL/once_attr.swift.
func captureRules(
  _ a: @escaping @once () -> Void,
  _ b: @escaping @once () -> Void,
  _ c: @escaping @once () -> Void,
  _ d: @escaping @once () -> Void,
  _ e: @escaping @once () -> Void,
  _ f: @escaping @once () -> Void,
  _ g: @escaping @once () -> Void,
  _ h: @escaping @once () -> Void
) {
  func takesExactlyOnce(_: @once () -> Void) {}
  func takesAtMostOnce(_: @callable(once) () -> Void) {}

  let _ = { @once in a() } // Ok
  takesExactlyOnce { b() } // Ok

  takesAtMostOnce { c() }
  // expected-error@-1 {{consumed capture 'c' of 'Deinitable'-conforming closure has non-'Deinitable' type '@once () -> Void'}}

  // Nested closures
  let _ = { @once in
    let inner = { @once in d() } // Ok
    inner()
  }
  let _ = { @callable(once) in
    let inner = { @once in e() }
    // expected-error@-1 {{consumed capture 'e' of 'Deinitable'-conforming closure has non-'Deinitable' type '@once () -> Void'}}
    inner()
  }

  // Capture lists
  let _ = { [f] in f() }
  // expected-error@-1 {{capture list entry 'f' of 'Deinitable'-conforming closure has non-'Deinitable' type '@once () -> Void'}}
  let _ = { @callable(once) [g] in g() }
  // expected-error@-1 {{capture list entry 'g' of 'Deinitable'-conforming closure has non-'Deinitable' type '@once () -> Void'}}
  let _ = { @once [h] in h() } // Ok
}

// An at-most-once value is `Deinitable`, so any closure can own it.
func captureRulesAtMostOnce(
  _ a: @escaping @callable(once) () -> Void,
  _ b: @escaping @callable(once) () -> Void,
  _ c: @escaping @callable(once) () -> Void,
  _ d: @escaping @callable(once) () -> Void
) {
  let _ = { @callable(once) in a() } // Ok
  let _ = { @once in b() } // Ok
  let _ = { @callable(once) [c] in c() } // Ok
  let _ = { [d] in _ = d } // Ok
}

// `UnsafeMutablePointer.pointee` has addressors but no getter.
func capturePointer(_ pointer: UnsafeMutablePointer<Int>) {
  let _ = { @once in _ = pointer.pointee } // Ok
  let _ = { @callable(once) in pointer.pointee = 1 } // Ok
}

// MARK: - Storage

// Storage that can drop a value without calling it requires `Deinitable`.

struct StoredPropertyStruct: ~Copyable {
  let a: @once () -> Void
  // expected-error@-1 {{stored property 'a' of 'Deinitable'-conforming struct 'StoredPropertyStruct' has non-'Deinitable' type '@once () -> Void'}}
  var b: @once () -> Void // Only the first is diagnosed.
  static let c: @once () -> Void = { }
  // expected-error@-1 {{static property 'c' cannot have non-'Deinitable' type '@once () -> Void'}}
  let atMostOnce: @callable(once) () -> Void // Ok

  // Computed properties and subscripts behave like functions.
  var computed: @once () -> Void { { } } // Ok
  var getSet: @once () -> Void { // Ok
    get { { } }
    set { newValue() }
  }
  subscript(i: Int) -> @once () -> Void { { } } // Ok
}

class StoredPropertyClass {
  var a: @once () -> Void
  // expected-error@-1 {{stored property 'a' of 'Deinitable'-conforming class 'StoredPropertyClass' has non-'Deinitable' type '@once () -> Void'}}
  lazy var b: @once () -> Void = { }
  // expected-error@-1 {{lazy property 'b' cannot have non-'Deinitable' type '@once () -> Void'}}

  init(a: @escaping @once () -> Void) {
    self.a = a
  }
}

actor StoredPropertyActor {
  let a: @once () -> Void
  // expected-error@-1 {{stored property 'a' of 'Deinitable'-conforming actor 'StoredPropertyActor' has non-'Deinitable' type '@once () -> Void'}}

  init(a: @escaping @once () -> Void) {
    self.a = a
  }
}

let globalExactlyOnce: @once () -> Void = { }
// expected-error@-1 {{global variable 'globalExactlyOnce' cannot have non-'Deinitable' type '@once () -> Void'}}

enum Payloads: ~Copyable {
  case a(@once () -> Void)
  // expected-error@-1 {{associated value 'a' of 'Deinitable'-conforming enum 'Payloads' has non-'Deinitable' type '@once () -> Void'}}
  case b(Int, label: @once () -> Void)
}

enum IndirectPayloads: ~Copyable {
  indirect case a(@once () -> Void)
  // expected-error@-1 {{associated value 'a' of 'Deinitable'-conforming enum 'IndirectPayloads' has non-'Deinitable' type '@once () -> Void'}}
  // expected-error@-2 {{non-'Copyable' enum 'IndirectPayloads' cannot be marked indirect or have indirect cases yet}}
}

func tupleParameter(_: (@once () -> Void, Int)) {}
// expected-error@-1 {{tuple with non-'Copyable' element type '@once () -> Void' is not supported}}
// expected-error@-2 {{parameter of non-'Copyable' type '(@once () -> Void, Int)' must specify ownership}}
// expected-note@-3 {{add 'borrowing' for an immutable reference}}
// expected-note@-4 {{add 'inout' for a mutable reference}}
// expected-note@-5 {{add 'consuming' to take the value from the caller}}

func tupleResult() -> (Int, @once () -> Void) { fatalError() }
// expected-error@-1 {{tuple with non-'Copyable' element type '@once () -> Void' is not supported}}

func tupleStorage(_ f: @escaping @once () -> Void) {
  let _: (x: @once () -> Void, y: Int)
  // expected-error@-1 {{tuple with non-'Copyable' element type '@once () -> Void' is not supported}}
  let _: ((Int, @once () -> Void), Int)
  // expected-error@-1 {{tuple with non-'Copyable' element type '@once () -> Void' is not supported}}
  let _ = (f, 0)
  // expected-error@-1 {{tuple with non-'Copyable' element type '@once () -> Void' is not supported}}
  let _: (@once () -> Void) = { } // Ok, not a tuple
}

func makeExactlyOnce() -> @once () -> Void { { } }

func asyncLetStorage() async {
  async let f: @once () -> Void = makeExactlyOnce()
  // expected-error@-1 {{'async let' binding 'f' cannot have non-'Deinitable' type '@once () -> Void'}}
  await f()
}

// Local variables and `inout` parameters keep the obligation with their scope
// or with the caller.
func allowedStorage(_ f: inout @once () -> Void) {
  let local: @once () -> Void = { }
  var localVar: @once () -> Void = { }
  localVar = { }
  _ = local
  _ = localVar
}

// MARK: - Property wrappers

@propertyWrapper
struct NCWrapper<T: ~Copyable>: ~Copyable {
// expected-note@-1 {{required by generic struct 'NCWrapper' where 'T' = '@once () -> Void'}}
  var wrappedValue: T
}

struct WrappedStorage: ~Copyable {
  @NCWrapper var wrapped: @once () -> Void
  // expected-error@-1 {{type '@once () -> Void' cannot conform to 'Deinitable'}}
}

// MARK: - Conventions

func conventions(
  _: @convention(c) @callable(once) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@callable(once)' types}}
  _: @convention(thin) @callable(once) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@callable(once)' types}}
  _: @convention(block) @callable(once) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@callable(once)' types}}
  _: @convention(c) @once () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@once' types}}
  _: @convention(thin) @once () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@once' types}}
  _: @convention(block) @once () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@once' types}}
  _: @convention(swift) @callable(once) () -> Void, // Ok
  _: @convention(swift) @once () -> Void // Ok
) {}

// MARK: - copy

func copyOperator(
  _ exactlyOnce: @once () -> Void,
  _ atMostOnce: @callable(once) () -> Void
) {
  _ = copy exactlyOnce // expected-error {{'copy' cannot be applied to non-'Copyable' types}}
  _ = copy atMostOnce // expected-error {{'copy' cannot be applied to non-'Copyable' types}}
}

// MARK: - Casts

// Casting a value of a non-'Copyable' type requires NoncopyableCasting (see
// test/SIL/callable_once_attr_consuming_captures.swift).

func casts(
  _ exactlyOnce: @once () -> Void,
  _ atMostOnce: @callable(once) () -> Void
) {
  _ = exactlyOnce is Int
  // expected-warning@-1 {{cast from '@once () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
  _ = atMostOnce is Int
  // expected-warning@-1 {{cast from '@callable(once) () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
  _ = exactlyOnce as? Int
  // expected-warning@-1 {{cast from '@once () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
  _ = atMostOnce as? Int
  // expected-warning@-1 {{cast from '@callable(once) () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
  _ = exactlyOnce as! Int
  // expected-warning@-1 {{cast from '@once () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
  _ = atMostOnce as! Int
  // expected-warning@-1 {{cast from '@callable(once) () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
}

func andAtMostOnce(
  _ lhs: Bool, _ rhs: @autoclosure @callable(once) () -> Bool
) -> Bool {
  lhs ? rhs() : false
}

// An `is` check of a function consumes it, so the autoclosure does too.
func isCheckInAtMostOnceAutoclosure(
  _ exactlyOnce: @once () -> Void,
  _ atMostOnce: @callable(once) () -> Void
) {
  _ = andAtMostOnce(true, exactlyOnce is Int)
  // expected-error@-1 {{consumed capture 'exactlyOnce' of 'Deinitable'-conforming closure has non-'Deinitable' type '@once () -> Void'}}
  // expected-warning@-2 {{cast from '@once () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-3 {{non-'Copyable' types cannot be conditionally cast}}
  _ = andAtMostOnce(true, atMostOnce is Int)
  // expected-warning@-1 {{cast from '@callable(once) () -> Void' to unrelated type 'Int' always fails}}
  // expected-error@-2 {{non-'Copyable' types cannot be conditionally cast}}
}

// MARK: - rethrows

struct RethrowsError: Error {}

// A `rethrows` function can call and forward its own parameters with
// execution semantics, which are implicitly `consuming`.

func rethrowsCallOnce<R>(_ body: @once () throws -> R) rethrows -> R {
  try body()
}

func rethrowsCallAtMostOnce<R>(
  _ body: @callable(once) () throws -> R
) rethrows -> R? {
  Bool.random() ? try body() : nil
}

func rethrowsForwardOnce<R>(_ body: @once () throws -> R) rethrows -> R {
  try rethrowsCallOnce(body)
}

func rethrowsForwardOnceInClosure<R>(
  _ body: @once () throws -> R
) rethrows -> R {
  try rethrowsCallOnce { try body() }
}

func rethrowsForwardAtMostOnce<R>(
  _ body: @callable(once) () throws -> R
) rethrows -> R? {
  try rethrowsCallAtMostOnce(body)
}

func rethrowsCallOptional(
  _ body: consuming (@callable(once) () throws -> Void)?
) rethrows {
  try body?()
}

func rethrowsCallConsumed<R>(_ body: @once () throws -> R) rethrows -> R {
  try (consume body)()
}

func rethrowsForwardConsumed<R>(_ body: @once () throws -> R) rethrows -> R {
  try rethrowsCallOnce(consume body)
}

// A parameter that's modified no longer holds the argument.
func rethrowsCallReassigned(
  _ body: @callable(once) () throws -> Void
) rethrows {
  body = { throw RethrowsError() }
  try body() // expected-error {{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
}

func rethrowsCallReassignedInClosure(
  _ body: @callable(once) () throws -> Void
) rethrows {
  func reassign() { body = { throw RethrowsError() } }
  reassign()
  try body() // expected-error {{call can throw, but the error is not handled; a function declared 'rethrows' may only throw if its parameter does}}
}

func callRethrowsWithoutThrowing() {
  _ = rethrowsCallOnce { 1 }
  _ = rethrowsForwardOnce { 1 }
  _ = rethrowsForwardOnceInClosure { 1 }
  _ = rethrowsForwardAtMostOnce { 1 }
  _ = rethrowsCallConsumed { 1 }
  _ = rethrowsForwardConsumed { 1 }
  rethrowsCallOptional(nil)
  rethrowsCallOptional { }
}

func callRethrowsWithThrowing() throws {
  _ = try rethrowsCallOnce { throw RethrowsError() }
  _ = try rethrowsForwardOnce { throw RethrowsError() }
  _ = try rethrowsForwardAtMostOnce { throw RethrowsError() }
  try rethrowsCallOptional { throw RethrowsError() }
}

func callRethrowsMissingTry() {
  rethrowsForwardOnce { throw RethrowsError() }
  // expected-error@-1 {{call can throw, but it is not marked with 'try' and the error is not handled}}
  // expected-note@-2 {{call is to 'rethrows' function, but argument function can throw}}
}

// MARK: - Printing

// Diagnostics don't print the `consuming` that at-most-once semantics imply.

func printedAndAtMostOnce(
  _ lhs: Bool, _ rhs: @autoclosure @callable(once) () -> Bool
) -> Bool {
  lhs ? rhs() : false
}

func printedExplicitlyConsuming(_: consuming @once () -> Void) {}
func printedInOut(_: inout @callable(once) () -> Void) {}
func printedExplicitlyConsumingInFunctionType(
  _: (consuming @callable(once) () -> Void) -> Void
) {}

// A typealias or `sending` doesn't imply `consuming`, so it's printed.
typealias PrintedAtMostOnce = @callable(once) () -> Void

func printedAlias(_: PrintedAtMostOnce) {}
func printedConsumingAlias(_: consuming PrintedAtMostOnce) {}
func printedConsumingSending(
  _: consuming sending @escaping @callable(once) () -> Void
) {}

func printedTypes() {
  let _: Int = printedAndAtMostOnce
  // expected-error@-1 {{cannot convert value of type '(Bool, @autoclosure @callable(once) () -> Bool) -> Bool' to specified type 'Int'}}
  let _: Int = printedExplicitlyConsuming
  // expected-error@-1 {{cannot convert value of type '(@once () -> Void) -> ()' to specified type 'Int'}}
  let _: Int = printedInOut
  // expected-error@-1 {{cannot convert value of type '(inout @callable(once) () -> Void) -> ()' to specified type 'Int'}}
  let _: Int = printedExplicitlyConsumingInFunctionType
  // expected-error@-1 {{cannot convert value of type '((@callable(once) () -> Void) -> Void) -> ()' to specified type 'Int'}}
  let _: Int = printedAlias
  // expected-error@-1 {{cannot convert value of type '(PrintedAtMostOnce) -> ()' (aka '(@callable(once) () -> ()) -> ()') to specified type 'Int'}}
  let _: Int = printedConsumingAlias
  // expected-error@-1 {{cannot convert value of type '(consuming PrintedAtMostOnce) -> ()' (aka '(@callable(once) () -> ()) -> ()') to specified type 'Int'}}
  let _: Int = printedConsumingSending
  // expected-error@-1 {{cannot convert value of type '(consuming sending @escaping @callable(once) () -> Void) -> ()' to specified type 'Int'}}
}
