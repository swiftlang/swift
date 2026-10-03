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
  // expected-error@-2 {{stored property 'exactlyOnce' of 'Deinitable'-conforming struct 'ExactlyOnceStorage' has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
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

// MARK: - Conversions

func conversionMatrix(
  plain: @escaping () -> Void,
  atMostOnce: @escaping @called(atMostOnce) () -> Void,
  exactlyOnce: @escaping @called(exactlyOnce) () -> Void
) {
  func takesPlain(_: @escaping () -> Void) {}
  func takesAtMostOnce(_: @escaping @called(atMostOnce) () -> Void) {}
  func takesExactlyOnce(_: @escaping @called(exactlyOnce) () -> Void) {}

  takesPlain(plain) // Ok
  takesAtMostOnce(plain) // Ok
  takesExactlyOnce(plain) // Ok

  takesPlain(atMostOnce)
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}
  takesAtMostOnce(atMostOnce) // Ok
  takesExactlyOnce(atMostOnce) // Ok

  takesPlain(exactlyOnce)
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '() -> Void'}}
  takesAtMostOnce(exactlyOnce)
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '@called(atMostOnce) () -> Void'}}
  takesExactlyOnce(exactlyOnce) // Ok

  let _: @called(exactlyOnce) () -> Void = plain // Ok
  let _: @called(exactlyOnce) () -> Void = atMostOnce // Ok
  let _: @called(atMostOnce) () -> Void = exactlyOnce
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '@called(atMostOnce) () -> Void'}}
}

func conversionVariance() {
  func takesAtMostOnce(_: @called(atMostOnce) () -> Void) {}
  func takesExactlyOnce(_: @called(exactlyOnce) () -> Void) {}

  // Parameters are contravariant.
  let _: (@escaping @called(atMostOnce) () -> Void) -> Void = takesExactlyOnce // Ok
  let _: (@escaping @called(exactlyOnce) () -> Void) -> Void = takesAtMostOnce
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '@called(atMostOnce) () -> Void'}}

  func makePlain() -> () -> Void { {} }
  func makeAtMostOnce() -> @called(atMostOnce) () -> Void { {} }
  func makeExactlyOnce() -> @called(exactlyOnce) () -> Void { {} }

  // Results are covariant.
  let _: () -> @called(exactlyOnce) () -> Void = makePlain // Ok
  let _: () -> @called(exactlyOnce) () -> Void = makeAtMostOnce // Ok
  let _: () -> @called(atMostOnce) () -> Void = makeExactlyOnce
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '@called(atMostOnce) () -> Void'}}
  let _: () -> () -> Void = makeExactlyOnce
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '() -> Void'}}
}

func conversionClosures() {
  func takesExactlyOnce(_: @called(exactlyOnce) () -> Void) {}
  func takesAtMostOnce(_: @called(atMostOnce) () -> Void) {}

  takesExactlyOnce { @called(atMostOnce) in } // Ok
  takesAtMostOnce { @called(exactlyOnce) in }
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> ()' to function type '@called(atMostOnce) () -> Void'}}
}

func conversionJoin(
  _ flag: Bool,
  plain: @escaping () -> Void,
  atMostOnce: @escaping @called(atMostOnce) () -> Void,
  exactlyOnce: @escaping @called(exactlyOnce) () -> Void
) {
  // The join of two function types has the more restrictive semantics.
  let a = flag ? plain : exactlyOnce
  let _: () -> Void = a
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '() -> Void'}}
  let b = flag ? atMostOnce : exactlyOnce
  let _: @called(atMostOnce) () -> Void = b
  // expected-error@-1 {{invalid conversion from '@called(exactlyOnce)' function of type '@called(exactlyOnce) () -> Void' to function type '@called(atMostOnce) () -> Void'}}
  let c = flag ? plain : atMostOnce
  let _: () -> Void = c
  // expected-error@-1 {{invalid conversion from '@called(atMostOnce)' function of type '@called(atMostOnce) () -> Void' to function type '() -> Void'}}
}

// MARK: - Witness matching

// Parameters
protocol Completable {
  func onCompletion(_ body: @called(exactlyOnce) () -> Void)
  // expected-note@-1 2 {{protocol requires function 'onCompletion' with type '(consuming @called(exactlyOnce) () -> Void) -> ()'}}
}

struct Request: Completable {
  func onCompletion(_ body: @called(exactlyOnce) () -> Void) {} // Ok
}

struct Broadcast: Completable { // expected-error {{type 'Broadcast' does not conform to protocol 'Completable'}}
  // expected-note@-1 {{add stubs for conformance}}
  func onCompletion(_ body: () -> Void) {}
  // expected-note@-1 {{candidate has non-matching type '(() -> Void) -> ()'}}
}

struct Retry: Completable { // expected-error {{type 'Retry' does not conform to protocol 'Completable'}}
  // expected-note@-1 {{add stubs for conformance}}
  func onCompletion(_ body: @called(atMostOnce) () -> Void) {}
  // expected-note@-1 {{candidate has non-matching type '(consuming @called(atMostOnce) () -> Void) -> ()'}}
}

protocol CompletableAtMostOnce {
  func onCompletion(_ body: @called(atMostOnce) () -> Void)
}

struct ExactlyOnceRequest: CompletableAtMostOnce {
  func onCompletion(_ body: @called(exactlyOnce) () -> Void) {} // Ok
}

protocol CompletablePlain {
  func onCompletion(_ body: () -> Void)
}

struct ExactlyOncePlainRequest: CompletablePlain {
  func onCompletion(_ body: @called(exactlyOnce) () -> Void) {} // Ok
}

// Results
protocol SingleResponse {
  func callback() -> @called(exactlyOnce) () -> Void
}

struct Success: SingleResponse {
  func callback() -> () -> Void { {} } // Ok
}

struct AtMostOnceSuccess: SingleResponse {
  func callback() -> @called(atMostOnce) () -> Void { {} } // Ok
}

struct ExactlyOnceSuccess: SingleResponse {
  func callback() -> @called(exactlyOnce) () -> Void { {} } // Ok
}

protocol AtMostOnceResponse {
  func callback() -> @called(atMostOnce) () -> Void
  // expected-note@-1 {{protocol requires function 'callback()' with type '() -> @called(atMostOnce) () -> Void'}}
}

struct ExactlyOnceFailure: AtMostOnceResponse { // expected-error {{type 'ExactlyOnceFailure' does not conform to protocol 'AtMostOnceResponse'}}
  // expected-note@-1 {{add stubs for conformance}}
  func callback() -> @called(exactlyOnce) () -> Void { {} }
  // expected-note@-1 {{candidate has non-matching type '() -> @called(exactlyOnce) () -> Void'}}
}

protocol Response {
  func callback() -> () -> Void
  // expected-note@-1 {{protocol requires function 'callback()' with type '() -> () -> Void'}}
}

struct Failure: Response { // expected-error {{type 'Failure' does not conform to protocol 'Response'}}
  // expected-note@-1 {{add stubs for conformance}}
  func callback() -> @called(exactlyOnce) () -> Void { {} }
  // expected-note@-1 {{candidate has non-matching type '() -> @called(exactlyOnce) () -> Void'}}
}

// Overrides must match the `@called` semantics of the overridden method
// exactly.
class OverrideBase {
  func plainParam(_: @escaping () -> Void) {}
  // expected-note@-1 {{potential overridden instance method 'plainParam' here}}
  func atMostOnceParam(_: @escaping @called(atMostOnce) () -> Void) {}
  // expected-note@-1 {{potential overridden instance method 'atMostOnceParam' here}}
  func exactlyOnceResult() -> @called(exactlyOnce) () -> Void { {} }
  // expected-note@-1 {{potential overridden instance method 'exactlyOnceResult()' here}}
}

class OverrideDerivedBad: OverrideBase {
  override func plainParam(_: @escaping @called(exactlyOnce) () -> Void) {}
  // expected-error@-1 {{method does not override any method from its superclass}}
  override func atMostOnceParam(_: @escaping @called(exactlyOnce) () -> Void) {}
  // expected-error@-1 {{method does not override any method from its superclass}}
  override func exactlyOnceResult() -> () -> Void { {} }
  // expected-error@-1 {{method does not override any method from its superclass}}
}

class OverrideDerivedGood: OverrideBase {
  override func plainParam(_: @escaping () -> Void) {}
  override func atMostOnceParam(_: @escaping @called(atMostOnce) () -> Void) {}
  override func exactlyOnceResult() -> @called(exactlyOnce) () -> Void { {} }
}

// MARK: - Overload resolution

func overloadRanking(
  plain: @escaping () -> Void,
  atMostOnce: @escaping @called(atMostOnce) () -> Void,
  exactlyOnce: @escaping @called(exactlyOnce) () -> Void
) {
  func overloaded(_: @escaping @called(atMostOnce) () -> Void) -> Int { 0 }
  func overloaded(_: @escaping @called(exactlyOnce) () -> Void) -> String { "" }

  // The closest execution semantics win.
  let _: Int = overloaded(plain)
  let _: Int = overloaded(atMostOnce)
  let _: String = overloaded(exactlyOnce)

  func plainOverloaded(_: @escaping () -> Void) -> Int { 0 }
  func plainOverloaded(_: @escaping @called(exactlyOnce) () -> Void) -> String { "" }

  let _: Int = plainOverloaded(plain)
  let _: String = plainOverloaded(atMostOnce)
  let _: String = plainOverloaded(exactlyOnce)

  // A closure literal adopts either contextual type, so the less restrictive
  // overload wins because it's more specialized.
  let _: Int = overloaded { }
  let _: Int = plainOverloaded { }
  let _: Int = overloaded { @called(atMostOnce) in }
  let _: String = overloaded { @called(exactlyOnce) in }
}

// MARK: - Existential conversions

protocol ExistentialProto {}
extension Int: ExistentialProto {}

func existentialConversions(
  atMostOnce: @escaping @called(atMostOnce) () -> Void,
  makeInt: @escaping () -> Int
) {
  // Calling a function doesn't help if the function value itself lacks an
  // invertible protocol that the existential requires.
  let _: any ~Escapable = atMostOnce
  // expected-error@-1 {{value of type '@called(atMostOnce) () -> Void' does not conform to specified type 'Copyable'}}

  let _: any ExistentialProto = makeInt
  // expected-error@-1 {{function produces expected type 'Int'; did you mean to call it with '()'?}}
}

// MARK: - Generic arguments

// A `@called(exactlyOnce)` function type isn't `Deinitable`, so it can't be a
// generic argument unless the parameter suppresses `Deinitable`.

struct NCBox<T: ~Copyable>: ~Copyable {
  // expected-note@-1 {{required by generic struct 'NCBox' where 'T' = '@called(exactlyOnce) () -> Void'}}
  var value: T
}

@available(SwiftStdlib 5.9, *)
struct PackHolder<each T> {}

@available(SwiftStdlib 5.9, *)
func genericArgumentPack(
  _: PackHolder<Int, @called(exactlyOnce) () -> Void>
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' does not conform to protocol 'Copyable'}}
) {}

func genericArgumentSpelled(
  _: Optional<@called(exactlyOnce) () -> Void>,
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' does not conform to protocol 'Deinitable'}}
  _: (@called(exactlyOnce) () -> Void)?,
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' does not conform to protocol 'Deinitable'}}
  _: [@called(exactlyOnce) () -> Void],
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' does not conform to protocol 'Copyable'}}
  _: [Int: @called(exactlyOnce) () -> Void],
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' does not conform to protocol 'Copyable'}}
  _: consuming NCBox<@called(exactlyOnce) () -> Void>,
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' does not conform to protocol 'Deinitable'}}
  _: consuming (@called(atMostOnce) () -> Void)?, // Ok
  _: consuming NCBox<@called(atMostOnce) () -> Void> // Ok
) {}

func genericArgumentInferred(
  exactlyOnce: @escaping @called(exactlyOnce) () -> Void,
  atMostOnce: @escaping @called(atMostOnce) () -> Void
) {
  func genericNC<T: ~Copyable>(_: consuming T) {}
  // expected-note@-1 2{{required by local function 'genericNC' where 'T' = '@called(exactlyOnce) () -> Void'}}
  // expected-note@-2 {{required by local function 'genericNC' where 'T' = '@called(exactlyOnce) () -> ()'}}

  genericNC(exactlyOnce)
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' cannot conform to 'Deinitable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
  genericNC { @called(exactlyOnce) in }
  // expected-error@-1 {{type '@called(exactlyOnce) () -> ()' cannot conform to 'Deinitable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
  genericNC(atMostOnce) // Ok

  _ = NCBox(value: exactlyOnce)
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' cannot conform to 'Deinitable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}

  // FIXME: [deinitable] Optional's payload must be Deinitable.
  let _: _? = exactlyOnce
  let _: _? = atMostOnce // Ok

  let _ = Optional.some(exactlyOnce)
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' cannot conform to 'Deinitable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
  // expected-note@-3 {{required by generic enum 'Optional' where 'Wrapped' = '@called(exactlyOnce) () -> Void'}}

  let _: any ~Copyable = exactlyOnce
  // expected-error@-1 {{value of type '@called(exactlyOnce) () -> Void' does not conform to specified type 'Deinitable'}}
  let _: any ~Copyable = atMostOnce // Ok

  _ = { (f: @escaping @called(exactlyOnce) () -> Void) in
    genericNC(f)
    // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' cannot conform to 'Deinitable'}}
    // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
    return 0
  }
}

// MARK: - Captures

// A closure owns its capture list entries and the captures that it consumes,
// and only a `@called(exactlyOnce)` closure isn't `Deinitable`. So it's the
// only kind of closure that can own a `@called(exactlyOnce)` value. Consuming
// a capture in a closure that might run more than once is diagnosed after
// type checking; see test/SIL/called_exactly_once.swift.
func captureRules(
  _ a: @escaping @called(exactlyOnce) () -> Void,
  _ b: @escaping @called(exactlyOnce) () -> Void,
  _ c: @escaping @called(exactlyOnce) () -> Void,
  _ d: @escaping @called(exactlyOnce) () -> Void,
  _ e: @escaping @called(exactlyOnce) () -> Void,
  _ f: @escaping @called(exactlyOnce) () -> Void,
  _ g: @escaping @called(exactlyOnce) () -> Void,
  _ h: @escaping @called(exactlyOnce) () -> Void
) {
  func takesExactlyOnce(_: @called(exactlyOnce) () -> Void) {}
  func takesAtMostOnce(_: @called(atMostOnce) () -> Void) {}

  let _ = { @called(exactlyOnce) in a() } // Ok
  takesExactlyOnce { b() } // Ok

  takesAtMostOnce { c() }
  // expected-error@-1 {{consumed capture 'c' of 'Deinitable'-conforming closure has non-Deinitable type '@called(exactlyOnce) () -> Void'}}

  // Nested closures
  let _ = { @called(exactlyOnce) in
    let inner = { @called(exactlyOnce) in d() } // Ok
    inner()
  }
  let _ = { @called(atMostOnce) in
    let inner = { @called(exactlyOnce) in e() }
    // expected-error@-1 {{consumed capture 'e' of 'Deinitable'-conforming closure has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
    inner()
  }

  // Capture lists
  let _ = { [f] in f() }
  // expected-error@-1 {{capture list entry 'f' of 'Deinitable'-conforming closure has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  let _ = { @called(atMostOnce) [g] in g() }
  // expected-error@-1 {{capture list entry 'g' of 'Deinitable'-conforming closure has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  let _ = { @called(exactlyOnce) [h] in h() } // Ok
}

// A `@called(atMostOnce)` value is `Deinitable`, so any closure can own it.
func captureRulesAtMostOnce(
  _ a: @escaping @called(atMostOnce) () -> Void,
  _ b: @escaping @called(atMostOnce) () -> Void,
  _ c: @escaping @called(atMostOnce) () -> Void,
  _ d: @escaping @called(atMostOnce) () -> Void
) {
  let _ = { @called(atMostOnce) in a() } // Ok
  let _ = { @called(exactlyOnce) in b() } // Ok
  let _ = { @called(atMostOnce) [c] in c() } // Ok
  let _ = { [d] in _ = d } // Ok
}

// MARK: - Storage

// Storage that can drop a value without calling it requires `Deinitable`.

struct StoredPropertyStruct: ~Copyable {
  let a: @called(exactlyOnce) () -> Void
  // expected-error@-1 {{stored property 'a' of 'Deinitable'-conforming struct 'StoredPropertyStruct' has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  var b: @called(exactlyOnce) () -> Void // Only the first is diagnosed.
  static let c: @called(exactlyOnce) () -> Void = { }
  // expected-error@-1 {{static property 'c' cannot have non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  let atMostOnce: @called(atMostOnce) () -> Void // Ok

  // Computed properties and subscripts behave like functions.
  var computed: @called(exactlyOnce) () -> Void { { } } // Ok
  var getSet: @called(exactlyOnce) () -> Void { // Ok
    get { { } }
    set { newValue() }
  }
  subscript(i: Int) -> @called(exactlyOnce) () -> Void { { } } // Ok
}

class StoredPropertyClass {
  var a: @called(exactlyOnce) () -> Void
  // expected-error@-1 {{stored property 'a' of 'Deinitable'-conforming class 'StoredPropertyClass' has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  lazy var b: @called(exactlyOnce) () -> Void = { }
  // expected-error@-1 {{lazy property 'b' cannot have non-Deinitable type '@called(exactlyOnce) () -> Void'}}

  init(a: @escaping @called(exactlyOnce) () -> Void) {
    self.a = a
  }
}

actor StoredPropertyActor {
  let a: @called(exactlyOnce) () -> Void
  // expected-error@-1 {{stored property 'a' of 'Deinitable'-conforming actor 'StoredPropertyActor' has non-Deinitable type '@called(exactlyOnce) () -> Void'}}

  init(a: @escaping @called(exactlyOnce) () -> Void) {
    self.a = a
  }
}

let globalExactlyOnce: @called(exactlyOnce) () -> Void = { }
// expected-error@-1 {{global variable 'globalExactlyOnce' cannot have non-Deinitable type '@called(exactlyOnce) () -> Void'}}

enum Payloads: ~Copyable {
  case a(@called(exactlyOnce) () -> Void)
  // expected-error@-1 {{associated value 'a' of 'Deinitable'-conforming enum 'Payloads' has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  case b(Int, label: @called(exactlyOnce) () -> Void)
}

enum IndirectPayloads: ~Copyable {
  indirect case a(@called(exactlyOnce) () -> Void)
  // expected-error@-1 {{associated value 'a' of 'Deinitable'-conforming enum 'IndirectPayloads' has non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  // expected-error@-2 {{noncopyable enum 'IndirectPayloads' cannot be marked indirect or have indirect cases yet}}
}

func tupleParameter(_: (@called(exactlyOnce) () -> Void, Int)) {}
// expected-error@-1 {{tuple with noncopyable element type '@called(exactlyOnce) () -> Void' is not supported}}
// expected-error@-2 {{parameter of noncopyable type '(@called(exactlyOnce) () -> Void, Int)' must specify ownership}}
// expected-note@-3 {{add 'borrowing' for an immutable reference}}
// expected-note@-4 {{add 'inout' for a mutable reference}}
// expected-note@-5 {{add 'consuming' to take the value from the caller}}

func tupleResult() -> (Int, @called(exactlyOnce) () -> Void) { fatalError() }
// expected-error@-1 {{tuple with noncopyable element type '@called(exactlyOnce) () -> Void' is not supported}}

func tupleStorage(_ f: @escaping @called(exactlyOnce) () -> Void) {
  let _: (x: @called(exactlyOnce) () -> Void, y: Int)
  // expected-error@-1 {{tuple with noncopyable element type '@called(exactlyOnce) () -> Void' is not supported}}
  let _: ((Int, @called(exactlyOnce) () -> Void), Int)
  // expected-error@-1 {{tuple with noncopyable element type '@called(exactlyOnce) () -> Void' is not supported}}
  let _ = (f, 0)
  // expected-error@-1 {{tuple with noncopyable element type '@called(exactlyOnce) () -> Void' is not supported}}
  let _: (@called(exactlyOnce) () -> Void) = { } // Ok, not a tuple
}

func makeExactlyOnce() -> @called(exactlyOnce) () -> Void { { } }

func asyncLetStorage() async {
  async let f: @called(exactlyOnce) () -> Void = makeExactlyOnce()
  // expected-error@-1 {{'async let' binding 'f' cannot have non-Deinitable type '@called(exactlyOnce) () -> Void'}}
  await f()
}

// Local variables and `inout` parameters keep the obligation with their scope
// or with the caller.
func allowedStorage(_ f: inout @called(exactlyOnce) () -> Void) {
  let local: @called(exactlyOnce) () -> Void = { }
  var localVar: @called(exactlyOnce) () -> Void = { }
  localVar = { }
  _ = local
  _ = localVar
}

// MARK: - Property wrappers

@propertyWrapper
struct NCWrapper<T: ~Copyable>: ~Copyable {
// expected-note@-1 {{required by generic struct 'NCWrapper' where 'T' = '@called(exactlyOnce) () -> Void'}}
  var wrappedValue: T
}

struct WrappedStorage: ~Copyable {
  @NCWrapper var wrapped: @called(exactlyOnce) () -> Void
  // expected-error@-1 {{type '@called(exactlyOnce) () -> Void' cannot conform to 'Deinitable'}}
  // expected-note@-2 {{only concrete types such as structs, enums and classes can conform to protocols}}
}

// MARK: - Conventions

func conventions(
  _: @convention(c) @called(atMostOnce) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
  _: @convention(thin) @called(atMostOnce) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
  _: @convention(block) @called(atMostOnce) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
  _: @convention(c) @called(exactlyOnce) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
  _: @convention(thin) @called(exactlyOnce) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
  _: @convention(block) @called(exactlyOnce) () -> Void,
  // expected-error@-1 {{'@convention' attribute is not allowed on '@called' types}}
  _: @convention(swift) @called(atMostOnce) () -> Void, // Ok
  _: @convention(swift) @called(exactlyOnce) () -> Void // Ok
) {}

// MARK: - copy

func copyOperator(
  _ exactlyOnce: @called(exactlyOnce) () -> Void,
  _ atMostOnce: @called(atMostOnce) () -> Void
) {
  _ = copy exactlyOnce // expected-error {{'copy' cannot be applied to noncopyable types}}
  _ = copy atMostOnce // expected-error {{'copy' cannot be applied to noncopyable types}}
}
