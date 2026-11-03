// RUN: %target-swift-frontend -emit-sil -swift-version 6 -target %target-swift-5.1-abi-triple -verify %s -o /dev/null

// REQUIRES: concurrency

// Make sure that when we call a non-Sendable function value, we merge the
// callee's region (i.e. its captures) with the regions of the call's
// arguments and results.

////////////////////////
// MARK: Declarations //
////////////////////////

class NonSendableKlass {
  var other: NonSendableKlass? = nil
}

@MainActor func transferToMain<T>(_ t: T) async {}
func useValue<T>(_ t: T) {}
func sendValue<T>(_ t: consuming sending T) {}
func mergeFn(_ a: NonSendableKlass, _ b: NonSendableKlass) {}

@globalActor actor CustomActor {
  static let shared = CustomActor()
}

func connect(_ x: NonSendableKlass) -> (NonSendableKlass) -> Void {
  return { y in
    x.other = y
    y.other = x
  }
}

func makeMerger(_ x: NonSendableKlass) -> (NonSendableKlass) -> Void {
  return { mergeFn($0, x) }
}

extension NonSendableKlass {
  func link(_ x: NonSendableKlass) { other = x }
}

struct ClosureHolder {
  var f: (NonSendableKlass) -> Void
}

func identity<T>(_ t: T) -> T { t }

/////////////////
// MARK: Tests //
/////////////////

actor Custom {
  var field = NonSendableKlass()

  func returnedClosure() -> sending NonSendableKlass {
    let x = NonSendableKlass()
    let y = NonSendableKlass()
    connect(x)(y)
    field = y
    return x // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'self'-isolated 'x' cannot be a 'sending' result. 'self'-isolated uses may race with caller uses}}
  }

  func localClosure() -> sending NonSendableKlass {
    let x = NonSendableKlass()
    let y = NonSendableKlass()
    let f: (NonSendableKlass) -> Void = { x.other = $0 }
    f(y)
    field = y
    return x // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'self'-isolated 'x' cannot be a 'sending' result. 'self'-isolated uses may race with caller uses}}
  }

  func closureParameter(_ f: (NonSendableKlass) -> Void) -> sending NonSendableKlass {
    let x = NonSendableKlass()
    f(x)
    return x // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'self'-isolated 'x' cannot be a 'sending' result. 'self'-isolated uses may race with caller uses}}
  }

  func sendableClosureParameter(_ f: @Sendable (NonSendableKlass) -> Void) -> sending NonSendableKlass {
    let x = NonSendableKlass()
    f(x)
    return x
  }

  func closureParameterResult(_ f: () -> NonSendableKlass) -> sending NonSendableKlass {
    return f() // expected-error {{task or actor-isolated value cannot be sent}}
  }

  // Make sure that merging in the callee does not hide errors from assigning
  // into a 'sending' indirect result.
  func closureParameterIndirectResult<T>(_ f: (NonSendableKlass) -> T) -> sending T {
    return f(field)
    // expected-error @-1 {{returning 'self'-isolated 'self.field' as a 'sending' result risks causing data races; 'self.field' is accessible to code in the current isolation context}}
    // expected-note @-2 {{returning 'self'-isolated 'self.field' risks causing data races since the caller assumes that 'self.field' can be safely sent to other isolation domains, but 'self.field' is accessible to code in the current isolation context}}
    // expected-error @-3 {{returning 'self'-isolated 'f' as a 'sending' result risks causing data races; 'f' is accessible to code in the current isolation context}}
    // expected-note @-4 {{returning 'self'-isolated 'f' risks causing data races since the caller assumes that 'f' can be safely sent to other isolation domains, but 'f' is accessible to code in the current isolation context}}
  }

  func optionalClosureParameter(_ f: ((NonSendableKlass) -> Void)?) -> sending NonSendableKlass {
    let x = NonSendableKlass()
    f?(x)
    return x // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'self'-isolated 'x' cannot be a 'sending' result. 'self'-isolated uses may race with caller uses}}
  }

  func escapingClosureParameter(_ f: @escaping (NonSendableKlass) -> Void) -> sending NonSendableKlass {
    let x = NonSendableKlass()
    f(x)
    return x // expected-error {{sending 'x' risks causing data races}}
    // expected-note @-1 {{'self'-isolated 'x' cannot be a 'sending' result. 'self'-isolated uses may race with caller uses}}
  }

  // A 'sending' result is disconnected, so it is not merged with the callee
  // even though the callee is actor-isolated.
  func closureParameterSendingResult(_ f: (NonSendableKlass) -> sending NonSendableKlass) -> sending NonSendableKlass {
    return f(field)
  }
}

func useAfterSendViaClosureCapture() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f: (NonSendableKlass) -> Void = { x.other = $0 }
  f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

func noCaptureClosureDoesNotMerge() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f: (NonSendableKlass) -> Void = { _ in }
  f(y)
  await transferToMain(y)
  useValue(x)
}

// A nonisolated(nonsending) closure runs in the caller's isolation, so the
// call does not cross isolation and must merge the callee.
func useAfterSendViaNonsendingAsyncClosureCapture(_ b: NonSendableKlass) async {
  let a = NonSendableKlass()
  let f: nonisolated(nonsending) (NonSendableKlass) async -> Void = { mergeFn($0, b) }
  await f(a)
  await transferToMain(a) // expected-error {{sending 'a' risks causing data races}}
  // expected-note @-1 {{sending 'a' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated code and code in the current isolation context}}
}

// Calling a @concurrent closure crosses isolation, so 'a' is sent to the
// callee rather than merged with it. 'a' is disconnected, so this is fine.
func concurrentAsyncClosureSendsArgument() async {
  let a = NonSendableKlass()
  let f: @concurrent (NonSendableKlass) async -> Void = { _ in }
  await f(a)
  useValue(a)
}

// The closure is in CustomActor's region since it captures a CustomActor
// value, so calling it with a main actor value merges incompatible regions.
@MainActor
struct CalleeInDifferentIsolationRegion {
  var mainField: NonSendableKlass? = nil
  @CustomActor var customField: NonSendableKlass? = nil

  init() {
    let f = makeMerger(customField!) // expected-note {{'f' is exposed to global actor 'CustomActor'-isolated code}}
    f(mainField!) // expected-warning {{executing operation could allow for references between values exposed to global actor 'CustomActor'-isolated code and main actor-isolated code risking data races}}
    // expected-note @-1 {{'self.mainField' is exposed to main actor-isolated code}}
  }
}

// The result of calling a closure is in the region of the closure's captures.
func closureResultAliasesCapture() {
  let ns = NonSendableKlass()
  let closure = { ns }
  // FIXME: The closure is named by its mangled name in this diagnostic.
  sendValue(closure()) // expected-error {{risks causing data races}}
  // expected-note @-1 {{used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

func closureResultAliasesCaptureThroughGenericFunction() {
  let ns = NonSendableKlass()
  func idFn<T>(_ t: T) -> T { t }
  let closure = { ns }
  let alias = idFn(closure())
  sendValue(alias) // expected-error {{sending 'alias' risks causing data races}}
  // expected-note @-1 {{'alias' used after being passed as a 'sending' parameter; Later uses could race}}
  useValue(ns) // expected-note {{access can happen concurrently}}
}

// Values returned from a closure capturing a task-isolated value are
// task-isolated, so they cannot be sent to concurrently executing child tasks.
func closureResultSentToTaskGroup(_ ns: NonSendableKlass) async {
  let c = { ns }

  await withTaskGroup { group in
    let ns1 = c()
    let ns2 = c()

    group.addTask { // expected-error {{passing closure as a 'sending' parameter risks causing data races between code in the current isolation context and concurrent execution of the closure}}
      useValue(ns1) // expected-note {{closure captures 'ns1' which is accessible to code in the current isolation context}}
    }

    group.addTask { // expected-error {{passing closure as a 'sending' parameter risks causing data races between code in the current isolation context and concurrent execution of the closure}}
      useValue(ns2) // expected-note {{closure captures 'ns2' which is accessible to code in the current isolation context}}
    }
  }
}

func closureResultOfTaskIsolatedCaptureSent(_ ns: NonSendableKlass) {
  let c = { ns }

  // FIXME: The closure is named by its mangled name in these diagnostics.
  sendValue(c()) // expected-error {{risks causing data races}}
  // expected-note @-1 {{is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}

  sendValue(c()) // expected-error {{risks causing data races}}
  // expected-note @-1 {{is passed as a 'sending' parameter; Uses in callee may race with code in the current isolation context}}
}

// Calling a closure parameter merges 'x' into the closure's task-isolated
// region, so a closure capturing 'x' cannot be a 'sending' result.
func closureParameterTaskIsolatedSendingClosureResult(_ f: (NonSendableKlass) -> Void) -> sending () -> Void {
  let x = NonSendableKlass()
  f(x)
  // FIXME: This diagnostic should name 'x' and its isolation.
  return { useValue(x) } // expected-error {{task or actor-isolated value cannot be sent}}
}

////////////////////////////////////
// MARK: Forms of Closure Callees //
////////////////////////////////////

// Each of these calls a closure value capturing 'x' through a different SIL
// lowering, merging 'y' into the region of 'x'.

// A local function used as a value is a partial_apply.
func localFunctionValue() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  func g(_ n: NonSendableKlass) { x.other = n }
  let f = g
  f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// An unapplied method reference is a curry thunk that captures 'self'.
func methodReference() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f = x.link
  f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// A closure stored in a 'var' is loaded from a box before being called.
func closureLoadedFromVar() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  var f: (NonSendableKlass) -> Void = { _ in }
  f = { x.other = $0 }
  f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// A closure stored in a struct is extracted before being called.
func closureStoredInStruct() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let h = ClosureHolder(f: { x.other = $0 })
  h.f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// A closure stored in an array is reabstracted when it is loaded.
func closureStoredInArray() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let fs: [(NonSendableKlass) -> Void] = [{ x.other = $0 }]
  fs[0](y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// A closure returned from generic code is wrapped in a reabstraction thunk.
func closureReabstractedThroughGeneric() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f = identity({ (n: NonSendableKlass) in x.other = n })
  f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// A function conversion is a convert_function on the closure.
func convertedClosure() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f: (NonSendableKlass) -> Void = { x.other = $0 }
  let g: (NonSendableKlass) throws -> Void = f
  try? g(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// Calling a throwing closure is a try_apply.
func throwingClosure() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  let f: (NonSendableKlass) throws -> Void = { x.other = $0 }
  try? f(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

// Generic arguments are passed to the closure indirectly.
func closureParameterGenericArgument<T>(_ f: (T) -> Void, _ make: () -> sending T) -> sending T {
  let t = make()
  f(t)
  return t // expected-error {{returning 't' as a 'sending' result risks causing data races}}
  // expected-note @-1 {{returning 't' risks causing data races since the caller assumes that 't' can be safely sent to other isolation domains}}
}

// Directly calling a local function or closure literal passes its captures as
// arguments, so 'y' is merged into the region of 'x' like any other argument.

func localFunctionDirectCall() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  func g(_ n: NonSendableKlass) { x.other = n }
  g(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

func genericLocalFunctionDirectCall() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  func g<T>(_ n: T) { x.other = n as? NonSendableKlass }
  g(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

func localFunctionCapturingVarDirectCall() async {
  var x = NonSendableKlass()
  x = NonSendableKlass()
  let y = NonSendableKlass()
  func g(_ n: NonSendableKlass) { x.other = n }
  g(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

func closureLiteralImmediatelyCalled() async {
  let x = NonSendableKlass()
  let y = NonSendableKlass()
  _ = { x.other = $0 }(y)
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}

func localFunctionResultDirectCall() async {
  let x = NonSendableKlass()
  func g() -> NonSendableKlass { x }
  let y = g()
  await transferToMain(y) // expected-error {{sending 'y' risks causing data races}}
  // expected-note @-1 {{sending 'y' to main actor-isolated global function 'transferToMain' risks causing data races between main actor-isolated and local nonisolated uses}}
  useValue(x) // expected-note {{access can happen concurrently}}
}
