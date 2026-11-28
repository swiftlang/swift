// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// All the ways in which 'send' cannot be used, with a few valid uses next to
// them for contrast.
//
// 'send' is fire-and-forget: it enqueues a call on the callee's actor without
// waiting for it, so there is nobody to receive a result or an error. It can
// be applied to a call of an actor isolated function which returns Void, even
// if its result is '@discardableResult', and does not throw

struct MyError: Error {}

actor Worker {
  func process(_ item: Int) async {}
  func finish() {}
  nonisolated func log(_ line: String) {}
  func count() -> Int { 0 }
  func boom() throws {}
  func typedBoom() throws(MyError) {}
  func neverThrows() throws(Never) {}
  func ensureRunning() {}
  var name: String { "worker" }
  func update(_ value: inout Int) {}
  func updateAsync(_ value: inout Int) async {}
  func updateSecond(_ item: Int, _ value: inout Int) {}
  @discardableResult func confirm() -> Bool { true }
  @discardableResult func confirmAsync() async -> Bool { true }
  @discardableResult func confirmNeverThrows() throws(Never) -> Int { 0 }
  @discardableResult func confirmThrows() throws -> Bool { true }
  func add(_ values: Span<Int>) {}
  func addAsync(_ values: Span<Int>) async {}
  func addRaw(_ item: Int, _ bytes: RawSpan) {}
  func take(_ value: Int) {}
  func forward<each T>(_ values: repeat each T) async {}
}

class Box {
  func update() {}
}

func freeFunction() {}
@MainActor func mainActorFunction() {}
@MainActor func throwingFreeFunction() throws {}
@MainActor func rethrowingFunction(_ body: () throws -> Void) rethrows {}
@MainActor func runEscaping(_ body: @escaping @Sendable () -> Void) {}
@MainActor func runNonEscaping(_ body: () -> Void) {}
func isolatedParameter(_ worker: isolated Worker) {}

func test(_ worker: Worker, _ box: Box, _ item: Int, _ maybe: Worker?) async {
  // An async Void method
  send worker.process(item)

  // A synchronous Void actor method
  send worker.finish()

  // A global actor isolated function
  send mainActorFunction()

  // A function isolated through an 'isolated' parameter
  send isolatedParameter(worker)

  // Only calls of actor isolated functions can be sent: not a nonisolated
  // method, a method of a nonisolated class, a nonisolated free function, nor
  // a closure value
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send worker.log("line")
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send box.update()
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send freeFunction()
  let closureValue: () async -> Void = {}
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send closureValue()

  // A 'throws(Never)' method does not throw
  send worker.neverThrows()

  // Calls with closure arguments
  send runEscaping { print("escaping") }
  send runNonEscaping { print("non-escaping") }

  // A method which returns a value which is not discardable
  // expected-error@+1{{'send' cannot discard the result of a call to 'count()'}}{{3-7=await}}
  send worker.count()

  // A closure value which returns a value
  let counter: () -> Int = { 0 }
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send counter()

  // A '@discardableResult' method must return 'Void' too
  // expected-error@+1{{'send' cannot discard the result of a call to 'confirm()'}}{{3-7=await}}
  send worker.confirm()
  // expected-error@+1{{'send' cannot discard the result of a call to 'confirmAsync()'}}{{3-7=await}}
  send worker.confirmAsync()
  // expected-error@+1{{'send' cannot discard the result of a call to 'confirmNeverThrows()'}}{{3-7=await}}
  send worker.confirmNeverThrows()
  // expected-error@+1{{'send' cannot discard the result of a call to 'confirm()'}}{{3-7=await}}
  send maybe?.confirm()

  // A throwing method with a result: the result is diagnosed first
  // expected-error@+1{{'send' cannot discard the result of a call to 'confirmThrows()'}}{{3-7=await}}
  send worker.confirmThrows()

  // A single-expression closure can consist of a 'send' call
  let closure: () -> Void = { send worker.finish() }
  _ = closure

  // A throwing method, diagnosed once: the operation closure of the 'send'
  // throws, even though the enclosing function does not
  // expected-error@+1{{'send' cannot discard error thrown by call to 'boom()'}}{{3-7=await}}
  send worker.boom()

  // A method with a typed 'throws'
  // expected-error@+1{{'send' cannot discard error thrown by call to 'typedBoom()'}}
  send worker.typedBoom()

  // A throwing free function
  // expected-error@+1{{'send' cannot discard error thrown by call to 'throwingFreeFunction()'}}
  send throwingFreeFunction()

  // A throwing closure value
  let throwingClosure: () throws -> Void = {}
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send throwingClosure()

  // A 'rethrows' function can throw, even if its argument does not
  // expected-error@+1{{'send' cannot discard error thrown by call to 'rethrowingFunction'}}
  send rethrowingFunction({})

  // An optional chain around a call of an async method: it does not make
  // the call if 'maybe' is nil
  send maybe?.process(item)

  // An optional chain around a call of a synchronous method
  send maybe?.ensureRunning()

  // The caller does not wait for the call, so it cannot get the value of an
  // 'inout' argument back
  var value = 0
  // expected-error@+1{{'send' cannot be applied to a call with 'inout' arguments}}
  send worker.update(&value)
  // expected-error@+1{{'send' cannot be applied to a call with 'inout' arguments}}
  send worker.updateAsync(&value)
  // expected-error@+1{{'send' cannot be applied to a call with 'inout' arguments}}
  send worker.updateSecond(item, &value)
  await worker.update(&value)

  // The lifetime of a non-escapable value is bound to the caller's scope,
  // which a 'send' call outlives
  let numbers = [1, 2, 3]
  // expected-error@+1{{'send' cannot be applied to a call with non-escapable argument of type 'Span<Int>'}}
  send worker.add(numbers.span)
  // expected-error@+1{{'send' cannot be applied to a call with non-escapable argument of type 'Span<Int>'}}
  send worker.addAsync(numbers.span)
  // expected-error@+1{{'send' cannot be applied to a call with non-escapable argument of type 'RawSpan'}}
  send worker.addRaw(item, numbers.span.bytes)
  await worker.add(numbers.span)

  // Not a function call
  // expected-error@+1{{'send' can only be applied to a function call}}
  send worker.ensureRunning
  // expected-error@+1{{'send' cannot be applied to property access}}
  send worker.name
}

// 'send' does not need an async context
func syncContext(_ worker: Worker) {
  send worker.ensureRunning()
  send worker.finish()
  send mainActorFunction()
}

// A 'send' call is of type 'Void', so it can be the body of a
// single-expression function, or returned from a 'Void' function
func singleExpression(_ worker: Worker) { send worker.finish() }
func explicitReturn(_ worker: Worker) {
  return send worker.finish()
}

// Only actor isolated functions can be sent to
actor IsolationWorker {
  func isolated() {}
  nonisolated func describe() {}
  nonisolated(nonsending) func log(_ s: String) async {}
  @concurrent func compute() async {}
}

@MainActor struct MainActorBox {
  func update() {}
}

func isolation(_ worker: IsolationWorker, _ box: MainActorBox) async {
  send worker.isolated()
  send box.update()
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send worker.describe()
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send worker.log("hello")
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send worker.compute()
  // A function value may be sent if its function type is actor isolated
  let isolatedClosure: @MainActor () -> Void = {}
  send isolatedClosure()
  let closure: () -> Void = {}
  // expected-error@+1{{'send' can only be applied to calls of actor isolated methods}}
  send closure()
}

// ==== -----------------------------------------------------------------------
// MARK: 'send' combined with 'await'

// 'send' does not wait for the call, so it cannot be combined with 'await',
// in either order. An 'await' within the arguments of a 'send' call is fine,
// it is evaluated by the caller before the call is sent

func value() async -> Int { 0 }

// ==== -----------------------------------------------------------------------
// MARK: 'await send'

func awaitSend(_ worker: Worker) async throws {
  // expected-error@+1:3{{'await' cannot be combined with 'send'; an operation cannot be both awaited on and sent without waiting for its result}}{{3-9=}}
  await send worker.finish()

  // expected-error@+2:7{{'await' cannot be combined with 'send'}}{{7-13=}}
  // expected-warning@+1:3{{no calls to throwing functions occur within 'try' expression}}
  try await send worker.finish()

  // expected-error@+1:3{{'await' cannot be combined with 'send'}}{{3-9=}}
  await (send worker.finish())

  // expected-warning@+3:3{{'try' must precede 'await'}}
  // expected-error@+2:3{{'await' cannot be combined with 'send'}}{{3-9=}}
  // expected-warning@+1:9{{no calls to throwing functions occur within 'try' expression}}
  await try send worker.finish()

  // Also when the 'send' is used as a value
  // expected-error@+1:11{{'await' cannot be combined with 'send'}}{{11-17=}}
  let _ = await send worker.finish()

  // Only the 'await' next to the 'send' is diagnosed, the outer one then has
  // nothing to await
  // expected-error@+2:9{{'await' cannot be combined with 'send'}}{{9-15=}}
  // expected-warning@+1:3{{no 'async' operations occur within 'await' expression}}
  await await send worker.finish()
}

// ==== -----------------------------------------------------------------------
// MARK: 'send await'

func sendAwait(_ worker: Worker) async {
  // expected-error@+1:8{{'send' cannot be combined with 'await'; an operation cannot be both awaited on and sent without waiting for its result}}{{8-14=}}
  send await worker.finish()

  // expected-error@+1:8{{'send' cannot be combined with 'await'}}{{8-14=}}
  send await worker.take(1)

  // expected-error@+1:8{{'send' cannot be combined with 'await'}}{{8-14=}}
  send await await worker.finish()
}

// ==== -----------------------------------------------------------------------
// MARK: Valid

func valid(_ worker: Worker) async throws {
  // An 'await' in an argument is evaluated by the caller
  send worker.take(await value())

  send worker.finish()
}

// ==== -----------------------------------------------------------------------
// MARK: Property and subscript access

// 'send' cannot be applied to a property or subscript access, nor to the
// assignment of a property

actor PropertyWorker {
  var count: Int = 0

  var message: String {
    get async { "" }
  }

  var name: String {
    get { "" }
    set {}
  }

  subscript(index: Int) -> String {
    get { "" }
    set {}
  }

  static var shared: Int { 0 }
}

func propertyAccess(worker: PropertyWorker, maybe: PropertyWorker?) async {
  send worker.count // expected-error{{'send' cannot be applied to property access}}
  send worker.message // expected-error{{'send' cannot be applied to property access}}
  send worker.name // expected-error{{'send' cannot be applied to property access}}
  send maybe?.count // expected-error{{'send' cannot be applied to property access}}
  send PropertyWorker.shared // expected-error{{'send' cannot be applied to property access}}
  send worker.name = "Caplin" // expected-error{{'send' cannot be applied to property assignment}}
  send worker[0] // expected-error{{'send' cannot be applied to subscript access}}
  send worker[0] = "x" // expected-error{{'send' cannot be applied to subscript access}}
}

func propertyAccessSync(worker: PropertyWorker) {
  send worker.count // expected-error{{'send' cannot be applied to property access}}
  send worker.name = "Caplin" // expected-error{{'send' cannot be applied to property assignment}}
  send worker[0] // expected-error{{'send' cannot be applied to subscript access}}
}

// ==== -----------------------------------------------------------------------
// MARK: Evaluation at the call site

// 'send' evaluates the receiver, the callee and the arguments of a call which
// runs in a new task at the call site, also in an optional chain. Their
// effects therefore happen at the call site and need their own 'try' or
// 'await' there. A call which cannot be evaluated this way is diagnosed,
// rather than evaluated in the task

func mayThrow() throws -> Int { 0 }
func asyncValue() async -> Int { 0 }

@MainActor struct Counter {
  var count = 0
  mutating func increment() async { count += 1 }
}

func optionalChainArgumentThrows(_ maybe: Worker?) {
  // expected-error@+1{{call can throw, but it is not marked with 'try' and the error is not handled}}
  send maybe?.process(mayThrow())
}

func optionalChainArgumentThrowsInThrowingContext(_ maybe: Worker?) throws {
  send maybe?.process(try mayThrow())
}

func optionalChainArgumentAwaits(_ maybe: Worker?) async {
  // expected-error@+1{{expression is 'async' but is not marked with 'await'}}
  send maybe?.process(asyncValue())
  // expected-note@-1{{call is 'async'}}

  send maybe?.process(await asyncValue())
}

func argumentThrows(_ worker: Worker) throws {
  send worker.process(try mayThrow())
}

func packExpansion<each T: Sendable>(_ worker: Worker, _ values: repeat each T) {
  // expected-error@+1{{'send' cannot be applied to a call with a pack expansion argument}}
  send worker.forward(repeat each values)
}

@MainActor func mutatingMethod() {
  var counter = Counter()
  // expected-error@+1{{'send' cannot be applied to a call of a mutating method}}
  send counter.increment()
  _ = counter
}
