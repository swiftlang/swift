// RUN: %target-typecheck-verify-swift -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// Which overload a 'send' or 'await' call picks when some of the overloads are
// '@oneway'.
//
// - 'send x.f()': if any '@oneway' overload of 'f' leads to a solution, the
//   solver picks among the '@oneway' overloads only and never attempts the
//   others. If none does, the usual rules pick among all overloads.
// - 'await x.f()': the same the other way around, an overload which is not
//   '@oneway' is preferred if any leads to a solution.
// - Any other call: the usual rules.
//
// Each overload is deprecated with a message naming it, so the deprecation
// warning shows which overload a call picked.

// ==== -----------------------------------------------------------------------
// MARK: A viable '@oneway' overload wins in a 'send' call

actor Basic {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func tell(_ n: Int) {}
  @available(*, deprecated, message: "PLAIN")
  func tell(_ n: Int) async {}

  @available(*, deprecated, message: "ONEWAY")
  @oneway func zero() {}
  @available(*, deprecated, message: "PLAIN")
  func zero() async {}
}

func basic(_ a: Basic) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.zero()
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tell(1)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.zero()
}

func basicFromSync(_ a: Basic) {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
}

// ==== -----------------------------------------------------------------------
// MARK: The '@oneway' preference beats the usual ranking

// The usual rules would pick the exact match, the '@oneway' overload needs
// a value-to-optional conversion
actor NeedsConversion {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func tell(_ n: Int?) {}
  @available(*, deprecated, message: "PLAIN")
  func tell(_ n: Int) {}
}

func needsConversion(_ a: NeedsConversion) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tell(1)
  // Only the '@oneway' overload takes 'nil'
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(nil)
  // The 'await' preference falls back to the '@oneway' overload
  // expected-warning@+2:11{{deprecated: ONEWAY}}
  // expected-warning@+1:11{{'@oneway' instance method 'tell' is not intended to be awaited on, use 'send' instead}}
  await a.tell(nil)
}

// The usual rules would pick the non-generic overload
actor GenericOneway {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func put<T: Sendable>(_ v: T) {}
  @available(*, deprecated, message: "PLAIN")
  func put(_ v: Int) {}
}

func genericOneway(_ a: GenericOneway) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.put(1)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.put("s")
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.put(1)
  // expected-warning@+2:11{{deprecated: ONEWAY}}
  // expected-warning@+1:11{{'@oneway' instance method 'put' is not intended to be awaited on, use 'send' instead}}
  await a.put("s")
}

// The usual rules would pick the overload which needs no default argument
actor DefaultArgument {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func example(a: Int = 32) {}
  @available(*, deprecated, message: "PLAIN")
  func example() {}
}

func defaultArgument(_ a: DefaultArgument) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.example()
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.example(a: 1)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.example()
}

// ==== -----------------------------------------------------------------------
// MARK: No '@oneway' overload matches, the usual rules pick among all of them

actor Fallback {
  // Argument type
  @available(*, deprecated, message: "ONEWAY")
  @oneway func log(_ s: String) {}
  @available(*, deprecated, message: "PLAIN")
  func log(_ n: Int) async {}

  // Argument label
  @available(*, deprecated, message: "ONEWAY")
  @oneway func move(to x: Int) {}
  @available(*, deprecated, message: "PLAIN")
  func move(by x: Int) {}

  // Number of arguments
  @available(*, deprecated, message: "ONEWAY")
  @oneway func ping() {}
  @available(*, deprecated, message: "PLAIN")
  func ping(_ n: Int) {}

  // Generic requirement
  @available(*, deprecated, message: "ONEWAY")
  @oneway func store<T: StringProtocol & Sendable>(_ v: T) {}
  @available(*, deprecated, message: "PLAIN")
  func store(_ v: Int) {}

  // A literal which only one overload can take. Disjunction pruning does not
  // rule out the '@oneway' overload since the literal's type is not known yet,
  // the '@oneway' overload fails while solving
  @available(*, deprecated, message: "ONEWAY")
  @oneway func set(_ s: String) {}
  @available(*, deprecated, message: "PLAIN")
  func set(_ n: Int) {}

  // A closure argument of a different arity
  @available(*, deprecated, message: "ONEWAY")
  @oneway func on(_ f: @escaping @Sendable (Int) -> Void) {}
  @available(*, deprecated, message: "PLAIN")
  func on(_ f: @escaping @Sendable (Int, Int) -> Void) {}
}

func fallback(_ a: Fallback, n: Int) async {
  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.log(1)
  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.log(n)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.log("x")

  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.move(by: 1)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.move(to: 1)

  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.ping(1)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.ping()

  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.store(1)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.store("x")

  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.set(1)
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.set("x")

  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.on { _, _ in }
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.on { _ in }
}

// An unavailable '@oneway' overload is skipped
actor UnavailableOneway {
  @available(*, unavailable)
  @oneway func stop() {}
  @available(*, deprecated, message: "PLAIN")
  func stop(_ code: Int = 0) {}
}

func unavailableOneway(_ a: UnavailableOneway) async {
  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.stop()
  // An explicit argument still only matches the non-'@oneway' overload
  // expected-warning@+1:10{{deprecated: PLAIN}}
  send a.stop(1)
}

// ==== -----------------------------------------------------------------------
// MARK: Several '@oneway' overloads

// The usual rules pick among the '@oneway' overloads
actor ManyOneway {
  @available(*, deprecated, message: "ONEWAY_INT")
  @oneway func log(_ n: Int) {}
  @available(*, deprecated, message: "ONEWAY_DOUBLE")
  @oneway func log(_ d: Double) {}
  @available(*, deprecated, message: "PLAIN")
  func log(_ n: Int) async {}
}

func manyOneway(_ a: ManyOneway) async {
  // expected-warning@+1:10{{deprecated: ONEWAY_INT}}
  send a.log(1)
  // expected-warning@+1:10{{deprecated: ONEWAY_DOUBLE}}
  send a.log(1.5)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.log(1)
}

// Ambiguous '@oneway' overloads are an error, even though an overload which
// is not '@oneway' would match exactly: it is not attempted
actor AmbiguousOneway {
  @oneway func f(_ n: Int, _ s: String = "") {} // expected-note@:16{{found this candidate}}
  @oneway func f(_ n: Int, _ b: Bool = false) {} // expected-note@:16{{found this candidate}}
  @available(*, deprecated, message: "PLAIN")
  func f(_ n: Int) {}
}

func ambiguousOneway(_ a: AmbiguousOneway) async {
  // expected-error@+1:10{{ambiguous use of 'f'}}
  send a.f(1)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.f(1)
}

// Both '@oneway' overloads lead to a solution and the usual rules pick
// between them: the operand of a 'send' is type-checked in an async closure,
// which prefers the 'async' overload
actor OnewaySyncAndAsync {
  @available(*, deprecated, message: "ONEWAY_SYNC")
  @oneway func tick() {}
  @available(*, deprecated, message: "ONEWAY_ASYNC")
  @oneway func tick() async {}
  @available(*, deprecated, message: "PLAIN")
  func tick(_ n: Int = 0) {}
}

func onewaySyncAndAsync(_ a: OnewaySyncAndAsync) async {
  // expected-warning@+1:10{{deprecated: ONEWAY_ASYNC}}
  send a.tick()
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tick()
}

// ==== -----------------------------------------------------------------------
// MARK: Overloaded arguments

// The '@oneway' preference only holds for the solver path which attempts the
// callee. If the solver attempted 'make()' first, both ('make() -> Int',
// PLAIN) and ('make() -> String', ONEWAY) would lead to a solution and nothing
// would rank one above the other. The callee has to be attempted first, which
// the favored '@oneway' choices make likely, but not certain
func make() -> Int { 0 }
func make() -> String { "" }

actor OverloadedArgument {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func tell(_ s: String) {}
  @available(*, deprecated, message: "PLAIN")
  func tell(_ n: Int) {}
}

func overloadedArgument(_ a: OverloadedArgument) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(make())
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tell(make())
}

// The argument of a 'send' call is not itself the operand of a 'send'
actor Inner {
  @available(*, deprecated, message: "INNER_ONEWAY")
  @oneway func value() {}
  @available(*, deprecated, message: "INNER_PLAIN")
  func value() -> Int { 0 }
}

func innerCall(_ a: Basic, _ i: isolated Inner) {
  // expected-warning@+2:10{{deprecated: ONEWAY}}
  // expected-warning@+1:17{{deprecated: INNER_PLAIN}}
  send a.tell(i.value())
}

// ==== -----------------------------------------------------------------------
// MARK: Call sites

actor CallSites {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func tell(_ n: Int) {}
  @available(*, deprecated, message: "PLAIN")
  func tell(_ n: Int) async {}

  func insideAsync() async {
    // expected-warning@+1:15{{deprecated: ONEWAY}}
    send self.tell(1)
    // Implicit 'self'
    // expected-warning@+1:10{{deprecated: ONEWAY}}
    send tell(1)
    // expected-warning@+1:16{{deprecated: PLAIN}}
    await self.tell(1)
  }

  func insideSync() {
    // expected-warning@+1:15{{deprecated: ONEWAY}}
    send self.tell(1)
    // A plain call: the usual rules, the 'async' overload cannot be called
    // here
    // expected-warning@+1:10{{deprecated: ONEWAY}}
    self.tell(1)
  }
}

@MainActor func fromMainActor(_ a: CallSites) {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
}

func optionalChain(_ a: CallSites?) async {
  // expected-warning@+1:11{{deprecated: ONEWAY}}
  send a?.tell(1)
  // expected-warning@+1:12{{deprecated: PLAIN}}
  await a?.tell(1)
}

func tryAwait(_ a: CallSites) async throws {
  // expected-warning@+2:15{{deprecated: PLAIN}}
  // expected-warning@+1:3{{no calls to throwing functions occur within 'try' expression}}
  try await a.tell(1)
}

// A '@oneway' overload declared by a protocol
protocol Teller: Actor {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func tell(_ n: Int)
  @available(*, deprecated, message: "PLAIN")
  func tell(_ n: Int) async
}

func generic<T: Teller>(_ a: T) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tell(1)
}

func existential(_ a: any Teller) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tell(1)
}

// Global actor isolated free functions
@available(*, deprecated, message: "ONEWAY")
@MainActor @oneway func announce(a: Int = 32) {}
@available(*, deprecated, message: "PLAIN")
@MainActor func announce() {}

@MainActor func mainActorFreeFunctions() {
  // expected-warning@+1:8{{deprecated: ONEWAY}}
  send announce()
  // expected-warning@+1:3{{deprecated: PLAIN}}
  announce()
}

func nonisolatedFreeFunctions() async {
  // expected-warning@+1:8{{deprecated: ONEWAY}}
  send announce()
  // expected-warning@+1:9{{deprecated: PLAIN}}
  await announce()
}

// ==== -----------------------------------------------------------------------
// MARK: Where 'await' and plain calls depend on the 'await' preference

// '@oneway func f() async' and 'func f()': in an async context the usual rules
// prefer the 'async' overload, which here is the '@oneway' one
actor OnewayIsAsync {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func tell(_ n: Int) async {}
  @available(*, deprecated, message: "PLAIN")
  func tell(_ n: Int) {}

  func insideAwait() async {
    // With the 'await' preference
    // expected-warning@+2:16{{deprecated: PLAIN}}
    // expected-warning@+1:5{{no 'async' operations occur within 'await' expression}}
    await self.tell(1)
  }

  func insidePlainAsync() async {
    // Not a 'send' or an 'await' call, so the usual rules pick the 'async'
    // overload. Today's scoring picks PLAIN and this compiles.
    // expected-warning@+3:10{{deprecated: ONEWAY}}
    // expected-error@+2:5{{expression is 'async' but is not marked with 'await'}}
    // expected-note@+1:5{{call is 'async'}}
    self.tell(1)
  }

  func insidePlainSync() {
    // expected-warning@+1:10{{deprecated: PLAIN}}
    self.tell(1)
  }
}

func onewayIsAsync(_ a: OnewayIsAsync) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.tell(1)
  // With the 'await' preference
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.tell(1)
}

// Same 'async'-ness: the usual rules pick the overload which needs no default
// argument, the '@oneway' one
actor SameAsyncness {
  @available(*, deprecated, message: "ONEWAY")
  @oneway func log(_ s: String) {}
  @available(*, deprecated, message: "PLAIN")
  func log(_ s: String, level: Int = 0) {}
}

func sameAsyncness(_ a: SameAsyncness) async {
  // expected-warning@+1:10{{deprecated: ONEWAY}}
  send a.log("x")
  // With the 'await' preference
  // expected-warning@+1:11{{deprecated: PLAIN}}
  await a.log("x")
}

// ==== -----------------------------------------------------------------------
// MARK: No '@oneway' overload, the usual rules

// A 'send' call does not prefer an overload which returns 'Void' over one
// which returns a value: neither is '@oneway', and the operand of a 'send' has
// no contextual type
actor Sendability {
  func pick() {} // expected-note@:8{{found this candidate}}
  @discardableResult func pick() -> Int { 0 } // expected-note@:27{{found this candidate}}
}

func sendability(_ a: Sendability) async {
  // expected-error@+1:10{{ambiguous use of 'pick()'}}
  send a.pick()
}
