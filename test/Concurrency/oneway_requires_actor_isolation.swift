// RUN: %target-typecheck-verify-swift -swift-version 6 -target %target-swift-6.2-abi-triple -enable-experimental-feature SendCalls

// REQUIRES: concurrency
// REQUIRES: swift_feature_SendCalls

// A '@oneway' function must be actor isolated: to an actor instance, also
// through an 'isolated' parameter, or to a global actor. A 'send' call is
// enqueued on the callee's actor, so a call through a protocol requirement
// which is not actor isolated cannot be sent either, whether it is made on a
// concrete type, an existential or a generic parameter

struct Mail: Sendable {}

// ==== -----------------------------------------------------------------------
// MARK: Declarations which are not actor isolated

protocol Mailman {
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway func deliver(_ mail: Mail)
  func plainDeliver(_ mail: Mail)
}

final class S1: Mailman, Sendable {
  func deliver(_ mail: Mail) {}
  func plainDeliver(_ mail: Mail) {}
}

actor ActorMailman: Mailman {
  nonisolated func deliver(_ mail: Mail) {}
  nonisolated func plainDeliver(_ mail: Mail) {}
}

class Bad {
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway func test() {}
}

actor GreatActor {
  @oneway func test() {}
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway static func staticTest() {}
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway nonisolated func nonisolatedTest() {}
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway nonisolated(nonsending) func nonsendingTest() async {}
  // expected-error@+1:3{{'@oneway' can only be applied to an actor isolated method}}
  @oneway @concurrent func concurrentTest() async {}

  @MainActor @oneway static func mainActorStaticTest() {}
}

// expected-error@+1:1{{'@oneway' can only be applied to an actor isolated method}}
@oneway func globalTest() {}

// '@oneway(never)' only dissuades from 'send', it does not require isolation
@oneway(never) func globalNeverTest() {}

class NeverExample {
  @oneway(never) func test() {}
}

// ==== -----------------------------------------------------------------------
// MARK: 'send' through a requirement which is not actor isolated

func s1(m: any Mailman & Sendable) {
  send m.deliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
  send m.plainDeliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
}

func s1g<M: Mailman & Sendable>(m: M) {
  send m.deliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
  send m.plainDeliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
}

func s1s(m: some Mailman & Sendable) {
  send m.deliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
}

func s1c(c: S1, a: ActorMailman) {
  send c.deliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
  send a.deliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
}

func s1o(m: (any Mailman & Sendable)?) {
  send m?.deliver(Mail()) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
}

// ==== -----------------------------------------------------------------------
// MARK: Actor isolated declarations

protocol LocalWorker: Actor {
  @oneway func stop()
  func flush()
}

actor PlainWorker: LocalWorker {
  func stop() {}
  func flush() {}
}

@MainActor
protocol MainWorker {
  @oneway func stop()
  func flush()
}

final class MainThing: MainWorker {
  func stop() {}
  func flush() {}
}

class Example {
  @oneway func testIsolated(_ actor: isolated GreatActor) {}
}

@MainActor @oneway func mainActorTest() {}

func positive(
  local: any LocalWorker, main: any MainWorker, example: Example & Sendable,
  great: GreatActor, optional: (any LocalWorker)?
) {
  send local.stop()
  send local.flush()
  send optional?.stop()
  send main.stop()
  send main.flush()
  send great.test()
  send mainActorTest()
  send GreatActor.mainActorStaticTest()
}

func positiveGeneric<L: LocalWorker, M: MainWorker>(local: L, main: M) {
  send local.stop()
  send main.stop()
}

func positiveSome(local: some LocalWorker, main: some MainWorker) {
  send local.stop()
  send main.flush()
}

func positiveIsolated(example: Example, great: isolated GreatActor) {
  send example.testIsolated(great)
}

// ==== -----------------------------------------------------------------------
// MARK: Function values

// '@oneway' is a declaration attribute, a function value is sent like any
// call: only if its function type is isolated to an actor

func takesMainActor(_ f: @escaping @MainActor (Int) -> Void) {
  send f(1)
}

func takesIsolatedParameter(
  _ f: @escaping @Sendable (isolated GreatActor, Int) -> Void,
  _ a: GreatActor
) {
  send f(a, 1)
}

func positiveFunctionValues(great: GreatActor) {
  let plainMain: @MainActor (Int) -> Void = { _ in }
  send plainMain(1)
  let plainIsolated: @Sendable (isolated GreatActor, Int) -> Void = { _, _ in }
  send plainIsolated(great, 1)
}

func negativeFunctionValues(
  sendable: @escaping @Sendable (Int) -> Void,
  isolatedAny: @escaping @isolated(any) (Int) -> Void,
  concurrent: @escaping @concurrent (Int) async -> Void
) {
  let plain: (Int) -> Void = { _ in }
  send plain(1) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
  send sendable(1) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
  send isolatedAny(1) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
  send concurrent(1) // expected-error@:3{{'send' can only be applied to calls of actor isolated methods}}
}

extension GreatActor {
  func sendLater() {
    // A partially applied '@oneway' method is a '() -> ()' value, whose
    // isolation to 'self' is not part of its type, so it cannot be sent
    let test = self.test
    send test() // expected-error@:5{{'send' can only be applied to calls of actor isolated methods}}
    send self.test()
  }
}
