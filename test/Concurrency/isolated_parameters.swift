// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete %s -emit-sil -o /dev/null -verify

// REQUIRES: asserts
// REQUIRES: concurrency
// REQUIRES: swift_swift_parser

@available(SwiftStdlib 5.1, *)
actor A {
  func f() { }
}

@available(SwiftStdlib 5.1, *)
extension Actor {
  func g() { }
}

@MainActor func mainActorFn() {}

actor Other {
  func inc() {}
  func pass(_ f: @Sendable  (Self) async -> Void) {}
}
actor Counter {
  func inc() {}
  func pass(_ f: @Sendable  (Self) async -> Void) {}
}

@available(SwiftStdlib 5.1, *)
typealias Fn = (isolated A) -> Void

@available(SwiftStdlib 5.1, *)
func globalFunc(_ a: A) { }

@available(SwiftStdlib 5.1, *)
func testIsolatedParamCallsAsync(a: isolated A, b: A) async {
  globalFunc(a)
  globalFunc(b)

}

@available(SwiftStdlib 5.1, *)
func testIsolatedParamCaptures(a: isolated A) async {
  let _ = {
    a.f()
  }
}

actor MyActor {
  func hello() {}
}

// Compiler >= 5.3 is ended to suppress the parser error
typealias MyFnFixed = (_: isolated MyActor) -> Void

func standalone(_: isolated MyActor) {}

func check() {
  let _: MyFnFixed = standalone
  let _: MyFnFixed = { (_: isolated MyActor) in () }
}


@available(SwiftStdlib 5.1, *)
protocol P {
  func f(isolated: MyActor) async
  func g(isolated x: MyActor) async
  func h(isolated MyActor: isolated MyActor)
  func i(isolated: isolated MyActor)
  func j(isolated: Int) -> Int
  func k(isolated y: Int) -> Int
  func l(isolated _: Int) -> Int
  func m(thing: isolated MyActor)
}

@available(SwiftStdlib 5.1, *)
struct S: P {
  func f(isolated: MyActor) async { await isolated.hello() }
  func g(isolated x: MyActor) async { await x.hello() }
  func h(isolated MyActor: isolated MyActor) { i(isolated: MyActor) }
  func i(isolated: isolated MyActor) { isolated.hello() }
  func j(isolated: Int) -> Int { return isolated }
  func k(isolated y: Int) -> Int { return j(isolated: y) }
  func l(isolated _: Int) -> Int { return k(isolated: 0) }
  func m(thing: MyActor) { }
}

func checkConformer(_ s: S, _ p: any P, _ ma: MyActor) async {
  s.m(thing: ma)
  await p.m(thing: ma)
  // expected-warning @-1 {{sending 'p' risks causing data races}}
  // expected-note @-2 {{sending task-isolated 'p' to actor-isolated instance method 'm(thing:)' risks causing data races between actor-isolated and task-isolated uses}}
}

// Redeclaration checking
actor TestActor {
  func test() {
  }
}

func tuplify<Ts>(_ fn: (Ts) -> Void) {}

// Inference of "isolated" on closure parameters.
@available(SwiftStdlib 5.1, *)
func testIsolatedClosureInference(one: A, two: A) async {
  let _: (isolated A) -> Void = {
    $0.f()
  }

  let _: (isolated A) -> Void = {
    $0.f()
  }

  let _: (isolated A) -> Void = { a2 in
    a2.f()
  }

  let _: (isolated A) -> Void = { (a2: isolated A) in
    a2.f()
  }
}

@available(SwiftStdlib 5.1, *)
func checkIsolatedAndGlobalClosures(_ a: A) {
  let _: @MainActor (isolated A) -> Void // expected-warning {{function type cannot have global actor and 'isolated' parameter; this is an error in the Swift 6 language mode}}
      = {
    $0.f()
  }

  let _: @MainActor (isolated A) -> Void // expected-warning {{function type cannot have global actor and 'isolated' parameter; this is an error in the Swift 6 language mode}}
      = { @MainActor in // expected-warning {{closure with 'isolated' parameter '$0' cannot have a global actor; this is an error in the Swift 6 language mode}}{{11-22=}}
    $0.f()
    mainActorFn()
  }
}

// "isolated" existential parameters.
protocol P2: Actor {
  func m()
}

@available(SwiftStdlib 5.1, *)
func testExistentialIsolated(a: isolated P2, b: P2) async {
  a.m()
  await b.m()
}

// "isolated" parameters of closures make the closure itself isolated.
extension TestActor {
  func isolatedMethod() { }
 

  // expected-warning@+1 {{instance method with 'isolated' parameter cannot be 'nonisolated'; this is an error in the Swift 6 language mode}}{{3-15=}}
  nonisolated func isolatedToParameter(_ other: isolated TestActor) {
    other.isolatedMethod()
  }
}

@available(SwiftStdlib 5.1, *)
func isolatedClosures() {
  let _: (isolated TestActor) -> Void = { (ta: isolated TestActor) in
    ta.isolatedMethod() // okay, isolated to ta

    _ = {
      ta.isolatedMethod() // okay, isolated to ta
    }
  }
}

// Test case for https://github.com/apple/swift/issues/62568
func execute<ActorType: Actor>(
  on isolatedActor: isolated ActorType,
  task: @escaping @Sendable (isolated ActorType) -> Void)
{
  // Compiler correctly allows this task to execute synchronously.
  task(isolatedActor)
  // Start a task that inherits the current execution context (i.e. that of the isolatedActor)
  Task {
    // 'await' is not not necessary because 'task' is synchronous.
    task(isolatedActor)
  }
}

actor ProtectsDictionary {
  var dictionary: [String: String] = ["A": "B"]
}

func getValues(
  forKeys keys: [String],
  from actor: isolated ProtectsDictionary
) -> [String?] {
  // A non-escaping, synchronous closure cannot cross isolation
  // boundaries; it should be isolated to 'actor'.
  keys.map { key in
    actor.dictionary[key]
  }
}

func isolated_generic_ok_1<T: Actor>(_ t: isolated T) {}


class NotSendable {}

func optionalIsolated(_ ns: NotSendable, to actor: isolated (any Actor)?) async {}
func optionalIsolatedSync(_ ns: NotSendable, to actor: isolated (any Actor)?) {}

nonisolated func callFromNonisolated(ns: NotSendable) async {
  await optionalIsolated(ns, to: nil)

  optionalIsolatedSync(ns, to: nil)

  let myActor = A()

  await optionalIsolated(ns, to: myActor)
  // expected-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-note @-2 {{sending task-isolated 'ns' to actor-isolated global function 'optionalIsolated(_:to:)' risks causing data races between actor-isolated and task-isolated uses}}
}

@MainActor func callFromMainActor(ns: NotSendable) async {
  await optionalIsolated(ns, to: nil)
  // expected-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-note @-2 {{sending main actor-isolated 'ns' to nonisolated global function 'optionalIsolated(_:to:)' risks causing data races between nonisolated and main actor-isolated uses}}

  optionalIsolatedSync(ns, to: nil)

  let myActor = A()

  await optionalIsolated(ns, to: myActor)
  // expected-warning @-1 {{sending 'ns' risks causing data races}}
  // expected-note @-2 {{sending main actor-isolated 'ns' to actor-isolated global function 'optionalIsolated(_:to:)' risks causing data races between actor-isolated and main actor-isolated uses}}

}

// TODO: Consider making an actor's Self behave like in a struct, removing this special casing.
//       We could consider changing this, so that self is always Self because we don't allow inheritance of actors.
//       See: https://github.com/apple/swift/issues/70954 and rdar://121091417
actor A2 {
  nonisolated func f1() async {
  }
  nonisolated func f2() async -> Self {
    await { (self: isolated Self) in }(self)
    await { (self: isolated Self?) in }(self)
    return self
  }
}

func testNonSendableCaptures(ns: NotSendable, a: isolated MyActor) {
  Task {
    _ = a
    _ = ns
  }

  // FIXME: The `a` in the capture list and `isolated a` are the same,
  // but the actor isolation checker doesn't know that.
  Task { [a] in // expected-warning {{passing closure as a 'sending' parameter risks causing data races between 'a'-isolated code and concurrent execution of the closure}}
    _ = a
    _ = ns // expected-note {{closure captures 'a'-isolated 'ns'}}
  }
}


@globalActor actor MyGlobal {
  static let shared = MyGlobal()
}

func sync(isolatedTo actor: isolated (any Actor)?) {}

func pass(value: NotSendable, isolation: isolated (any Actor)?) async -> NotSendable {
  value
}

func preciseIsolated(a: isolated MyActor) async {
  sync(isolatedTo: a)
  sync(isolatedTo: nil) // okay from anywhere
  sync(isolatedTo: #isolation)

  Task { @MainActor in
    sync(isolatedTo: MainActor.shared)
    sync(isolatedTo: nil) // okay from anywhere
    sync(isolatedTo: #isolation)
  }

  Task { @MyGlobal in
    sync(isolatedTo: MyGlobal.shared)
    sync(isolatedTo: nil) // okay from anywhere
    sync(isolatedTo: #isolation)
  }

  Task.detached {
    sync(isolatedTo: nil) // okay from anywhere
    sync(isolatedTo: #isolation)
  }
}

func testLValueIsolated() async {
  var a = A() // expected-warning {{variable 'a' was never mutated}}
  await sync(isolatedTo: a)
}

@MainActor func fromMain(ns: NotSendable) async -> NotSendable {
  await pass(value: ns, isolation: MainActor.shared)
}

nonisolated func fromNonisolated(ns: NotSendable) async -> NotSendable {
  await pass(value: ns, isolation: nil)
}

public func useDefaultIsolation(
  _ isolation: isolated (any Actor)? = #isolation
) {}

public func useDefaultIsolationWithoutIsolatedParam(
  _ isolation: (any Actor)? = #isolation
) {}

@MainActor func callUseDefaultIsolation() async {
  useDefaultIsolation()
  useDefaultIsolationWithoutIsolatedParam()
}

public actor MyActorIsolatedParameterMerge {
  private var inProgressIndexTasks: [Int: Int] = [:]

  public func test() async {
    await withTaskGroup(of: Void.self) { taskGroup in
      for (_, _) in inProgressIndexTasks {}
      await taskGroup.waitForAll()
    }
  }
}

// rdar://138394497
class ClassWithIsolatedAsyncInitializer {
    init(isolation: isolated (any Actor)? = #isolation) async {}
}

// https://github.com/swiftlang/swift/issues/80992
struct WritableActorKeyPath<Root: Actor, Value>: Sendable {
    var getter: @Sendable (isolated Root) -> Value
    var setter: @Sendable (isolated Root, Value) -> Void

    subscript(_ root: isolated Root) -> Value {
        get { getter(root) }
        nonmutating set { setter(root, newValue) }
    }
}
