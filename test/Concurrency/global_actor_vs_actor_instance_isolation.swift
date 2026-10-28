// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -parse-as-library -emit-sil -o /dev/null -verify -swift-version 5
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -parse-as-library -emit-sil -o /dev/null -verify -swift-version 5 -strict-concurrency=complete
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -parse-as-library -emit-sil -o /dev/null -verify -swift-version 6

// REQUIRES: concurrency

// Isolation to global actor 'MyActor' is the specific instance 'MyActor.shared'
// but you are technically able to instantiate other instances, so we reject
// some code in surprising cases and confuse people. Test the cases where we can
// emit a slightly more elucidating note.

@globalActor
actor MyActor {
  static let shared = MyActor()

  var state = 0
  // expected-note@-1:7 {{property declared here}}
  // expected-note@-2:7 {{mutation of this property is only permitted within the actor}}

  func foo() {}
  // expected-note@-1:8 10 {{calls to instance method 'foo()' from outside of its actor context are implicitly asynchronous}}

  func callsGlobalActorFunction() {
    globalActorFunction()
    // expected-error@-1:5 {{call to global actor 'MyActor'-isolated global function 'globalActorFunction()' in a synchronous actor-isolated context}}
    // expected-note@-2:5 {{'self' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func callsGlobalActorFunctionAsync() async {
    globalActorFunction()
    // expected-error@-1:5 {{global actor 'MyActor'-isolated global function 'globalActorFunction()' cannot be called from outside of the actor}}
    // expected-note@-2:5 {{'self' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func readsGlobalActorVariableAsync() async {
    _ = globalActorVariable
    // expected-error@-1:9 {{global actor 'MyActor'-isolated var 'globalActorVariable' cannot be accessed from outside of the actor}}
    // expected-note@-2:9 {{'self' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func readsGlobalActorStatic() {
    _ = GlobalActorStatics.value
    // expected-error@-1:28 {{global actor 'MyActor'-isolated static property 'value' can not be referenced on a different actor instance}}
    // expected-note@-2:28 {{'self' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func globalActorClosure() {
    let _: @MyActor () -> Void = { self.foo() }
    // expected-error@-1:41 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
    // expected-note@-2:36 {{'self' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }
}

@MyActor func globalActorFunction() {}
// expected-note@-1:15 2 {{calls to global function 'globalActorFunction()' from outside of its actor context are implicitly asynchronous}}

@MyActor var globalActorVariable = 0

enum GlobalActorStatics {
  @MyActor static var value = 0
  // expected-note@-1:23 {{static property declared here}}
}

func takesIsolated(_ a: isolated MyActor) {}
// expected-note@-1:6 {{calls to global function 'takesIsolated' from outside of its actor context are implicitly asynchronous}}

func makeActor() -> MyActor { MyActor() }

@MyActor
struct MyStruct {
  let instance: MyActor
  let optionalInstance: MyActor?

  func call() {
    instance.foo()
    // expected-error@-1:14 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
    // expected-note@-2:5 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func callThroughSelf() {
    self.instance.foo()
    // expected-error@-1:19 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
    // expected-note@-2:5 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func callThroughOptional() {
    optionalInstance?.foo()
    // expected-error@-1:23 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
    // expected-note@-2:5 {{'optionalInstance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func callOnUnnamedInstance() {
    makeActor().foo()
    // expected-error@-1:17 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
    // expected-note@-2:5 {{the actor instance may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func read() {
    _ = instance.state
    // expected-error@-1:18 {{actor-isolated property 'state' can not be referenced from global actor 'MyActor'}}
    // expected-note@-2:9 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func mutate() {
    instance.state = 1
    // expected-error@-1:14 {{actor-isolated property 'state' can not be mutated from global actor 'MyActor'}}
    // expected-note@-2:5 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func passIsolated() {
    // TODO: Not an incredible diagnostic...
    takesIsolated(instance)
    // expected-error@-1:5 {{call to actor-isolated global function 'takesIsolated' in a synchronous global actor 'MyActor'-isolated context}}
    // expected-note@-2:19 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func passSharedIsolated() {
    takesIsolated(MyActor.shared)
  }

  // TODO: Accept; 'MyActor.shared' is the global actor, as in
  // 'passSharedIsolated()'.
  func callShared() {
    MyActor.shared.foo()
    // expected-error@-1:20 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
  }

  func callAsync() async {
    instance.foo()
    // expected-error@-1:14 {{actor-isolated instance method 'foo()' cannot be called from outside of the actor}}
    // expected-note@-2:5 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func readAsync() async {
    _ = instance.state
    // expected-error@-1:18 {{actor-isolated property 'state' cannot be accessed from outside of the actor}}
    // expected-note@-2:9 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  func readTwiceAsync() async {
    _ = instance.state + instance.state
    // expected-error@-1:9 {{expression is 'async' but is not marked with 'await'}}
    // expected-note@-2:9 {{property access is 'async'}}
    // expected-note@-3:26 {{property access is 'async'}}
    // expected-note@-4:9 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
    // expected-note@-5:26 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
  }

  // TODO: This can be accepted with .shared special casing / preventing
  // multiple instances of actors used as global actors...
  func callSharedAsync() async {
    // Test that we don't emit the note here, since it isn't true!
    MyActor.shared.foo()
    // expected-error@-1:20 {{actor-isolated instance method 'foo()' cannot be called from outside of the actor}}
  }
}

@MyActor func closure(_ instance: MyActor) {
  let _: @MyActor () -> Void = { instance.foo() }
  // expected-error@-1:43 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
  // expected-note@-2:34 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
}

@MyActor func closureAsync(_ instance: MyActor) {
  let _: @MyActor () async -> Void = { instance.foo() }
  // expected-error@-1:49 {{actor-isolated instance method 'foo()' cannot be called from outside of the actor}}
  // expected-note@-2:40 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
}

func isolatedParameterContext(_ instance: isolated MyActor) {
  globalActorFunction()
  // expected-error@-1:3 {{call to global actor 'MyActor'-isolated global function 'globalActorFunction()' in a synchronous actor-isolated context}}
  // expected-note@-2:3 {{'instance' may be a different instance of 'MyActor' than global actor 'MyActor'}}
}

// I hope nobody ever writes code like this...
extension MainActor {
  func mainActorInstanceMethod() {}
  // expected-note@-1:8 2 {{calls to instance method 'mainActorInstanceMethod()' from outside of its actor context are implicitly asynchronous}}

  // TODO: Accept; every 'MainActor' is the main actor.
  func callsMainActorFunction() {
    mainActorFunction()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFunction()' in a synchronous actor-isolated context}}
  }
}

@MainActor func mainActorFunction() {}
// expected-note@-1:17 {{calls to global function 'mainActorFunction()' from outside of its actor context are implicitly asynchronous}}

// We could consider special casing MainActor to accept these cases in theory,
// but we probably shouldn't encourage extensions on MainActor.
@MainActor func mainActor(_ instance: MainActor) {
  instance.mainActorInstanceMethod()
  // expected-error@-1:12 {{call to actor-isolated instance method 'mainActorInstanceMethod()' in a synchronous main actor-isolated context}}
}

@globalActor
struct GlobalActorOfMainActor {
  static let shared = MainActor.shared
}

@GlobalActorOfMainActor func mainActorInstance(_ instance: MainActor) {
  instance.mainActorInstanceMethod()
  // expected-error@-1:12 {{call to actor-isolated instance method 'mainActorInstanceMethod()' in a synchronous global actor 'GlobalActorOfMainActor'-isolated context}}
}

actor OtherActor {
  func foo() {}
  // expected-note@-1:8 {{calls to instance method 'foo()' from outside of its actor context are implicitly asynchronous}}
}

@MyActor func otherActor(_ other: OtherActor) {
  other.foo()
  // expected-error@-1:9 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'MyActor'-isolated context}}
}

protocol ActorProtocol: Actor {}

extension ActorProtocol {
  func protocolMethod() {}
  // expected-note@-1:8 {{calls to instance method 'protocolMethod()' from outside of its actor context are implicitly asynchronous}}
}

@MyActor func generic<A: ActorProtocol>(_ actor: A) {
  actor.protocolMethod()
  // expected-error@-1:9 {{call to actor-isolated instance method 'protocolMethod()' in a synchronous global actor 'MyActor'-isolated context}}
}

// Struct global actor with a different name from its ActorType.
@globalActor
struct GlobalActorOfMyActor {
  static let shared = MyActor()
}

@GlobalActorOfMyActor func sharedInstanceOfOtherType(_ instance: MyActor) {
  instance.foo()
  // expected-error@-1:12 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'GlobalActorOfMyActor'-isolated context}}
  // expected-note@-2:3 {{'instance' may be a different instance of 'MyActor' than global actor 'GlobalActorOfMyActor'}}

  // This must not be accepted via shared special casing since MyActor.shared
  // and GlobalActorOfMyActor.shared may differ... only a rule about only one
  // instance when used as a global actor would do it. But I don't think people
  // do this...
  MyActor.shared.foo()
  // expected-error@-1:18 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'GlobalActorOfMyActor'-isolated context}}
  // expected-note@-2:3 {{'shared' may be a different instance of 'MyActor' than global actor 'GlobalActorOfMyActor'}}

  // TODO: we could accept this.
  GlobalActorOfMyActor.shared.foo()
  // expected-error@-1:31 {{call to actor-isolated instance method 'foo()' in a synchronous global actor 'GlobalActorOfMyActor'-isolated context}}
}

// ActorType of 'ActorWithOtherSharedInstance' is not an
// 'ActorWithOtherSharedInstance'.
@globalActor
actor ActorWithOtherSharedInstance {
  static let shared = MyActor()

  func bar() {}
  // expected-note@-1:8 {{calls to instance method 'bar()' from outside of its actor context are implicitly asynchronous}}
}

// Since actors don't support inheritance I think these can never be the same
// instance and the note would be inappropriate.
@ActorWithOtherSharedInstance
func otherSharedInstance(_ instance: ActorWithOtherSharedInstance) {
  instance.bar()
  // expected-error@-1:12 {{call to actor-isolated instance method 'bar()' in a synchronous global actor 'ActorWithOtherSharedInstance'-isolated context}}
}
