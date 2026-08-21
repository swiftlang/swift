// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix minimal-
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix complete- -strict-concurrency=complete
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- -language-mode 6
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- -language-mode 6 -enable-experimental-feature NoExplicitNonIsolated
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- -language-mode 6 -enable-upcoming-feature NonisolatedNonsendingByDefault
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple %s -emit-sil -o /dev/null -verify -verify-additional-prefix swift6- -language-mode 6 -default-isolation MainActor

// REQUIRES: concurrency
// REQUIRES: swift_feature_NoExplicitNonIsolated
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

@MainActor
func mainActorFn() {}
// expected-note@-1:6 9 {{calls to global function 'mainActorFn()' from outside of its actor context are implicitly asynchronous}}

@MainActor
protocol MainActorEmptyProto {}

nonisolated
protocol NonisolatedInstanceReq {
  func nonisolatedInstanceMethod()
}

@MainActor
protocol MainActorInstanceReq {
  func mainActorInstanceMethod()
}

@MainActor
protocol MainActorStaticReq {
  static func mainActorStaticMethod()
}

@MainActor
protocol MainActorAsyncInstanceReq {
  func mainActorAsyncInstanceMethod() async
}

// 'nonisolated' so that '-default-isolation MainActor' does not make it implicitly Sendable.
nonisolated final class NonSendableValue {}
// expected-complete-note@-1:25 {{class 'NonSendableValue' does not conform to the 'Sendable' protocol}}
// expected-swift6-note@-2:25 {{class 'NonSendableValue' does not conform to the 'Sendable' protocol}}

@MainActor
protocol MainActorAsyncNonSendableReq {
  func mainActorAsyncNonSendableMethod(_ value: NonSendableValue) async
}

@MainActor
protocol MainActorPropertyReq {
  var mainActorProperty: Int { get set }
}

@MainActor
protocol MainActorStaticPropertyReq {
  static var mainActorStaticProperty: Int { get }
}

@MainActor
protocol MainActorInitReq {
  init()
}

// Implicitly @MainActor isolated only under '-default-isolation MainActor'.
protocol DefaultIsolatedProto {
  func defaultIsolatedMethod()
}

// A witness that can be reached from the requirement's isolation is fine.
actor NoRequirements: MainActorEmptyProto {}

actor NonisolatedWitness: MainActorInstanceReq {
  nonisolated func mainActorInstanceMethod() {}
}

actor GlobalActorWitness: MainActorInstanceReq {
  @MainActor func mainActorInstanceMethod() {}
}

actor AsyncWitness: MainActorAsyncInstanceReq {
  func mainActorAsyncInstanceMethod() async {} // ok since the requirement is async.
}

// async can't hop between isolations when the params or return is NS.
actor AsyncNonSendableWitness: MainActorAsyncNonSendableReq {
  func mainActorAsyncNonSendableMethod(_ value: NonSendableValue) async {}
  // expected-swift6-error@-1:8 {{non-Sendable parameter type 'NonSendableValue' cannot be sent from caller of protocol requirement 'mainActorAsyncNonSendableMethod' into actor-isolated implementation}}
  // expected-complete-warning@-2:8 {{non-Sendable parameter type 'NonSendableValue' cannot be sent from caller of protocol requirement 'mainActorAsyncNonSendableMethod' into actor-isolated implementation; this is an error in the Swift 6 language mode}}
}

actor ActorIsolatedWitness: MainActorInstanceReq {
  // expected-swift6-error@-1:29 {{conformance of 'ActorIsolatedWitness' to protocol 'MainActorInstanceReq' involves isolation mismatches and can cause data races}}
  // expected-minimal-warning@-2:29 {{conformance of 'ActorIsolatedWitness' to protocol 'MainActorInstanceReq' involves isolation mismatches and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-complete-warning@-3:29 {{conformance of 'ActorIsolatedWitness' to protocol 'MainActorInstanceReq' involves isolation mismatches and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-4:29 {{turn data races into runtime errors with '@preconcurrency'}}
  // TODO: ^ we should not suggest preconcurrency when it is unhelpful.
  func mainActorInstanceMethod() {}
  // expected-note@-1:8 {{actor-isolated instance method 'mainActorInstanceMethod()' cannot satisfy main actor-isolated requirement}}
  // expected-note@-2:8 {{mark instance method 'mainActorInstanceMethod()' 'nonisolated'}}
}

actor BaselineInstanceIsolated: NonisolatedInstanceReq {
  nonisolated func nonisolatedInstanceMethod() {}

  static func staticMethod() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'staticMethod()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }

  func instanceIsActorInstanceIsolated() {
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous actor-isolated context}}
  }
}

actor NoGlobalActorInference: MainActorInstanceReq {
  nonisolated func mainActorInstanceMethod() {}

  static func staticMethod() { // Must not be @MainActor
    // expected-note@-1:15 {{add '@MainActor' to make static method 'staticMethod()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }

  func instanceStaysActorInstanceIsolated() { // Must not be @MainActor
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous actor-isolated context}}
  }
}

// Under -default-isolation MainActor in Swift 6.4, this would have inferred @MainActor onto the protocol which would have been inherited by the actor.
actor DefaultIsolatedConformance: DefaultIsolatedProto {
  nonisolated func defaultIsolatedMethod() {}

  static func staticMethod() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'staticMethod()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }
}

@MainActor func mainActorCaller() {
  BaselineInstanceIsolated.staticMethod()
  NoGlobalActorInference.staticMethod()
  DefaultIsolatedConformance.staticMethod()

  _ = BaselineInstanceIsolated()
  _ = NoGlobalActorInference()
  _ = DefaultIsolatedConformance()
}

nonisolated func nonisolatedCaller() {
  BaselineInstanceIsolated.staticMethod()
  NoGlobalActorInference.staticMethod()
  DefaultIsolatedConformance.staticMethod()

  _ = BaselineInstanceIsolated()
  _ = NoGlobalActorInference()
  _ = DefaultIsolatedConformance()
}

actor StaticWitness: MainActorStaticReq {
  static func mainActorStaticMethod() {
    mainActorFn()
  }

  static func notAWitness() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'notAWitness()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }
}

actor StoredPropertyWitness: MainActorPropertyReq {
  // expected-swift6-error@-1:30 {{conformance of 'StoredPropertyWitness' to protocol 'MainActorPropertyReq' involves isolation mismatches and can cause data races}}
  // expected-minimal-warning@-2:30 {{conformance of 'StoredPropertyWitness' to protocol 'MainActorPropertyReq' involves isolation mismatches and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-complete-warning@-3:30 {{conformance of 'StoredPropertyWitness' to protocol 'MainActorPropertyReq' involves isolation mismatches and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-note@-4:30 {{turn data races into runtime errors with '@preconcurrency'}}
  var mainActorProperty: Int = 0
  // expected-note@-1:7 {{actor-isolated property 'mainActorProperty' cannot satisfy main actor-isolated requirement}}
}

actor NonisolatedPropertyWitness: MainActorPropertyReq {
  nonisolated var mainActorProperty: Int {
    get { 0 }
    set {}
  }
}

actor StaticPropertyWitness: MainActorStaticPropertyReq {
  static var mainActorStaticProperty: Int {
    mainActorFn() // ok, the witness is inferred to be main actor-isolated.
    return 0
  }
}

actor InitWitness: MainActorInitReq {
  init() {}
  // expected-note@-1:3 {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-2:3 {{main actor isolation inferred from conformance to protocol 'MainActorInitReq'}}
}

// TODO: should we allow nonisolated inits under this case? no way to opt out of the global actor isolation, even though nonisolated is fine.
actor NonisolatedInitWitness: MainActorInitReq {
  nonisolated init() {}
  // expected-swift6-error@-1:3 {{'nonisolated' on an actor's synchronous initializer is invalid}}
  // expected-minimal-warning@-2:3 {{'nonisolated' on an actor's synchronous initializer is invalid; this is an error in the Swift 6 language mode}}
  // expected-complete-warning@-3:3 {{'nonisolated' on an actor's synchronous initializer is invalid; this is an error in the Swift 6 language mode}}
  // expected-note@-4:15 {{calls to initializer 'init()' from outside of its actor context are implicitly asynchronous}}
  // expected-note@-5:15 {{main actor isolation inferred from conformance to protocol 'MainActorInitReq'}}
}

nonisolated func makeActorsFromNonisolated() {
  _ = InitWitness()
  // expected-error@-1:7 {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
  _ = NonisolatedInitWitness()
  // expected-error@-1:7 {{call to main actor-isolated initializer 'init()' in a synchronous nonisolated context}}
}

protocol RefinesIsolated: MainActorInstanceReq {}

actor RefinedConformance: RefinesIsolated {
  nonisolated func mainActorInstanceMethod() {}

  static func staticMethod() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'staticMethod()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }
}

@SomeGlobalActor
func someGlobalActorFn() {}
// expected-note@-1:6 {{calls to global function 'someGlobalActorFn()' from outside of its actor context are implicitly asynchronous}}

@SomeGlobalActor
protocol SomeGlobalActorProto {
  func someGlobalActorMethod()
}

actor OtherGlobalActor: SomeGlobalActorProto {
  @SomeGlobalActor func someGlobalActorMethod() {
    someGlobalActorFn()
  }

  static func notAWitness() {
    // expected-note@-1:15 {{add '@SomeGlobalActor' to make static method 'notAWitness()' part of global actor 'SomeGlobalActor'}}
    someGlobalActorFn()
    // expected-error@-1:5 {{call to global actor 'SomeGlobalActor'-isolated global function 'someGlobalActorFn()' in a synchronous nonisolated context}}
  }
}

actor IsolatedConformance: @MainActor MainActorInstanceReq {
  @MainActor func mainActorInstanceMethod() {}

  static func notAWitness() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'notAWitness()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }
}

actor ExtensionConformance {}

extension ExtensionConformance: MainActorInstanceReq {
  nonisolated func mainActorInstanceMethod() {}

  static func notAWitness() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'notAWitness()' part of global actor 'MainActor'}}
    mainActorFn()
    // expected-error@-1:5 {{call to main actor-isolated global function 'mainActorFn()' in a synchronous nonisolated context}}
  }
}

actor PreconcurrencyConformance: @preconcurrency MainActorInstanceReq {
  // expected-warning@-1:7 {{'@preconcurrency' on conformance to 'MainActorInstanceReq' has no effect}}
  // expected-swift6-error@-2:50 {{conformance of 'PreconcurrencyConformance' to protocol 'MainActorInstanceReq' involves isolation mismatches and can cause data races}}
  // expected-minimal-warning@-3:50 {{conformance of 'PreconcurrencyConformance' to protocol 'MainActorInstanceReq' involves isolation mismatches and can cause data races; this is an error in the Swift 6 language mode}}
  // expected-complete-warning@-4:50 {{conformance of 'PreconcurrencyConformance' to protocol 'MainActorInstanceReq' involves isolation mismatches and can cause data races; this is an error in the Swift 6 language mode}}
  func mainActorInstanceMethod() {}
  // expected-note@-1:8 {{actor-isolated instance method 'mainActorInstanceMethod()' cannot satisfy main actor-isolated requirement}}
  // expected-note@-2:8 {{mark instance method 'mainActorInstanceMethod()' 'nonisolated'}}
}
