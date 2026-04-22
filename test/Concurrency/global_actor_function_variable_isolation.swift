// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -verify-additional-prefix minimal-targeted-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -strict-concurrency=targeted -verify-additional-prefix minimal-targeted-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -strict-concurrency=complete -verify-additional-prefix complete-
// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple -swift-version 6 -verify-additional-prefix swift6-

// REQUIRES: concurrency

// https://github.com/swiftlang/swift/issues/81599
// Top-level variables are main-actor-isolated, but their function type's isolation can be erased.
// The reference isolation needs to be checked for applications of VarDecls.

// Call this to test that the functions stored in top level variables actually have MainActor inferred.
// MainActor isn't inferred for minimal or targeted however, so we'll need to verify that its not allowed to be called in those cases.
@MainActor func mainActorOnly() -> Int { 0 } // expected-minimal-targeted-note 3 {{calls to global function 'mainActorOnly()' from outside of its actor context are implicitly asynchronous}}

// expected-complete-note@+2 2 {{let declared here}}
// expected-swift6-note@+1 2 {{let declared here}}
let topLevelImplicitMainActorFnLet: () -> Void = {
  _ = mainActorOnly() // expected-minimal-targeted-error {{call to main actor-isolated global function 'mainActorOnly()' in a synchronous nonisolated context}}
}

let sendableCaller: @Sendable () -> Void = {
  topLevelImplicitMainActorFnLet() // expected-complete-warning {{main actor-isolated let 'topLevelImplicitMainActorFnLet' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-1 {{main actor-isolated let 'topLevelImplicitMainActorFnLet' can not be referenced from a nonisolated context}}

  // Refs that weren't part of application were already being checked.
  let _ = topLevelImplicitMainActorFnLet // expected-complete-warning {{main actor-isolated let 'topLevelImplicitMainActorFnLet' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-1 {{main actor-isolated let 'topLevelImplicitMainActorFnLet' can not be referenced from a nonisolated context}}
}

let sendableAsyncCaller: @Sendable () async -> Void = {
  await topLevelImplicitMainActorFnLet() // expected-minimal-targeted-warning {{no 'async' operations occur within 'await' expression}}
  // expected-complete-warning@-1 {{non-Sendable type '() -> Void' of let 'topLevelImplicitMainActorFnLet' cannot exit main actor-isolated context}}
  // expected-complete-note@-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  // expected-swift6-error@-3 {{non-Sendable type '() -> Void' of let 'topLevelImplicitMainActorFnLet' cannot exit main actor-isolated context}}
  // expected-swift6-note@-4 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

// expected-complete-note@+2 2 {{var declared here}}
// expected-swift6-note@+1 2 {{var declared here}}
var topLevelImplicitMainActorFnVar: () -> Void = {
  _ = mainActorOnly() // expected-minimal-targeted-error {{call to main actor-isolated global function 'mainActorOnly()' in a synchronous nonisolated context}}
}

let sendableCallerForVar: @Sendable () -> Void = {
  topLevelImplicitMainActorFnVar() // expected-complete-warning {{main actor-isolated var 'topLevelImplicitMainActorFnVar' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-1 {{main actor-isolated var 'topLevelImplicitMainActorFnVar' can not be referenced from a nonisolated context}}

  let _ = topLevelImplicitMainActorFnVar // expected-complete-warning {{main actor-isolated var 'topLevelImplicitMainActorFnVar' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-1 {{main actor-isolated var 'topLevelImplicitMainActorFnVar' can not be referenced from a nonisolated context}}
}

@MainActor func testFromMainActor() {
  topLevelImplicitMainActorFnLet()
  topLevelImplicitMainActorFnVar()
}

// expected-complete-note@+2 2 {{var declared here}}
// expected-swift6-note@+1 2 {{var declared here}}
var topLevelImplicitMainActorOptionalFn: (() -> Void)? = {
  _ = mainActorOnly() // expected-minimal-targeted-error {{call to main actor-isolated global function 'mainActorOnly()' in a synchronous nonisolated context}}
}

let sendableCallerForOptional: @Sendable () -> Void = {
  topLevelImplicitMainActorOptionalFn?() // expected-complete-warning {{main actor-isolated var 'topLevelImplicitMainActorOptionalFn' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-1 {{main actor-isolated var 'topLevelImplicitMainActorOptionalFn' can not be referenced from a nonisolated context}}

  let _ = topLevelImplicitMainActorOptionalFn // expected-complete-warning {{main actor-isolated var 'topLevelImplicitMainActorOptionalFn' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-1 {{main actor-isolated var 'topLevelImplicitMainActorOptionalFn' can not be referenced from a nonisolated context}}
}

struct IsolatedFnLet {
  @MainActor static let explicitMainActorLet: () -> Void = {} // expected-minimal-targeted-note 2 {{static property declared here}}
  // expected-complete-note@-1 2 {{static property declared here}}
  // expected-swift6-note@-2 2 {{static property declared here}}
}

// expected-minimal-targeted-note@+3 {{add '@MainActor' to make global function 'testStaticFromNonisolated()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
// expected-complete-note@+2 {{add '@MainActor' to make global function 'testStaticFromNonisolated()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
// expected-swift6-note@+1 {{add '@MainActor' to make global function 'testStaticFromNonisolated()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
func testStaticFromNonisolated() {
  IsolatedFnLet.explicitMainActorLet() // expected-minimal-targeted-warning {{main actor-isolated static property 'explicitMainActorLet' can not be referenced from a nonisolated context}}
  // expected-complete-warning@-1 {{main actor-isolated static property 'explicitMainActorLet' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-2 {{main actor-isolated static property 'explicitMainActorLet' can not be referenced from a nonisolated context}}
}

@MainActor func testStaticSameActor() {
  IsolatedFnLet.explicitMainActorLet()
}

// Exercise differing variable and function type isolation.

@globalActor actor OtherGlobalActor {
  static let shared = OtherGlobalActor()
}

struct CrossIsolatedFnLets {
  @OtherGlobalActor static let otherActorHoldingMainActorFn: @MainActor () -> Void = {} // expected-minimal-targeted-note {{static property declared here}}
  // expected-complete-note@-1 {{static property declared here}}
  // expected-swift6-note@-2 {{static property declared here}}
  @MainActor static let mainActorHoldingOtherActorFn: @OtherGlobalActor () -> Void = {} // expected-minimal-targeted-note {{static property declared here}}
  // expected-complete-note@-1 {{static property declared here}}
  // expected-swift6-note@-2 {{static property declared here}}
}

// From the storage's actor: storage access is fine, but the call crosses isolation.
@OtherGlobalActor func testOtherHoldingMainSync() {
  CrossIsolatedFnLets.otherActorHoldingMainActorFn() // expected-error {{call to main actor-isolated function in a synchronous global actor 'OtherGlobalActor'-isolated context}}
}

@MainActor func testMainHoldingOtherSync() {
  CrossIsolatedFnLets.mainActorHoldingOtherActorFn() // expected-error {{call to global actor 'OtherGlobalActor'-isolated function in a synchronous main actor-isolated context}}
}

// From the function type's actor: call is fine, but storage access crosses isolation.
@MainActor func testMainCallingOtherHeldSync() {
  CrossIsolatedFnLets.otherActorHoldingMainActorFn() // expected-minimal-targeted-warning {{global actor 'OtherGlobalActor'-isolated static property 'otherActorHoldingMainActorFn' can not be referenced from the main actor}}
  // expected-complete-warning@-1 {{global actor 'OtherGlobalActor'-isolated static property 'otherActorHoldingMainActorFn' can not be referenced from the main actor}}
  // expected-swift6-error@-2 {{global actor 'OtherGlobalActor'-isolated static property 'otherActorHoldingMainActorFn' can not be referenced from the main actor}}
}

@OtherGlobalActor func testOtherCallingMainHeldSync() {
  CrossIsolatedFnLets.mainActorHoldingOtherActorFn() // expected-minimal-targeted-warning {{main actor-isolated static property 'mainActorHoldingOtherActorFn' can not be referenced from global actor 'OtherGlobalActor'}}
  // expected-complete-warning@-1 {{main actor-isolated static property 'mainActorHoldingOtherActorFn' can not be referenced from global actor 'OtherGlobalActor'}}
  // expected-swift6-error@-2 {{main actor-isolated static property 'mainActorHoldingOtherActorFn' can not be referenced from global actor 'OtherGlobalActor'}}
}

// Async from the storage's actor: await hops to the function type's actor.
@OtherGlobalActor func testOtherHoldingMainAsync() async {
  await CrossIsolatedFnLets.otherActorHoldingMainActorFn()
}

@MainActor func testMainHoldingOtherAsync() async {
  await CrossIsolatedFnLets.mainActorHoldingOtherActorFn()
}

// expected-minimal-targeted-note@+3 {{add '@MainActor' to make global function 'testReferenceWithoutCall()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
// expected-complete-note@+2 {{add '@MainActor' to make global function 'testReferenceWithoutCall()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
// expected-swift6-note@+1 {{add '@MainActor' to make global function 'testReferenceWithoutCall()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
func testReferenceWithoutCall() {
  let _ = IsolatedFnLet.explicitMainActorLet // expected-minimal-targeted-warning {{main actor-isolated static property 'explicitMainActorLet' can not be referenced from a nonisolated context}}
  // expected-complete-warning@-1 {{main actor-isolated static property 'explicitMainActorLet' can not be referenced from a nonisolated context}}
  // expected-swift6-error@-2 {{main actor-isolated static property 'explicitMainActorLet' can not be referenced from a nonisolated context}}
}

func testFromNonisolatedAsync() async {
  await topLevelImplicitMainActorFnLet() // expected-minimal-targeted-warning {{no 'async' operations occur within 'await' expression}}
  // expected-complete-warning@-1 {{non-Sendable type '() -> Void' of let 'topLevelImplicitMainActorFnLet' cannot exit main actor-isolated context}}
  // expected-complete-note@-2 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
  // expected-swift6-error@-3 {{non-Sendable type '() -> Void' of let 'topLevelImplicitMainActorFnLet' cannot exit main actor-isolated context}}
  // expected-swift6-note@-4 {{a function type must be marked '@Sendable' to conform to 'Sendable'}}
}

struct MutableFnVar {
  @MainActor static var explicitMainActorVar: () -> Void = {} // expected-note {{static property declared here}}
}

// expected-note@+1 {{add '@MainActor' to make global function 'testMutableVar()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
func testMutableVar() {
  MutableFnVar.explicitMainActorVar() // expected-error {{main actor-isolated static property 'explicitMainActorVar' can not be referenced from a nonisolated context}}
}

struct OptionalFnVars {
  @MainActor static var optionalFn: (() -> Void)? = {} // expected-note {{static property declared here}}
}

// expected-note@+1 {{add '@MainActor' to make global function 'testOptionalChaining()' part of global actor 'MainActor'}} {{1-1=@MainActor }}
func testOptionalChaining() {
  OptionalFnVars.optionalFn?() // expected-error {{main actor-isolated static property 'optionalFn' can not be referenced from a nonisolated context}}
}
