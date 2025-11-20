// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -swift-version 6 -emit-sil -o /dev/null -verify %s

// REQUIRES: concurrency
// REQUIRES: asserts

final class ImmutablePoint: Sendable {
  let x : Int = 0
  let y : Int = 0
}

actor SomeActor { }

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

/// ------------------------------------------------------------------
/// -- Value types do not need isolation on their stored properties --
protocol MainCounter {
  @MainActor var counter: Int { get set }
  @MainActor var ticker: Int { get set }
}

struct InferredFromConformance: MainCounter {
  var counter = 0
  var ticker: Int {
    get { 1 }
    set {}
  }
}

@MainActor
struct InferredFromContext {
  var point = ImmutablePoint()
  var polygon: [ImmutablePoint] {
    get { [] }
  }

  nonisolated let flag: Bool = false

  subscript(_ i: Int) -> Int { return i }

  static var stuff: [Int] = []
}

func checkIsolationValueType(_ formance: InferredFromConformance,
                             _ ext: InferredFromContext,
                             _ anno: NoGlobalActorValueType) async {
  // these do not need an await, since it's a value type
  _ = ext.point
  _ = formance.counter
  _ = anno.counter

  // make sure it's just a warning if someone was awaiting on it previously
  _ = await ext.point // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = await formance.counter  // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = await anno.counter  // expected-warning {{no 'async' operations occur within 'await' expression}}
  
  // this does not need an await, since the property is 'Sendable' and of a
  // value type
  _ = anno.point
  _ = await anno.point
  // expected-warning@-1 {{no 'async' operations occur within 'await' expression}}

  // these do need await, regardless of reference or value type
  _ = await (formance as any MainCounter).counter
  // expected-error@-1 {{non-Sendable type 'any MainCounter' cannot be sent into main actor-isolated context in call to property 'counter'}}
  _ = await ext[1]
  _ = await formance.ticker
  _ = await ext.polygon
  _ = await InferredFromContext.stuff
  _ = await NoGlobalActorValueType.polygon
}

struct NoGlobalActorValueType {
  @SomeGlobalActor var point: ImmutablePoint

  @MainActor let counter: Int

  @MainActor static var polygon: [ImmutablePoint] = []
}

/// -----------------------------------------------------------------

@MainActor
class MainActorIsolated {
  init() {}

  // expected-note@+1 {{static property declared here}}
  static let shared = MainActorIsolated()
}

nonisolated func accessAcrossActors() {
  // expected-error@+1 {{main actor-isolated static property 'shared' can not be referenced from a nonisolated context}}
  let _ = MainActorIsolated.shared
}

struct ReferenceSelfDotMethods {
  @MainActor
  func mainActorAffinedFunction() {}

  nonisolated
  private func testCurry() -> (Self) -> (@MainActor () -> Void) {
    let functionRef = Self.mainActorAffinedFunction
    return functionRef
  }

  @MainActor
  private func callOnMainActorOk() {
    let mainActorAffinedClosure = testCurry()(self)
    mainActorAffinedClosure()
  }

  nonisolated
  private func nonisolatedCallErrors() {
    let mainActorAffinedClosure = testCurry()(self)
    // expected-note@-1 {{calls to let 'mainActorAffinedClosure' from outside of its actor context are implicitly asynchronous}}
    mainActorAffinedClosure()
    // expected-error@-1 {{call to main actor-isolated let 'mainActorAffinedClosure' in a synchronous nonisolated context}}
  }
}

actor UserDefinedActorSelfDotMethod {
  func actorAffinedFunc() {} // expected-note {{calls to instance method 'actorAffinedFunc()' from outside of its actor context are implicitly asynchronous}}

  // Unfortunately we can't express the desired isolation of this returned closure statically to
  // be able to call it on the desired actor. This may be possible with the acceptance of
  // https://forums.swift.org/t/closure-isolation-control/70378 but I think we need more expressivity
  // in the type system to express this sort of curry.
  nonisolated
  private func testCurry() -> (UserDefinedActorSelfDotMethod) -> (@isolated(any) () -> Void) {
    let functionRef = Self.actorAffinedFunc // expected-error {{call to actor-isolated instance method 'actorAffinedFunc()' in a synchronous nonisolated context}}
    return functionRef // expected-error {{cannot convert return expression of type '@Sendable (isolated Self) -> @Sendable () -> ()' to return type '(UserDefinedActorSelfDotMethod) -> @isolated(any) () -> Void'}}
  }
}