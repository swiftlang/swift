// RUN: %target-swift-frontend -disable-availability-checking -swift-version 6 -emit-sil -o /dev/null -verify %s

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
  // expected-error@-1 {{non-sendable type 'any MainCounter' cannot be sent into main actor-isolated context in call to property 'counter'}}
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
