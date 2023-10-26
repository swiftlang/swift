// RUN: %target-swift-frontend -disable-availability-checking -warn-concurrency -swift-version 6 -emit-sil -o /dev/null -verify %s
// RUN: %target-swift-frontend -disable-availability-checking -warn-concurrency -swift-version 6 -emit-sil -o /dev/null -verify -enable-experimental-feature RegionBasedIsolation %s

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

  nonisolated let flag: Bool = false // expected-error {{'nonisolated' is redundant on struct's stored properties}}{{3-15=}}

  subscript(_ i: Int) -> Int { return i }

  static var stuff: [Int] = []
}

func checkIsolationValueType(_ formance: InferredFromConformance,
                             _ ext: InferredFromContext,
                             _ anno: NoGlobalActorValueType) async {
  // these do not need an await, since it's a value type
  _ = ext.point
  _ = formance.counter
  _ = anno.point
  _ = anno.counter

  // make sure it's just a warning if someone was awaiting on it previously
  _ = await ext.point // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = await formance.counter  // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = await anno.point  // expected-warning {{no 'async' operations occur within 'await' expression}}
  _ = await anno.counter  // expected-warning {{no 'async' operations occur within 'await' expression}}

  // these do need await, regardless of reference or value type
  _ = await (formance as any MainCounter).counter
  // expected-warning@-1 {{non-sendable type 'any MainCounter' passed in implicitly asynchronous call to main actor-isolated property 'counter' cannot cross actor boundary}}
  _ = await ext[1]
  _ = await formance.ticker
  _ = await ext.polygon
  _ = await InferredFromContext.stuff
  _ = await NoGlobalActorValueType.polygon
}

// check for instance members that do not need global-actor protection
struct NoGlobalActorValueType {
  @SomeGlobalActor var point: ImmutablePoint // expected-error {{stored property 'point' within struct cannot have a global actor}}

  @MainActor let counter: Int // expected-error {{stored property 'counter' within struct cannot have a global actor}}

  @MainActor static var polygon: [ImmutablePoint] = []
}

/// -----------------------------------------------------------------
