// RUN: %target-swift-frontend %s -emit-sil -o - -verify -swift-version 6 | %FileCheck %s

// REQUIRES: concurrency

// The initializer of a 'lazy' stored property is moved into the property's
// synthesized getter. A computed (get-only) property with the same body has its
// closures inherit the enclosing actor isolation; a 'lazy' property should
// behave identically. This test checks that closures appearing in a 'lazy'
// initializer inherit the lazy variable's actor isolation (so region-based
// isolation does not emit spurious cross-isolation diagnostics), while
// '@Sendable' / '@concurrent' closures correctly remain nonisolated -- exactly
// as they would inside a computed property getter.
//
// rdar://problem -- see getActorIsolationOfContext in lib/AST/Decl.cpp, which
// reports a lazy stored property's initializer context as isolated to the lazy
// variable so the initializer's closures inherit that isolation.

class NS {}

@globalActor
actor CustomActor {
  static let shared = CustomActor()
}

func takesSendable(_ f: @Sendable () -> Int) -> Int { f() }
func takesSending(_ f: sending () -> Int) -> Int { f() }
func takesPlain(_ f: () -> Int) -> Int { f() }

// =============================================================================
// @MainActor (non-final) class -- the original reproducer shape.
// =============================================================================

@MainActor
class MainActorClass {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in MainActorClass.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a = items.map { item in use(item) }
}

// =============================================================================
// @MainActor final class.
// =============================================================================

@MainActor
final class MainActorFinalClass {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in MainActorFinalClass.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a = items.map { item in use(item) }
}

// =============================================================================
// @MainActor struct.
// =============================================================================

@MainActor
struct MainActorStruct {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in MainActorStruct.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a = items.map { item in use(item) }
}

// =============================================================================
// Isolation on the 'lazy' property only (enclosing type is nonisolated).
// =============================================================================

class PropertyOnlyClass {
  var items: [NS] = []
  @MainActor func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in PropertyOnlyClass.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  @MainActor lazy var a = items.map { item in use(item) }
}

struct PropertyOnlyStruct {
  var items: [NS] = []
  @MainActor func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in PropertyOnlyStruct.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  @MainActor lazy var a = items.map { item in use(item) }
}

// =============================================================================
// Custom global actor (not MainActor), on the type and on the property only.
// =============================================================================

@CustomActor
class CustomActorClass {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in CustomActorClass.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: CustomActor
  lazy var a = items.map { item in use(item) }
}

class CustomActorPropertyOnly {
  var items: [NS] = []
  @CustomActor func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in CustomActorPropertyOnly.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: CustomActor
  @CustomActor lazy var a = items.map { item in use(item) }
}

// =============================================================================
// Explicit type annotation on the lazy property.
// =============================================================================

@MainActor
class ExplicitType {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in ExplicitType.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a: [Int] = items.map { item in use(item) }
}

// =============================================================================
// Directly-invoked closure (IIFE): the '.map' closure is now nested one level
// deeper, inside the outer '{ ... }()' closure. Both must inherit isolation,
// matching a computed property.
// =============================================================================

@MainActor
class ImmediatelyInvoked {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in ImmediatelyInvoked.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: // closure #1 in closure #1 in ImmediatelyInvoked.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a = { self.items.map { item in self.use(item) } }()
}

// =============================================================================
// Nested closures (without an IIFE): the inner closure must also inherit.
// =============================================================================

@MainActor
class NestedClosures {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in NestedClosures.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: // closure #1 in closure #1 in NestedClosures.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a = items.map { outer in items.map { inner in use(inner) } }
}

// =============================================================================
// '@Sendable' / '@concurrent' closures break the inheritance chain and must
// remain nonisolated, just as inside a computed property getter.
// =============================================================================

@MainActor
class SendableClosure {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // The '@Sendable' sync closure stays nonisolated; the plain '.map' closure
  // still inherits MainActor.
  // CHECK-LABEL: // closure #1 in SendableClosure.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: // closure #1 in closure #1 in SendableClosure.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  // CHECK-LABEL: // closure #2 in closure #1 in SendableClosure.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a: [Int] = {
    _ = takesSendable { @Sendable in 0 }
    return items.map { item in use(item) }
  }()
}

@MainActor
class NonSendableInsideSendable {
  func use() -> Int { 0 }

  // A non-'@Sendable' closure nested inside a '@Sendable' closure must remain
  // nonisolated -- the '@Sendable' boundary breaks isolation inheritance.
  // CHECK-LABEL: // closure #1 in NonSendableInsideSendable.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  // CHECK-LABEL: // closure #1 in closure #1 in NonSendableInsideSendable.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  // CHECK-LABEL: // closure #1 in closure #1 in closure #1 in NonSendableInsideSendable.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  lazy var a: Int = { takesSendable { @Sendable in takesPlain { 7 } } }()
}

// =============================================================================
// Isolation inference boundaries: a closure that is '@Sendable' or passed to a
// 'sending' parameter must NOT inherit the lazy variable's isolation. Such a
// closure infers its own isolation independent of the enclosing context, so it
// stays nonisolated -- exactly as it would inside a computed property getter.
// (If it instead inherited the actor isolation, region-based isolation would
// reject sending the now-isolated, non-Sendable closure.)
// =============================================================================

@MainActor
class SendableBoundary {
  // CHECK-LABEL: // closure #1 in SendableBoundary.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  lazy var a: Int = takesSendable { 1 }
}

@MainActor
class SendingBoundary {
  // CHECK-LABEL: // closure #1 in SendingBoundary.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  lazy var a: Int = takesSending { 2 }
}

@MainActor
class CustomActorSendingBoundary {
  // The same holds for a custom global actor, not just MainActor.
  // CHECK-LABEL: // closure #1 in CustomActorSendingBoundary.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  @CustomActor lazy var a: Int = takesSending { 3 }
}

@MainActor
class MixedBoundary {
  func use() -> Int { 0 }

  // A boundary closure ('sending') and a plain closure in the same initializer:
  // only the plain one inherits; the boundary closure stays nonisolated.
  // CHECK-LABEL: // closure #1 in MixedBoundary.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  // CHECK-LABEL: // closure #2 in MixedBoundary.a.getter
  // CHECK-NEXT:  // Isolation: global_actor. type: MainActor
  lazy var a: Int = takesSending { 1 } + takesPlain { use() }
}

// =============================================================================
// Control: a nonisolated enclosing context must NOT gain isolation. The lazy
// initializer closure stays nonisolated (the lazy variable is not isolated).
// =============================================================================

class NonisolatedClass {
  var items: [NS] = []
  func use(_ x: NS) -> Int { 0 }

  // CHECK-LABEL: // closure #1 in NonisolatedClass.a.getter
  // CHECK-NEXT:  // Isolation: nonisolated
  lazy var a = items.map { item in use(item) }
}
