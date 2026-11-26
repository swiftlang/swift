// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s

// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-sil %s -sil-verify-all > /dev/null

// https://github.com/swiftlang/swift/issues/77880
// (and related: https://github.com/swiftlang/swift/issues/88027)
//
// When passing a `(T) throws(E) -> R` closure to a parameter of type
// `(T) throws -> U` where `U` is a generic type variable substituted to
// `R` (which is itself a tuple — possibly `Void`), SILGen used to build a
// reabstraction thunk whose inner result is `@out R` and whose outer
// result is `@out T` (an opaque archetype that happens to substitute to
// `R`). The ResultPlanner used to route this through a parallel tuple
// walk on the substituted types; the walk would either iterate zero
// times (empty tuple) or mis-align inner/outer slots (non-empty tuple),
// tripping an assertion during thunk emission (and earlier causing a
// `SmallVector grow_pod` crash in IRGen).
//
// The closure is now lowered at the parameter's abstraction pattern with
// the type-erased error type, so it is passed directly, without a thunk
// (https://github.com/swiftlang/swift/issues/92312). Each case below
// covers a different combination of tuple arity, element types, and outer
// throws convention.

enum E: Error { case foo }

struct Holder {
  // Three distinct entry points, differing only in the outer throws
  // convention. All three used to crash.

  mutating func rethrowsBytes<T>(_ body: (UnsafeMutableRawPointer) throws -> T) rethrows -> T {
    fatalError()
  }
  mutating func throwsBytes<T>(_ body: (UnsafeMutableRawPointer) throws -> T) throws -> T {
    fatalError()
  }
  mutating func throwsAnyBytes<T>(_ body: (UnsafeMutableRawPointer) throws(any Error) -> T) throws(any Error) -> T {
    fatalError()
  }
}

// ----------------------------------------------------------------------------
// Case 1: empty tuple (Void) via `rethrows`.
// Closure: `@out ()` + `@error any Error`
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk12compilesVoidyyKF
// CHECK: [[CLOSURE1:%.*]] = function_ref @$s36typed_throws_tuple_into_opaque_thunk12compilesVoidyyKFySvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <()>
// CHECK-NEXT: [[THICK1:%.*]] = thin_to_thick_function [[CLOSURE1]]
// CHECK-NOT: thunk
// CHECK: function_ref @$s36typed_throws_tuple_into_opaque_thunk6HolderV13rethrowsBytesyxxSvKXEKlF
// CHECK-NEXT: try_apply {{%[0-9]+}}<()>({{%[0-9]+}}, [[THICK1]], {{%[0-9]+}})
// CHECK-LABEL: sil private [ossa] @$s36typed_throws_tuple_into_opaque_thunk12compilesVoidyyKFySvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <()> {
// CHECK: bb0({{%[0-9]+}} : $*(), {{%[0-9]+}} : $UnsafeMutableRawPointer):
func compilesVoid() throws {
  var h = Holder()
  try h.rethrowsBytes { (p) throws(E) in
  }
}

// ----------------------------------------------------------------------------
// Case 2: trivial 2-tuple via `rethrows`. Regression check that the fix
// isn't tied to empty tuples.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk14compilesPairOfSi_SitSgyKF
// CHECK: [[CLOSURE2:%.*]] = function_ref @$s36typed_throws_tuple_into_opaque_thunk14compilesPairOfSi_SitSgyKFSi_SitSvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <(Int, Int)>
// CHECK-NEXT: [[THICK2:%.*]] = thin_to_thick_function [[CLOSURE2]]
// CHECK-NOT: thunk
// CHECK: function_ref @$s36typed_throws_tuple_into_opaque_thunk6HolderV13rethrowsBytesyxxSvKXEKlF
// CHECK-NEXT: try_apply {{%[0-9]+}}<(Int, Int)>({{%[0-9]+}}, [[THICK2]], {{%[0-9]+}})
// CHECK-LABEL: sil private [ossa] @$s36typed_throws_tuple_into_opaque_thunk14compilesPairOfSi_SitSgyKFSi_SitSvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <(Int, Int)> {
// CHECK: bb0({{%[0-9]+}} : $*(Int, Int), {{%[0-9]+}} : $UnsafeMutableRawPointer):
func compilesPairOf() throws -> (Int, Int)? {
  var h = Holder()
  return try h.rethrowsBytes { (p) throws(E) -> (Int, Int) in
    return (1, 2)
  }
}

// ----------------------------------------------------------------------------
// Case 3: heterogeneous 3-tuple containing a reference-counted element
// (`String`) via `throws`. The outer convention is `throws` (rather than
// `rethrows`), and the non-trivial element is stored into the closure's
// `@out` result. The crash manifested identically to the other shapes
// because the planner state was driven by inner-result slot count, not
// the specific element types.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk14compilesTripleSi_SSSbtSgyKF
// CHECK: [[CLOSURE3:%.*]] = function_ref @$s36typed_throws_tuple_into_opaque_thunk14compilesTripleSi_SSSbtSgyKFSi_SSSbtSvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <(Int, String, Bool)>
// CHECK-NEXT: [[THICK3:%.*]] = thin_to_thick_function [[CLOSURE3]]
// CHECK-NOT: thunk
// CHECK: function_ref @$s36typed_throws_tuple_into_opaque_thunk6HolderV0B5BytesyxxSvKXEKlF
// CHECK-NEXT: try_apply {{%[0-9]+}}<(Int, String, Bool)>({{%[0-9]+}}, [[THICK3]], {{%[0-9]+}})
// CHECK-LABEL: sil private [ossa] @$s36typed_throws_tuple_into_opaque_thunk14compilesTripleSi_SSSbtSgyKFSi_SSSbtSvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <(Int, String, Bool)> {
// CHECK: bb0({{%[0-9]+}} : $*(Int, String, Bool), {{%[0-9]+}} : $UnsafeMutableRawPointer):
func compilesTriple() throws -> (Int, String, Bool)? {
  var h = Holder()
  return try h.throwsBytes { (p) throws(E) -> (Int, String, Bool) in
    return (42, "hi", true)
  }
}

// ----------------------------------------------------------------------------
// Case 4: throws(any Error) on the outer function. `rethrows` and
// `throws(any Error)` lower to the same SIL function type, so the closure
// has the same lowered type as in case 2.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk20compilesThrowsAnyAllSi_SitSgyKF
// CHECK: [[CLOSURE4:%.*]] = function_ref @$s36typed_throws_tuple_into_opaque_thunk20compilesThrowsAnyAllSi_SitSgyKFSi_SitSvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <(Int, Int)>
// CHECK-NEXT: [[THICK4:%.*]] = thin_to_thick_function [[CLOSURE4]]
// CHECK-NOT: thunk
// CHECK: function_ref @$s36typed_throws_tuple_into_opaque_thunk6HolderV0B8AnyBytesyxxSvKXEKlF
// CHECK-NEXT: try_apply {{%[0-9]+}}<(Int, Int)>({{%[0-9]+}}, [[THICK4]], {{%[0-9]+}})
// CHECK-LABEL: sil private [ossa] @$s36typed_throws_tuple_into_opaque_thunk20compilesThrowsAnyAllSi_SitSgyKFSi_SitSvAA1EOYKXEfU_ : $@convention(thin) @substituted <τ_0_0> (UnsafeMutableRawPointer) -> (@out τ_0_0, @error any Error) for <(Int, Int)> {
// CHECK: bb0({{%[0-9]+}} : $*(Int, Int), {{%[0-9]+}} : $UnsafeMutableRawPointer):
func compilesThrowsAnyAll() throws -> (Int, Int)? {
  var h = Holder()
  return try h.throwsAnyBytes { (p) throws(E) -> (Int, Int) in
    return (3, 4)
  }
}

// Sibling: outer pattern is a tuple `(opaque T, String?)` whose first
// element substitutes to `()`. The fix's interface-type gate prevents
// the whole-tuple-into-opaque branch from mis-claiming the sibling
// `String?` slot when recursively processing the empty-tuple element.

protocol DonutFryer {}

extension DonutFryer {
  @discardableResult
  func fry<T>(perform: () throws -> (donut: T, glaze: String?)) rethrows -> T {
    let (d, _) = try perform()
    return d
  }
}

func drainOil() {}

struct DonutShop {
  let fryer: DonutFryer
  func openForBusiness(_ fn: () -> (donut: (), glaze: String?)) {
    self.fryer.fry(perform: fn)
  }
}

// CHECK-LABEL: sil shared {{.*}}[reabstraction_thunk] {{.*}}@$sSSSgs5Error_pIgozo_ytAAsAB_pIegrozo_TR
// CHECK: bb0([[OUT:%.*]] : $*(), [[INNER:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> (@owned Optional<String>, @error any Error)):
// CHECK-NEXT: try_apply [[INNER]]()

