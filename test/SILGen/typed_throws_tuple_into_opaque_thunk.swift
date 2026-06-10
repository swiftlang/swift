// RUN: %target-swift-emit-silgen %s | %FileCheck %s
// RUN: %target-swift-emit-sil %s -sil-verify-all > /dev/null

// https://github.com/swiftlang/swift/issues/77880
// (and related: https://github.com/swiftlang/swift/issues/88027)
//
// When passing a `(T) throws(E) -> R` closure to a parameter of type
// `(T) throws -> U` where `U` is a generic type variable substituted to
// `R` (which is itself a tuple — possibly `Void`), SILGen must build a
// reabstraction thunk whose inner result is `@out R` and whose outer
// result is `@out T` (an opaque archetype that happens to substitute to
// `R`). The ResultPlanner used to route this through a parallel tuple
// walk on the substituted types; the walk would either iterate zero
// times (empty tuple) or mis-align inner/outer slots (non-empty tuple),
// tripping an assertion during thunk emission (and earlier causing a
// `SmallVector grow_pod` crash in IRGen).
//
// Each case below exercises the reabstraction through a different
// combination of inner tuple arity, element types, and outer throws
// convention. Every reabstraction thunk should forward the inner
// closure's `@out R` slot directly into the outer's `@out T` slot via
// `try_apply` (the planner's addInPlace optimization), and re-erase
// the typed error into `any Error` on the failure edge.

enum E: Error { case foo }

struct Holder {
  // Three distinct entry points, differing only in the outer throws
  // convention the thunk must target. All three used to crash.

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
// Inner:  `@out ()` + `@error E`
// Outer:  `@out ()` + `@error any Error`
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk12compilesVoidyyKF
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSvyt36typed_throws_tuple_into_opaque_thunk1EOIetyrzo_Svyts5Error_pIegyrzo_TR
// CHECK: bb0([[OUT:%.*]] : $*(), [[PTR:%.*]] : $UnsafeMutableRawPointer, [[INNER:%.*]] : $@convention(thin) (UnsafeMutableRawPointer) -> (@out (), @error E)):
// CHECK-NEXT: try_apply [[INNER]]([[OUT]], [[PTR]]) {{.*}} normal [[NORMAL:bb[0-9]+]], error [[ERROR:bb[0-9]+]]
// CHECK: [[NORMAL]]
// CHECK: return
// CHECK: [[ERROR]]({{%.*}} : $E):
// CHECK: alloc_existential_box $any Error, $E
// CHECK: throw
func compilesVoid() throws {
  var h = Holder()
  try h.rethrowsBytes { (p) throws(E) in
  }
}

// ----------------------------------------------------------------------------
// Case 2: trivial 2-tuple via `rethrows`. Same addInPlace thunk shape
// — regression check that the fix isn't tied to empty tuples.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk14compilesPairOfSi_SitSgyKF
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSvSi_Sit36typed_throws_tuple_into_opaque_thunk1EOIetyrzo_SvSi_Sits5Error_pIegyrzo_TR
// CHECK: bb0([[OUT2:%.*]] : $*(Int, Int), [[PTR2:%.*]] : $UnsafeMutableRawPointer, [[INNER2:%.*]] : $@convention(thin) (UnsafeMutableRawPointer) -> (@out (Int, Int), @error E)):
// CHECK-NEXT: try_apply [[INNER2]]([[OUT2]], [[PTR2]])
func compilesPairOf() throws -> (Int, Int)? {
  var h = Holder()
  return try h.rethrowsBytes { (p) throws(E) -> (Int, Int) in
    return (1, 2)
  }
}

// ----------------------------------------------------------------------------
// Case 3: heterogeneous 3-tuple containing a reference-counted element
// (`String`) via `throws`. The outer convention is `throws` (rather than
// `rethrows`), and the non-trivial element exercises copy/destroy along
// the addInPlace path. Crash manifests identically to the other shapes
// because the planner state is driven by inner-result slot count, not
// the specific element types.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk14compilesTripleSi_SSSbtSgyKF
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSvSi_SSSbt36typed_throws_tuple_into_opaque_thunk1EOIetyrzo_SvSi_SSSbts5Error_pIegyrzo_TR
// CHECK: bb0([[OUT3:%.*]] : $*(Int, String, Bool), [[PTR3:%.*]] : $UnsafeMutableRawPointer, [[INNER3:%.*]] : $@convention(thin) (UnsafeMutableRawPointer) -> (@out (Int, String, Bool), @error E)):
// CHECK-NEXT: try_apply [[INNER3]]([[OUT3]], [[PTR3]])
func compilesTriple() throws -> (Int, String, Bool)? {
  var h = Holder()
  return try h.throwsBytes { (p) throws(E) -> (Int, String, Bool) in
    return (42, "hi", true)
  }
}

// ----------------------------------------------------------------------------
// Case 4: throws(any Error) on the outer function. Reabstraction for
// `rethrows` and `throws(any Error)` lowers to the same SIL function
// type, so case 2's thunk is shared here — just confirm the function
// itself is emitted and references it by the same mangled name.
// ----------------------------------------------------------------------------

// CHECK-LABEL: sil hidden [ossa] @$s36typed_throws_tuple_into_opaque_thunk20compilesThrowsAnyAllSi_SitSgyKF
// CHECK: function_ref @$sSvSi_Sit36typed_throws_tuple_into_opaque_thunk1EOIetyrzo_SvSi_Sits5Error_pIegyrzo_TR
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
  func openForBusiness() {
    self.fryer.fry {
      (drainOil(), nil)
    }
  }
}

// CHECK-LABEL: sil shared {{.*}}[reabstraction_thunk] {{.*}}@$sSSSgIgo_ytAAs5Error_pIegrozo_TR
// CHECK: bb0([[OUT:%.*]] : $*(), [[INNER:%.*]] : @guaranteed $@noescape @callee_guaranteed () -> @owned Optional<String>):
// CHECK-NEXT: [[RES:%.*]] = apply [[INNER]]()
// CHECK-NEXT: return [[RES]]
