// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-stdlib -emit-silgen -suppress-argument-labels-in-types %s | %FileCheck %s

import Swift

var zero: Int

// CHECK-LABEL: sil hidden @_TF16generic_closures28generic_nondependent_context{{.*}}
func generic_nondependent_context<T>(_ x: T, y: Int) -> Int {
  func foo() -> Int { return y }

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures28generic_nondependent_context{{.*}} : $@convention(thin) (Int) -> Int
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]](%1)
  // CHECK: strong_release [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures28generic_nondependent_context{{.*}} : $@convention(thin) (Int) -> Int
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures15generic_capture{{.*}}
func generic_capture<T>(_ x: T) -> Any.Type {
  func foo() -> Any.Type { return T.self }

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures15generic_capture{{.*}} : $@convention(thin) <τ_0_0> () -> @thick Any.Type
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]]<T>()
  // CHECK: strong_release [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures15generic_capture{{.*}} : $@convention(thin) <τ_0_0> () -> @thick Any.Type
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]<T>()
  return foo()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures20generic_capture_cast{{.*}}
func generic_capture_cast<T>(_ x: T, y: Any) -> Bool {
  func foo(_ a: Any) -> Bool { return a is T }

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures20generic_capture_cast{{.*}} : $@convention(thin) <τ_0_0> (@in Any) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]]<T>()
  // CHECK: strong_release [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures20generic_capture_cast{{.*}} : $@convention(thin) <τ_0_0> (@in Any) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]<T>([[ARG:%.*]])
  return foo(y)
}

protocol Concept {
  var sensical: Bool { get }
}

// CHECK-LABEL: sil hidden @_TF16generic_closures29generic_nocapture_existential{{.*}}
func generic_nocapture_existential<T>(_ x: T, y: Concept) -> Bool {
  func foo(_ a: Concept) -> Bool { return a.sensical }

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures29generic_nocapture_existential{{.*}} : $@convention(thin) (@in Concept) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = thin_to_thick_function [[FOO]]
  // CHECK: strong_release [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures29generic_nocapture_existential{{.*}} : $@convention(thin) (@in Concept) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]([[ARG:%.*]])
  return foo(y)
}

// CHECK-LABEL: sil hidden @_TF16generic_closures25generic_dependent_context{{.*}}
func generic_dependent_context<T>(_ x: T, y: Int) -> T {
  func foo() -> T { return x }

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures25generic_dependent_context{{.*}} : $@convention(thin) <τ_0_0> (@owned @box τ_0_0) -> @out τ_0_0
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]]<T>([[BOX:%.*]])
  // CHECK: strong_release [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures25generic_dependent_context{{.*}} : $@convention(thin) <τ_0_0> (@owned @box τ_0_0) -> @out τ_0_0
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]<T>
  return foo()
}

enum Optionable<Wrapped> {
  case none
  case some(Wrapped)
}

class NestedGeneric<U> {
  class func generic_nondependent_context<T>(_ x: T, y: Int, z: U) -> Int {
    func foo() -> Int { return y }
    let _ = foo
    return foo()
  }

  class func generic_dependent_inner_context<T>(_ x: T, y: Int, z: U) -> T {
    func foo() -> T { return x }
    let _ = foo
    return foo()
  }

  class func generic_dependent_outer_context<T>(_ x: T, y: Int, z: U) -> U {
    func foo() -> U { return z }
    let _ = foo
    return foo()
  }

  class func generic_dependent_both_contexts<T>(_ x: T, y: Int, z: U) -> (T, U) {
    func foo() -> (T, U) { return (x, z) }
    let _ = foo
    return foo()
  }

  // CHECK-LABEL: sil hidden @_TFC16generic_closures13NestedGeneric20nested_reabstraction{{.*}}
  //   CHECK:       [[REABSTRACT:%.*]] = function_ref @_TTRG__rXFo___XFo_iT__iT__
  //   CHECK:       partial_apply [[REABSTRACT]]<U, T>
  func nested_reabstraction<T>(_ x: T) -> Optionable<() -> ()> {
    return .some({})
  }
}

// <rdar://problem/15417773>
// Ensure that nested closures capture the generic parameters of their nested
// context.

// CHECK: sil hidden @_TF16generic_closures25nested_closure_in_generic{{.*}} : $@convention(thin) <T> (@in T) -> @out T
// CHECK:   function_ref [[OUTER_CLOSURE:@_TFF16generic_closures25nested_closure_in_genericurFxxU_FT_Q_]]
// CHECK: sil shared [[OUTER_CLOSURE]] : $@convention(thin) <T> (@inout_aliasable T) -> @out T
// CHECK:   function_ref [[INNER_CLOSURE:@_TFFF16generic_closures25nested_closure_in_genericurFxxU_FT_Q_U_FT_Q_]]
// CHECK: sil shared [[INNER_CLOSURE]] : $@convention(thin) <T> (@inout_aliasable T) -> @out T {
func nested_closure_in_generic<T>(_ x:T) -> T {
  return { { x }() }()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures16local_properties
func local_properties<T>(_ t: inout T) {
  // CHECK: [[TBOX:%[0-9]+]] = alloc_box $T
  var prop: T {
    get {
      return t
    }
    set {
      t = newValue
    }
  }

  // CHECK: [[GETTER_REF:%[0-9]+]] = function_ref [[GETTER_CLOSURE:@_TFF16generic_closures16local_properties.*]] : $@convention(thin) <τ_0_0> (@owned @box τ_0_0) -> @out τ_0_0
  // CHECK: apply [[GETTER_REF]]
  t = prop

  // CHECK: [[SETTER_REF:%[0-9]+]] = function_ref [[SETTER_CLOSURE:@_TFF16generic_closures16local_properties.*]] : $@convention(thin) <τ_0_0> (@in τ_0_0, @owned @box τ_0_0) -> ()
  // CHECK: apply [[SETTER_REF]]
  prop = t

  var prop2: T {
    get {
      return t
    }
    set {
      // doesn't capture anything
    }
  }

  // CHECK: [[GETTER2_REF:%[0-9]+]] = function_ref [[GETTER2_CLOSURE:@_TFF16generic_closures16local_properties.*]] : $@convention(thin) <τ_0_0> (@owned @box τ_0_0) -> @out τ_0_0
  // CHECK: apply [[GETTER2_REF]]
  t = prop2

  // CHECK: [[SETTER2_REF:%[0-9]+]] = function_ref [[SETTER2_CLOSURE:@_TFF16generic_closures16local_properties.*]] : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
  // CHECK: apply [[SETTER2_REF]]
  prop2 = t
}

protocol Fooable {
  static func foo() -> Bool
}

// <rdar://problem/16399018>
func shmassert(_ f: @autoclosure () -> Bool) {}

// CHECK-LABEL: sil hidden @_TF16generic_closures21capture_generic_param
func capture_generic_param<A: Fooable>(_ x: A) {
  shmassert(A.foo())
}

// Make sure we use the correct convention when capturing class-constrained
// member types: <rdar://problem/24470533>
class Class {}

protocol HasClassAssoc { associatedtype Assoc : Class }

// CHECK-LABEL: sil hidden @_TF16generic_closures34captures_class_constrained_genericuRxS_13HasClassAssocrFTx1fFwx5AssocwxS1__T_
// CHECK: bb0(%0 : $*T, %1 : $@callee_owned (@owned T.Assoc) -> @owned T.Assoc):
// CHECK: [[GENERIC_FN:%.*]] = function_ref @_TFF16generic_closures34captures_class_constrained_genericuRxS_13HasClassAssocrFTx1fFwx5AssocwxS1__T_U_FT_FQQ_5AssocS2_
// CHECK: [[CONCRETE_FN:%.*]] = partial_apply [[GENERIC_FN]]<T, T.Assoc>(%1)

func captures_class_constrained_generic<T : HasClassAssoc>(_ x: T, f: @escaping (T.Assoc) -> T.Assoc) {
  let _: () -> (T.Assoc) -> T.Assoc = { f }
}

// Make sure local generic functions can have captures

// CHECK-LABEL: sil hidden @_TF16generic_closures13outer_genericurFT1tx1iSi_T_ : $@convention(thin) <T> (@in T, Int) -> ()
func outer_generic<T>(t: T, i: Int) {
  func inner_generic_nocapture<U>(u: U) -> U {
    return u
  }

  func inner_generic1<U>(u: U) -> Int {
    return i
  }

  func inner_generic2<U>(u: U) -> T {
    return t
  }

  let _: () -> () = inner_generic_nocapture
  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures13outer_genericurFT1tx1iSi_T_L_23inner_generic_nocaptureu__rFT1uqd___qd__ : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<T, ()>() : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  // CHECK: [[THUNK:%.*]] = function_ref @_TTRGrXFo_iT__iT__XFo___
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]<T>([[CLOSURE]])
  // CHECK: strong_release [[THUNK_CLOSURE]]

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures13outer_genericurFT1tx1iSi_T_L_23inner_generic_nocaptureu__rFT1uqd___qd__ : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<T, T>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  _ = inner_generic_nocapture(u: t)

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures13outer_genericurFT1tx1iSi_T_L_14inner_generic1u__rfT1uqd___Si : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<T, ()>(%1) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  // CHECK: [[THUNK:%.*]] = function_ref @_TTRGrXFo_iT__dSi_XFo__dSi_
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]<T>([[CLOSURE]])
  // CHECK: strong_release [[THUNK_CLOSURE]]
  let _: () -> Int = inner_generic1

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures13outer_genericurFT1tx1iSi_T_L_14inner_generic1u__rfT1uqd___Si : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<T, T>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  _ = inner_generic1(u: t)

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures13outer_genericurFT1tx1iSi_T_L_14inner_generic2u__rfT1uqd___x : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned @box τ_0_0) -> @out τ_0_0
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<T, ()>([[ARG:%.*]]) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned @box τ_0_0) -> @out τ_0_0
  // CHECK: [[THUNK:%.*]] = function_ref @_TTRGrXFo_iT__ix_XFo__ix_
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]<T>([[CLOSURE]])
  // CHECK: strong_release [[THUNK_CLOSURE]]
  let _: () -> T = inner_generic2

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures13outer_genericurFT1tx1iSi_T_L_14inner_generic2u__rfT1uqd___x : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned @box τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<T, T>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned @box τ_0_0) -> @out τ_0_0
  _ = inner_generic2(u: t)
}

// CHECK-LABEL: sil hidden @_TF16generic_closures14outer_concreteFT1iSi_T_ : $@convention(thin) (Int) -> ()
func outer_concrete(i: Int) {
  func inner_generic_nocapture<U>(u: U) -> U {
    return u
  }

  func inner_generic<U>(u: U) -> Int {
    return i
  }

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures14outer_concreteFT1iSi_T_L_23inner_generic_nocaptureurFT1ux_x : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<()>() : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  // CHECK: [[THUNK:%.*]] = function_ref @_TTRXFo_iT__iT__XFo___
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]([[CLOSURE]])
  // CHECK: strong_release [[THUNK_CLOSURE]]
  let _: () -> () = inner_generic_nocapture

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures14outer_concreteFT1iSi_T_L_23inner_generic_nocaptureurFT1ux_x : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<Int>({{.*}}) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  _ = inner_generic_nocapture(u: i)

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures14outer_concreteFT1iSi_T_L_13inner_genericurfT1ux_Si : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<()>(%0) : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  // CHECK: [[THUNK:%.*]] = function_ref @_TTRXFo_iT__dSi_XFo__dSi_
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]([[CLOSURE]])
  // CHECK: strong_release [[THUNK_CLOSURE]]
  let _: () -> Int = inner_generic

  // CHECK: [[FN:%.*]] = function_ref @_TFF16generic_closures14outer_concreteFT1iSi_T_L_13inner_genericurfT1ux_Si : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<Int>({{.*}}) : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  _ = inner_generic(u: i)
}

// CHECK-LABEL: sil hidden @_TF16generic_closures32mixed_generic_nongeneric_nestingurFT1tx_T_ : $@convention(thin) <T> (@in T) -> ()
func mixed_generic_nongeneric_nesting<T>(t: T) {
  func outer() {
    func middle<U>(u: U) {
      func inner() -> U {
        return u
      }
      inner()
    }
    middle(u: 11)
  }
  outer()
}

// CHECK-LABEL: sil shared @_TFF16generic_closures32mixed_generic_nongeneric_nestingurFT1tx_T_L_5outerurFT_T_ : $@convention(thin) <T> () -> ()
// CHECK-LABEL: sil shared @_TFFF16generic_closures32mixed_generic_nongeneric_nestingurFT1tx_T_L_5outerurFT_T_L_6middleu__rFT1uqd___T_ : $@convention(thin) <T><U> (@in U) -> ()
// CHECK-LABEL: sil shared @_TFFFF16generic_closures32mixed_generic_nongeneric_nestingurFT1tx_T_L_5outerurFT_T_L_6middleu__rFT1uqd___T_L_5inneru__rfT_qd__ : $@convention(thin) <T><U> (@owned @box U) -> @out U
