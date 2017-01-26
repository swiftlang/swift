// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -parse-stdlib -emit-silgen %s | %FileCheck %s

import Swift

var zero: Int

// CHECK-LABEL: sil hidden @_T016generic_closures0A21_nondependent_context{{[_0-9a-zA-Z]*}}F
func generic_nondependent_context<T>(_ x: T, y: Int) -> Int {
  func foo() -> Int { return y }

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A21_nondependent_context{{.*}} : $@convention(thin) (Int) -> Int
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]](%1)
  // CHECK: destroy_value [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A21_nondependent_context{{.*}} : $@convention(thin) (Int) -> Int
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo()
}

// CHECK-LABEL: sil hidden @_T016generic_closures0A8_capture{{[_0-9a-zA-Z]*}}F
func generic_capture<T>(_ x: T) -> Any.Type {
  func foo() -> Any.Type { return T.self }

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A8_capture{{.*}} : $@convention(thin) <τ_0_0> () -> @thick Any.Type
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]]<T>()
  // CHECK: destroy_value [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A8_capture{{.*}} : $@convention(thin) <τ_0_0> () -> @thick Any.Type
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]<T>()
  return foo()
}

// CHECK-LABEL: sil hidden @_T016generic_closures0A13_capture_cast{{[_0-9a-zA-Z]*}}F
func generic_capture_cast<T>(_ x: T, y: Any) -> Bool {
  func foo(_ a: Any) -> Bool { return a is T }

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A13_capture_cast{{.*}} : $@convention(thin) <τ_0_0> (@in Any) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]]<T>()
  // CHECK: destroy_value [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A13_capture_cast{{.*}} : $@convention(thin) <τ_0_0> (@in Any) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]<T>([[ARG:%.*]])
  return foo(y)
}

protocol Concept {
  var sensical: Bool { get }
}

// CHECK-LABEL: sil hidden @_T016generic_closures0A22_nocapture_existential{{[_0-9a-zA-Z]*}}F
func generic_nocapture_existential<T>(_ x: T, y: Concept) -> Bool {
  func foo(_ a: Concept) -> Bool { return a.sensical }

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A22_nocapture_existential{{.*}} : $@convention(thin) (@in Concept) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = thin_to_thick_function [[FOO]]
  // CHECK: destroy_value [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A22_nocapture_existential{{.*}} : $@convention(thin) (@in Concept) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]([[ARG:%.*]])
  return foo(y)
}

// CHECK-LABEL: sil hidden @_T016generic_closures0A18_dependent_context{{[_0-9a-zA-Z]*}}F
func generic_dependent_context<T>(_ x: T, y: Int) -> T {
  func foo() -> T { return x }

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A18_dependent_context{{.*}} : $@convention(thin) <τ_0_0> (@owned <τ_0_0> { var τ_0_0 } <τ_0_0>) -> @out τ_0_0
  // CHECK: [[FOO_CLOSURE:%.*]] = partial_apply [[FOO]]<T>([[BOX:%.*]])
  // CHECK: destroy_value [[FOO_CLOSURE]]
  let _ = foo

  // CHECK: [[FOO:%.*]] = function_ref @_T016generic_closures0A18_dependent_context{{.*}} : $@convention(thin) <τ_0_0> (@owned <τ_0_0> { var τ_0_0 } <τ_0_0>) -> @out τ_0_0
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

  // CHECK-LABEL: sil hidden @_T016generic_closures13NestedGenericC20nested_reabstraction{{[_0-9a-zA-Z]*}}F
  //   CHECK:       [[REABSTRACT:%.*]] = function_ref @_T0Ix_ytytIxir_TR
  //   CHECK:       partial_apply [[REABSTRACT]]
  func nested_reabstraction<T>(_ x: T) -> Optionable<() -> ()> {
    return .some({})
  }
}

// <rdar://problem/15417773>
// Ensure that nested closures capture the generic parameters of their nested
// context.

// CHECK: sil hidden @_T016generic_closures018nested_closure_in_A0xxlF : $@convention(thin) <T> (@in T) -> @out T
// CHECK:   function_ref [[OUTER_CLOSURE:@_T016generic_closures018nested_closure_in_A0xxlFxycfU_]]
// CHECK: sil shared [[OUTER_CLOSURE]] : $@convention(thin) <T> (@inout_aliasable T) -> @out T
// CHECK:   function_ref [[INNER_CLOSURE:@_T016generic_closures018nested_closure_in_A0xxlFxycfU_xycfU_]]
// CHECK: sil shared [[INNER_CLOSURE]] : $@convention(thin) <T> (@inout_aliasable T) -> @out T {
func nested_closure_in_generic<T>(_ x:T) -> T {
  return { { x }() }()
}

// CHECK-LABEL: sil hidden @_T016generic_closures16local_properties{{[_0-9a-zA-Z]*}}F
func local_properties<T>(_ t: inout T) {
  var prop: T {
    get {
      return t
    }
    set {
      t = newValue
    }
  }

  // CHECK: [[GETTER_REF:%[0-9]+]] = function_ref [[GETTER_CLOSURE:@_T016generic_closures16local_properties[_0-9a-zA-Z]*]] : $@convention(thin) <τ_0_0> (@inout_aliasable τ_0_0) -> @out τ_0_0
  // CHECK: apply [[GETTER_REF]]
  t = prop

  // CHECK: [[SETTER_REF:%[0-9]+]] = function_ref [[SETTER_CLOSURE:@_T016generic_closures16local_properties[_0-9a-zA-Z]*]] : $@convention(thin) <τ_0_0> (@in τ_0_0, @inout_aliasable τ_0_0) -> ()
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

  // CHECK: [[GETTER2_REF:%[0-9]+]] = function_ref [[GETTER2_CLOSURE:@_T016generic_closures16local_properties[_0-9a-zA-Z]*]] : $@convention(thin) <τ_0_0> (@inout_aliasable τ_0_0) -> @out τ_0_0
  // CHECK: apply [[GETTER2_REF]]
  t = prop2

  // CHECK: [[SETTER2_REF:%[0-9]+]] = function_ref [[SETTER2_CLOSURE:@_T016generic_closures16local_properties[_0-9a-zA-Z]*]] : $@convention(thin) <τ_0_0> (@in τ_0_0) -> ()
  // CHECK: apply [[SETTER2_REF]]
  prop2 = t
}

protocol Fooable {
  static func foo() -> Bool
}

// <rdar://problem/16399018>
func shmassert(_ f: @autoclosure () -> Bool) {}

// CHECK-LABEL: sil hidden @_T016generic_closures08capture_A6_param{{[_0-9a-zA-Z]*}}F
func capture_generic_param<A: Fooable>(_ x: A) {
  shmassert(A.foo())
}

// Make sure we use the correct convention when capturing class-constrained
// member types: <rdar://problem/24470533>
class Class {}

protocol HasClassAssoc { associatedtype Assoc : Class }

// CHECK-LABEL: sil hidden @_T016generic_closures027captures_class_constrained_A0yx_5AssocQzADc1ftAA08HasClassF0RzlF
// CHECK: bb0([[ARG1:%.*]] : $*T, [[ARG2:%.*]] : $@callee_owned (@owned T.Assoc) -> @owned T.Assoc):
// CHECK: [[GENERIC_FN:%.*]] = function_ref @_T016generic_closures027captures_class_constrained_A0yx_5AssocQzADc1ftAA08HasClassF0RzlFAdDcycfU_
// CHECK: [[ARG2_COPY:%.*]] = copy_value [[ARG2]]
// CHECK: [[CONCRETE_FN:%.*]] = partial_apply [[GENERIC_FN]]<T>([[ARG2_COPY]])

func captures_class_constrained_generic<T : HasClassAssoc>(_ x: T, f: @escaping (T.Assoc) -> T.Assoc) {
  let _: () -> (T.Assoc) -> T.Assoc = { f }
}

// Make sure local generic functions can have captures

// CHECK-LABEL: sil hidden @_T016generic_closures06outer_A0yx1t_Si1itlF : $@convention(thin) <T> (@in T, Int) -> ()
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
  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures06outer_A0yx1t_Si1itlF06inner_A10_nocaptureL_qd__qd__1u_tr__lF : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<T, ()>() : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  // CHECK: [[THUNK:%.*]] = function_ref @_T0ytytIxir_Ix_TR
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]([[CLOSURE]])
  // CHECK: destroy_value [[THUNK_CLOSURE]]

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures06outer_A0yx1t_Si1itlF06inner_A10_nocaptureL_qd__qd__1u_tr__lF : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<T, T>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0) -> @out τ_1_0
  _ = inner_generic_nocapture(u: t)

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures06outer_A0yx1t_Si1itlF14inner_generic1L_Siqd__1u_tr__lF : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<T, ()>(%1) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  // CHECK: [[THUNK:%.*]] = function_ref @_T0ytSiIxid_SiIxd_TR
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]([[CLOSURE]])
  // CHECK: destroy_value [[THUNK_CLOSURE]]
  let _: () -> Int = inner_generic1

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures06outer_A0yx1t_Si1itlF14inner_generic1L_Siqd__1u_tr__lF : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<T, T>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, Int) -> Int
  _ = inner_generic1(u: t)

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures06outer_A0yx1t_Si1itlF14inner_generic2L_xqd__1u_tr__lF : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned <τ_0_0> { var τ_0_0 } <τ_0_0>) -> @out τ_0_0
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<T, ()>([[ARG:%.*]]) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned <τ_0_0> { var τ_0_0 } <τ_0_0>) -> @out τ_0_0
  // CHECK: [[THUNK:%.*]] = function_ref @_T0ytxIxir_xIxr_lTR
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]<T>([[CLOSURE]])
  // CHECK: destroy_value [[THUNK_CLOSURE]]
  let _: () -> T = inner_generic2

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures06outer_A0yx1t_Si1itlF14inner_generic2L_xqd__1u_tr__lF : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned <τ_0_0> { var τ_0_0 } <τ_0_0>) -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<T, T>({{.*}}) : $@convention(thin) <τ_0_0><τ_1_0> (@in τ_1_0, @owned <τ_0_0> { var τ_0_0 } <τ_0_0>) -> @out τ_0_0
  _ = inner_generic2(u: t)
}

// CHECK-LABEL: sil hidden @_T016generic_closures14outer_concreteySi1i_tF : $@convention(thin) (Int) -> ()
func outer_concrete(i: Int) {
  func inner_generic_nocapture<U>(u: U) -> U {
    return u
  }

  func inner_generic<U>(u: U) -> Int {
    return i
  }

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures14outer_concreteySi1i_tF06inner_A10_nocaptureL_xx1u_tlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<()>() : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  // CHECK: [[THUNK:%.*]] = function_ref @_T0ytytIxir_Ix_TR
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]([[CLOSURE]])
  // CHECK: destroy_value [[THUNK_CLOSURE]]
  let _: () -> () = inner_generic_nocapture

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures14outer_concreteySi1i_tF06inner_A10_nocaptureL_xx1u_tlF : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<Int>({{.*}}) : $@convention(thin) <τ_0_0> (@in τ_0_0) -> @out τ_0_0
  _ = inner_generic_nocapture(u: i)

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures14outer_concreteySi1i_tF06inner_A0L_Six1u_tlF : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  // CHECK: [[CLOSURE:%.*]] = partial_apply [[FN]]<()>(%0) : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  // CHECK: [[THUNK:%.*]] = function_ref @_T0ytSiIxid_SiIxd_TR
  // CHECK: [[THUNK_CLOSURE:%.*]] = partial_apply [[THUNK]]([[CLOSURE]])
  // CHECK: destroy_value [[THUNK_CLOSURE]]
  let _: () -> Int = inner_generic

  // CHECK: [[FN:%.*]] = function_ref @_T016generic_closures14outer_concreteySi1i_tF06inner_A0L_Six1u_tlF : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  // CHECK: [[RESULT:%.*]] = apply [[FN]]<Int>({{.*}}) : $@convention(thin) <τ_0_0> (@in τ_0_0, Int) -> Int
  _ = inner_generic(u: i)
}

// CHECK-LABEL: sil hidden @_T016generic_closures06mixed_A19_nongeneric_nestingyx1t_tlF : $@convention(thin) <T> (@in T) -> ()
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

// CHECK-LABEL: sil shared @_T016generic_closures06mixed_A19_nongeneric_nestingyx1t_tlF5outerL_yylF : $@convention(thin) <T> () -> ()
// CHECK-LABEL: sil shared @_T016generic_closures06mixed_A19_nongeneric_nestingyx1t_tlF5outerL_yylF6middleL_yqd__1u_tr__lF : $@convention(thin) <T><U> (@in U) -> ()
// CHECK-LABEL: sil shared @_T016generic_closures06mixed_A19_nongeneric_nestingyx1t_tlF5outerL_yylF6middleL_yqd__1u_tr__lF5innerL_qd__yr__lF : $@convention(thin) <T><U> (@owned <τ_0_0><τ_1_0> { var τ_1_0 } <T, U>) -> @out U

protocol Doge {
  associatedtype Nose : NoseProtocol
}

protocol NoseProtocol {
  associatedtype Squeegee
}

protocol Doggo {}

struct DogSnacks<A : Doggo> {}

func capture_same_type_representative<Daisy: Doge, Roo: Doggo>(slobber: Roo)
    where Roo == Daisy.Nose.Squeegee {
  var s = DogSnacks<Daisy.Nose.Squeegee>()
  _ = { _ = s }
}
