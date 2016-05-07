// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

import Swift

var zero: Int

// CHECK-LABEL: sil hidden @_TF16generic_closures28generic_nondependent_context{{.*}}
func generic_nondependent_context<T>(_ x: T, y: Int) -> Int {
  func foo() -> Int { return y }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures28generic_nondependent_context{{.*}} : $@convention(thin) (Int) -> Int
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures15generic_capture{{.*}}
func generic_capture<T>(_ x: T) -> Any.Type {
  func foo() -> Any.Type { return T.self }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures15generic_capture{{.*}} : $@convention(thin) <τ_0_0> () -> @thick protocol<>.Type
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures20generic_capture_cast{{.*}}
func generic_capture_cast<T>(_ x: T, y: Any) -> Bool {
  func foo(_ a: Any) -> Bool { return a is T }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures20generic_capture_cast{{.*}} : $@convention(thin) <τ_0_0> (@in protocol<>) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo(y)
}

protocol Concept {
  var sensical: Bool { get }
}

// CHECK-LABEL: sil hidden @_TF16generic_closures29generic_nocapture_existential{{.*}}
func generic_nocapture_existential<T>(_ x: T, y: Concept) -> Bool {
  func foo(_ a: Concept) -> Bool { return a.sensical }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures29generic_nocapture_existential{{.*}} : $@convention(thin) (@in Concept) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo(y)
}

// CHECK-LABEL: sil hidden @_TF16generic_closures25generic_dependent_context{{.*}}
func generic_dependent_context<T>(_ x: T, y: Int) -> T {
  func foo() -> T { return x }
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
    return foo()
  }

  class func generic_dependent_inner_context<T>(_ x: T, y: Int, z: U) -> T {
    func foo() -> T { return x }
    return foo()
  }

  class func generic_dependent_outer_context<T>(_ x: T, y: Int, z: U) -> U {
    func foo() -> U { return z }
    return foo()
  }

  class func generic_dependent_both_contexts<T>(_ x: T, y: Int, z: U) -> (T, U) {
    func foo() -> (T, U) { return (x, z) }
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

func captures_class_constrained_generic<T : HasClassAssoc>(_ x: T, f: (T.Assoc) -> T.Assoc) {
  let _: () -> (T.Assoc) -> T.Assoc = { f }
}
