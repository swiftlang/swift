// RUN: %target-swift-frontend -parse-stdlib -emit-silgen %s | FileCheck %s

import Swift

var zero: Int

// CHECK-LABEL: sil hidden @_TF16generic_closures28generic_nondependent_context{{.*}}
func generic_nondependent_context<T>(x: T, y: Int) -> Int {
  var y = y
  func foo() -> Int { return y }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures28generic_nondependent_context{{.*}} : $@convention(thin) (@owned @box Int) -> Int
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures15generic_capture{{.*}}
func generic_capture<T>(x: T) -> Any.Type {
  func foo() -> Any.Type { return T.self }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures15generic_capture{{.*}} : $@convention(thin) <τ_0_0> () -> @thick protocol<>.Type
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo()
}

// CHECK-LABEL: sil hidden @_TF16generic_closures20generic_capture_cast{{.*}}
func generic_capture_cast<T>(x: T, y: Any) -> Bool {
  func foo(a: Any) -> Bool { return a is T }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures20generic_capture_cast{{.*}} : $@convention(thin) <τ_0_0> (@in protocol<>) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo(y)
}

protocol Concept {
  var sensical: Bool { get }
}

// CHECK-LABEL: sil hidden @_TF16generic_closures29generic_nocapture_existential{{.*}}
func generic_nocapture_existential<T>(x: T, y: Concept) -> Bool {
  func foo(a: Concept) -> Bool { return a.sensical }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures29generic_nocapture_existential{{.*}} : $@convention(thin) (@in Concept) -> Bool
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]
  return foo(y)
}

// CHECK-LABEL: sil hidden @_TF16generic_closures25generic_dependent_context{{.*}}
func generic_dependent_context<T>(x: T, y: Int) -> T {
  func foo() -> T { return x }
  // CHECK: [[FOO:%.*]] = function_ref @_TFF16generic_closures25generic_dependent_context{{.*}} : $@convention(thin) <τ_0_0> (@out τ_0_0, @owned @box τ_0_0) -> ()
  // CHECK: [[FOO_CLOSURE:%.*]] = apply [[FOO]]<T>
  return foo()
}

enum Optionable<T> {
  case Some(T)
  case None
}

class NestedGeneric<U> {
  class func generic_nondependent_context<T>(x: T, y: Int, z: U) -> Int {
    func foo() -> Int { return y }
    return foo()
  }

  class func generic_dependent_inner_context<T>(x: T, y: Int, z: U) -> T {
    func foo() -> T { return x }
    return foo()
  }

  class func generic_dependent_outer_context<T>(x: T, y: Int, z: U) -> U {
    func foo() -> U { return z }
    return foo()
  }

  class func generic_dependent_both_contexts<T>(x: T, y: Int, z: U) -> (T, U) {
    func foo() -> (T, U) { return (x, z) }
    return foo()
  }

  // CHECK-LABEL: sil hidden @_TFC16generic_closures13NestedGeneric20nested_reabstraction{{.*}}
  //   CHECK:       [[REABSTRACT:%.*]] = function_ref @_TTRG__rXFo__dT__XFo_iT__iT__
  //   CHECK:       partial_apply [[REABSTRACT]]<U, T>
  func nested_reabstraction<T>(x: T) -> Optionable<() -> ()> {
    return .Some({})
  }
}

// CHECK-LABEL: sil hidden @_TF16generic_closures24generic_curried_function{{.*}} : $@convention(thin) <T, U> (@in U, @in T) -> () {
// CHECK-LABEL: sil shared @_TF16generic_closures24generic_curried_function{{.*}}
func generic_curried_function<T, U>(x: T)(y: U) { }

var f: (Int) -> () = generic_curried_function(zero)

  // <rdar://problem/15417773>
  // Ensure that nested closures capture the generic parameters of their nested
  // context.

  // CHECK: sil hidden @_TF16generic_closures25nested_closure_in_generic{{.*}} : $@convention(thin) <T> (@out T, @in T) -> ()
  // CHECK:   function_ref [[OUTER_CLOSURE:@_TFF16generic_closures25nested_closure_in_genericurFxxU_FT_Q_]]
  // CHECK: sil shared [[OUTER_CLOSURE]] : $@convention(thin) <T> (@out T, @inout_aliasable T) -> ()
  // CHECK:   function_ref [[INNER_CLOSURE:@_TFFF16generic_closures25nested_closure_in_genericurFxxU_FT_Q_U_FT_Q_]]
  // CHECK: sil shared [[INNER_CLOSURE]] : $@convention(thin) <T> (@out T, @inout_aliasable T) -> () {
  func nested_closure_in_generic<T>(x:T) -> T {
    return { { x }() }()
  }

// CHECK-LABEL: sil hidden @_TF16generic_closures16local_properties
func local_properties<T>(inout t: T) {
  // CHECK: [[TBOX:%[0-9]+]] = alloc_box $T
  var prop: T {
    get {
      return t
    }
    set {
      t = newValue
    }
  }

  // CHECK: [[GETTER_REF:%[0-9]+]] = function_ref [[GETTER_CLOSURE:@_TFF16generic_closures16local_properties.*]] : $@convention(thin) <τ_0_0> (@out τ_0_0, @owned @box τ_0_0) -> ()
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

  // CHECK: [[GETTER2_REF:%[0-9]+]] = function_ref [[GETTER2_CLOSURE:@_TFF16generic_closures16local_properties.*]] : $@convention(thin) <τ_0_0> (@out τ_0_0, @owned @box τ_0_0) -> ()
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
func shmassert(@autoclosure f: () -> Bool) {}

// CHECK-LABEL: sil hidden @_TF16generic_closures21capture_generic_param
func capture_generic_param<A: Fooable>(x: A) {
  shmassert(A.foo())
}
