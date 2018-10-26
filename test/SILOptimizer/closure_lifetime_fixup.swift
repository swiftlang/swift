// RUN: %target-swift-frontend %s -sil-verify-all -enable-sil-ownership -emit-sil -o - | %FileCheck %s

func use_closure(_ c : () -> () ) {
  c()
}

func use_closureGeneric<T>(_ c : () -> T ) {
  _ = c()
}

public class C {
  func doIt() {}
  func returnInt() -> Int { return 0 }
}

// CHECK-LABEL: sil @$s22closure_lifetime_fixup10testSimple1cyAA1CC_tF : $@convention(thin) (@guaranteed C) -> () {
// CHECK: bb0([[ARG:%.*]] : $C):
// CHECK: [[F:%.*]]  = function_ref @$s22closure_lifetime_fixup10testSimple1cyAA1CC_tFyyXEfU_
// CHECK-NEXT:  strong_retain [[ARG]] : $C
// CHECK-NEXT:  [[PA:%.*]] = partial_apply [callee_guaranteed] [[F]]([[ARG]]) : $@convention(thin) (@guaranteed C) -> ()
// CHECK-NEXT: strong_retain [[PA]] : $@callee_guaranteed () -> ()
// CHECK-NEXT:  [[CVT:%.*]] = convert_escape_to_noescape [[PA]] : $@callee_guaranteed () -> () to $@noescape @callee_guaranteed () -> ()
// CHECK-NEXT:  // function_ref use_closure(_:)
// CHECK-NEXT:  [[F2:%.*]] = function_ref @$s22closure_lifetime_fixup04use_A0yyyyXEF : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
// CHECK-NEXT:  apply [[F2]]([[CVT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> ()) -> ()
// CHECK-NEXT:  strong_release [[PA]] : $@callee_guaranteed () -> ()
// CHECK-NEXT:  strong_release [[PA]] : $@callee_guaranteed () -> ()
// CHECK-NEXT:  tuple ()
// CHECK-NEXT:  return {{.*}} : $()
public func testSimple(c: C) {
  use_closure { c.doIt() }
}

// CHECK-LABEL: sil @$s22closure_lifetime_fixup11testGeneric1cyAA1CC_tF : $@convention(thin) (@guaranteed C) -> () {
// CHECK:bb0([[ARG:%.*]] : $C):
// CHECK:  [[F:%.*]] = function_ref @$s22closure_lifetime_fixup11testGeneric1cyAA1CC_tFSiyXEfU_ : $@convention(thin) (@guaranteed C) -> Int
// CHECK-NEXT:  strong_retain [[ARG]] : $C
// CHECK-NEXT:  [[PA:%.*]] = partial_apply [callee_guaranteed] [[F]]([[ARG]]) : $@convention(thin) (@guaranteed C) -> Int
// CHECK-NEXT:  strong_retain [[PA]] : $@callee_guaranteed () -> Int
// CHECK-NEXT:  [[CVT:%.*]] = convert_escape_to_noescape [[PA]] : $@callee_guaranteed () -> Int to $@noescape @callee_guaranteed () -> Int
// CHECK-NEXT:  // function_ref thunk for @callee_guaranteed () -> (@unowned Int)
// CHECK-NEXT:  [[F:%.*]] = function_ref @$sSiIgd_SiIegr_TR : $@convention(thin) (@noescape @callee_guaranteed () -> Int) -> @out Int
// CHECK-NEXT:  [[PA2:%.*]] = partial_apply [callee_guaranteed] [[F]]([[CVT]]) : $@convention(thin) (@noescape @callee_guaranteed () -> Int) -> @out Int
// CHECK-NEXT:  [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]] : $@callee_guaranteed () -> @out Int to $@noescape @callee_guaranteed () -> @out Int
// CHECK-NEXT:  strong_release [[PA]] : $@callee_guaranteed () -> Int
// CHECK-NEXT:  // function_ref use_closureGeneric<A>(_:)
// CHECK-NEXT:  [[F2:%.*]] = function_ref @$s22closure_lifetime_fixup04use_A7GenericyyxyXElF : $@convention(thin) <τ_0_0> (@noescape @callee_guaranteed () -> @out τ_0_0) -> ()
// CHECK-NEXT:  %12 = apply [[F2]]<Int>([[CVT2]]) : $@convention(thin) <τ_0_0> (@noescape @callee_guaranteed () -> @out τ_0_0) -> ()
// CHECK-NEXT:  strong_release [[PA2]] : $@callee_guaranteed () -> @out Int
// CHECK-NEXT:  strong_release [[PA]] : $@callee_guaranteed () -> Int
// CHECK-NEXT:  tuple ()
// CHECK-NEXT:  return {{.*}} : $()

public func testGeneric(c: C) {
  use_closureGeneric { return c.returnInt() }
}

public protocol P {
  associatedtype Element
  subscript<U>(a: (Element) -> U, b: (U) -> Element) -> U { get set }
}

// Make sure we keep the closure alive up until after the write back.

// CHECK-LABEL: sil @$s22closure_lifetime_fixup10testModify1pyxz_tAA1PRzSS7ElementRtzlF : $@convention(thin) <T where T : P, T.Element == String> (@inout T) -> () {
// CHECK: bb0
// CHECK:  [[PA1:%.*]] = partial_apply [callee_guaranteed]
// CHECK:  [[ENUM1:%.*]] = enum $Optional<@callee_guaranteed (@in_guaranteed String) -> @out Int>, #Optional.some!enumelt.1, [[PA1]] 
// CHECK:  [[CVT1:%.*]] = convert_escape_to_noescape [[PA1]]
// CHECK:  [[PA2:%.*]] = partial_apply [callee_guaranteed]
// CHECK:  [[ENUM2:%.*]] = enum $Optional<@callee_guaranteed (@in_guaranteed Int) -> @out String>, #Optional.some!enumelt.1, [[PA2]]
// CHECK:  [[CVT2:%.*]] = convert_escape_to_noescape [[PA2]]
// CHECK:  [[W:%.*]] = witness_method $T, #P.subscript!modify.1
// CHECK:  ([[BUFFER:%.*]], [[TOKEN:%.*]]) = begin_apply [[W]]<T, Int>([[CVT1]], [[CVT2]], {{.*}})
// CHECK:  end_apply [[TOKEN]]
// CHECK:  release_value [[ENUM1]]
// CHECK:  release_value [[ENUM2]]
public func testModify<T : P>(p: inout T) where T.Element == String {
  p[{Int($0)!}, {String($0)}] += 1
}

public func dontCrash<In, Out>(test: Bool, body: @escaping ((In) -> Out, In) -> Out ) -> (In) -> Out {
  var result: ((In) -> Out)!
  result = { (x: In) in
    if test {
      return body(result, x)
    }
    let r = body(result, x)
    return r
  }
  return result
}
