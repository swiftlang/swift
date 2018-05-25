
// RUN: %target-swift-emit-silgen -module-name witnesses_class -enable-sil-ownership %s | %FileCheck %s

protocol Fooable: class {
  func foo()
  static func bar()
  init()
}

class Foo: Fooable {
  
  func foo() { }
  // CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class3FooCAA7FooableA2aDP3foo{{[_0-9a-zA-Z]*}}FTW
  // CHECK-NOT:     function_ref
  // CHECK:         class_method

  class func bar() {}
  // CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class3FooCAA7FooableA2aDP3bar{{[_0-9a-zA-Z]*}}FZTW
  // CHECK-NOT:     function_ref
  // CHECK:         class_method

  required init() {}
  // CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class3FooCAA7FooableA2aDP{{[_0-9a-zA-Z]*}}fCTW
  // CHECK-NOT:     function_ref
  // CHECK:         class_method
}

// CHECK-LABEL: sil hidden @$S15witnesses_class3genyyxAA7FooableRzlF
// CHECK:         bb0([[SELF:%.*]] : @guaranteed $T)
// CHECK-NOT:     copy_value [[SELF]]
// CHECK:         [[METHOD:%.*]] = witness_method $T
// CHECK:         apply [[METHOD]]<T>([[SELF]])
// CHECK-NOT:         destroy_value [[SELF]]
// CHECK:         return
func gen<T: Fooable>(_ foo: T) {
  foo.foo()
}

// CHECK-LABEL: sil hidden @$S15witnesses_class2exyyAA7Fooable_pF
// CHECK: bb0([[SELF:%[0-0]+]] : @guaranteed $Fooable):
// CHECK:         [[SELF_PROJ:%.*]] = open_existential_ref [[SELF]]
// CHECK:         [[METHOD:%.*]] = witness_method $[[OPENED:@opened(.*) Fooable]],
// CHECK-NOT:     copy_value [[SELF_PROJ]] : $
// CHECK:         apply [[METHOD]]<[[OPENED]]>([[SELF_PROJ]])
// CHECK-NOT:     destroy_value [[SELF]]
// CHECK:         return
func ex(_ foo: Fooable) {
  foo.foo()
}

// Default implementations in a protocol extension
protocol HasDefaults {
  associatedtype T = Self

  func hasDefault()

  func hasDefaultTakesT(_: T)

  func hasDefaultGeneric<U : Fooable>(_: U)

  func hasDefaultGenericTakesT<U : Fooable>(_: T, _: U)
}

extension HasDefaults {
  func hasDefault() {}

  func hasDefaultTakesT(_: T) {}

  func hasDefaultGeneric<U : Fooable>(_: U) {}

  func hasDefaultGenericTakesT<U : Fooable>(_: T, _: U) {}
}

protocol Barable {}

class UsesDefaults<X : Barable> : HasDefaults {}

// Covariant Self:

// CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class12UsesDefaultsCyqd__GAA03HasD0A2aEP10hasDefaultyyFTW : $@convention(witness_method: HasDefaults) <τ_0_0><τ_1_0 where τ_0_0 : UsesDefaults<τ_1_0>, τ_1_0 : Barable> (@in_guaranteed τ_0_0) -> () {
// CHECK: [[FN:%.*]] = function_ref @$S15witnesses_class11HasDefaultsPAAE10hasDefaultyyF : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaults> (@in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<τ_0_0>(
// CHECK: return

// Invariant Self, since type signature contains an associated type:

// CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class12UsesDefaultsCyxGAA03HasD0A2aEP16hasDefaultTakesTyy1TQzFTW : $@convention(witness_method: HasDefaults) <τ_0_0 where τ_0_0 : Barable> (@in_guaranteed UsesDefaults<τ_0_0>, @in_guaranteed UsesDefaults<τ_0_0>) -> ()
// CHECK: [[FN:%.*]] = function_ref @$S15witnesses_class11HasDefaultsPAAE16hasDefaultTakesTyy1TQzF : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaults> (@in_guaranteed τ_0_0.T, @in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<UsesDefaults<τ_0_0>>(
// CHECK: return

// Covariant Self:

// CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class12UsesDefaultsCyqd__GAA03HasD0A2aEP17hasDefaultGenericyyqd__AA7FooableRd__lFTW : $@convention(witness_method: HasDefaults) <τ_0_0><τ_1_0 where τ_0_0 : UsesDefaults<τ_1_0>, τ_1_0 : Barable><τ_2_0 where τ_2_0 : Fooable> (@guaranteed τ_2_0, @in_guaranteed τ_0_0) -> () {
// CHECK: [[FN:%.*]] = function_ref @$S15witnesses_class11HasDefaultsPAAE17hasDefaultGenericyyqd__AA7FooableRd__lF : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaults><τ_1_0 where τ_1_0 : Fooable> (@guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<τ_0_0, τ_2_0>(
// CHECK: return

// Invariant Self, since type signature contains an associated type:

// CHECK-LABEL: sil private [transparent] [thunk] @$S15witnesses_class12UsesDefaultsCyxGAA03HasD0A2aEP23hasDefaultGenericTakesTyy1TQz_qd__tAA7FooableRd__lFTW : $@convention(witness_method: HasDefaults) <τ_0_0 where τ_0_0 : Barable><τ_1_0 where τ_1_0 : Fooable> (@in_guaranteed UsesDefaults<τ_0_0>, @guaranteed τ_1_0, @in_guaranteed UsesDefaults<τ_0_0>) -> ()
// CHECK: [[FN:%.*]] = function_ref @$S15witnesses_class11HasDefaultsPAAE23hasDefaultGenericTakesTyy1TQz_qd__tAA7FooableRd__lF : $@convention(method) <τ_0_0 where τ_0_0 : HasDefaults><τ_1_0 where τ_1_0 : Fooable> (@in_guaranteed τ_0_0.T, @guaranteed τ_1_0, @in_guaranteed τ_0_0) -> ()
// CHECK: apply [[FN]]<UsesDefaults<τ_0_0>, τ_1_0>(
// CHECK: return
