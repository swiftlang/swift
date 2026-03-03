// RUN:%target-swift-frontend -emit-silgen %s -enable-experimental-feature BorrowAndMutateAccessors -enable-experimental-feature SuppressedAssociatedTypes -enable-library-evolution | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors
// REQUIRES: swift_feature_SuppressedAssociatedTypes

public protocol P {
  associatedtype Element
  var prop1: Element { get set }
  var prop2: Element { get set }
  var prop3: Element { get set }
}

public class Klass {}

public struct W<Element>: P {
  var _prop: Element
  public var prop1: Element {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }

  public var prop2: Element {
    _read {
      yield _prop
    }
    _modify {
      yield &_prop
    }
  }

  public var prop3: Element {
    get {
      return _prop
    }
    set {
      _prop = newValue
    }
  }
}

public protocol NCP: ~Copyable {
  associatedtype Element: ~Copyable
  var prop1: Element { get set }
  var prop2: Element { get set }
  var prop3: Element { get set }
}

public struct NC<Element: ~Copyable>: NCP & ~Copyable {
  var _prop: Element

  public var prop1: Element {
    borrow {
      return _prop
    }
    mutate {
      return &_prop
    }
  }

  public var prop2: Element {
    _read {
      yield _prop
    }
    _modify {
      yield &_prop
    }
  }

  public var prop3: Element
}

func use<T: ~Copyable>(_ t: borrowing T) {}

func mutate<T: ~Copyable>(_ t: inout T) {}

// CHECK-LABEL: sil hidden [ossa] @$s25borrow_accessor_synthesis6testP1yyxAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> () {
// CHECK: bb0([[REG0:%.*]] : $*τ_0_0):
// CHECK:   [[REG2:%.*]] = alloc_stack $(τ_0_0).Element
// CHECK:   [[REG3:%.*]] = witness_method $τ_0_0, #P.prop1!getter : <Self where Self : P> (Self) -> () -> Self.Element : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<τ_0_0>([[REG2]], [[REG0]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG5:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG6:%.*]] = apply [[REG5]]<(τ_0_0).Element>([[REG2]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG2]]
// CHECK:   dealloc_stack [[REG2]]
// CHECK:   [[REG9:%.*]] = alloc_stack $(τ_0_0).Element
// CHECK:   [[REG10:%.*]] = witness_method $τ_0_0, #P.prop2!getter : <Self where Self : P> (Self) -> () -> Self.Element : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG11:%.*]] = apply [[REG10]]<τ_0_0>([[REG9]], [[REG0]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG12:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG13:%.*]] = apply [[REG12]]<(τ_0_0).Element>([[REG9]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG9]]
// CHECK:   dealloc_stack [[REG9]]
// CHECK:   [[REG16:%.*]] = alloc_stack $(τ_0_0).Element
// CHECK:   [[REG17:%.*]] = witness_method $τ_0_0, #P.prop3!getter : <Self where Self : P> (Self) -> () -> Self.Element : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG18:%.*]] = apply [[REG17]]<τ_0_0>([[REG16]], [[REG0]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG19:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG20:%.*]] = apply [[REG19]]<(τ_0_0).Element>([[REG16]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG16]]
// CHECK:   dealloc_stack [[REG16]]
// CHECK-LABEL: } // end sil function '$s25borrow_accessor_synthesis6testP1yyxAA1PRzlF'
func testP1(_ p: some P) {
  // For Copyable types, all opaque read accesses go through getter
  // Conforming types can synthesize the getter from stored property, get, _read or borrow
  use(p.prop1)
  use(p.prop2)
  use(p.prop3)
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis6testP2yyxzAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> () {
// CHECK: bb0([[REG0:%.*]] : $*τ_0_0):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = witness_method $τ_0_0, #P.prop1!modify : <Self where Self : P> (inout Self) -> () -> () : $@yield_once @convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   ([[REG4]], [[REG5]]) = begin_apply [[REG3]]<τ_0_0>([[REG2]]) : $@yield_once @convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   [[REG6:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<(τ_0_0).Element>([[REG4]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG8:%.*]] = end_apply [[REG5]] as $()
// CHECK:   end_access [[REG2]]
// CHECK:   [[REG10:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG11:%.*]] = witness_method $τ_0_0, #P.prop2!modify : <Self where Self : P> (inout Self) -> () -> () : $@yield_once @convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   ([[REG12:%.*]], [[REG13:%.*]]) = begin_apply [[REG11]]<τ_0_0>([[REG10]]) : $@yield_once @convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   [[REG14:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG15:%.*]] = apply [[REG14]]<(τ_0_0).Element>([[REG12]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG16:%.*]] = end_apply [[REG13]] as $()
// CHECK:   end_access [[REG10]]
// CHECK:   [[REG18:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG19:%.*]] = witness_method $τ_0_0, #P.prop3!modify : <Self where Self : P> (inout Self) -> () -> () : $@yield_once @convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   ([[REG20:%.*]], [[REG21:%.*]]) = begin_apply [[REG19]]<τ_0_0>([[REG18]]) : $@yield_once @convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   [[REG22:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG23:%.*]] = apply [[REG22]]<(τ_0_0).Element>([[REG20]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG24:%.*]] = end_apply [[REG21]] as $()
// CHECK:   end_access [[REG18]]
// CHECK:   [[REG26:%.*]] = tuple ()
// CHECK:   return [[REG26]]
// CHECK: }
func testP2(_ p: inout some P) {
  // For Copyable types, all opaque modify accesses go through modify
  // Conforming types can synthesize the modify from stored property, _modify or mutate
  mutate(&p.prop1)
  mutate(&p.prop2)
  mutate(&p.prop3)
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis6testP3yyxzAA1PRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> () {
// CHECK: bb0([[REG0:%.*]] : $*τ_0_0):
// CHECK:   [[REG2:%.*]] = begin_access [read] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = alloc_stack $(τ_0_0).Element
// CHECK:   [[REG4:%.*]] = witness_method $τ_0_0, #P.prop1!getter : <Self where Self : P> (Self) -> () -> Self.Element : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG5:%.*]] = apply [[REG4]]<τ_0_0>([[REG3]], [[REG2]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   end_access [[REG2]]
// CHECK:   [[REG7:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG8:%.*]] = witness_method $τ_0_0, #P.prop1!setter : <Self where Self : P> (inout Self) -> (Self.Element) -> () : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in τ_0_0.Element, @inout τ_0_0) -> ()
// CHECK:   [[REG9:%.*]] = apply [[REG8]]<τ_0_0>([[REG3]], [[REG7]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in τ_0_0.Element, @inout τ_0_0) -> ()
// CHECK:   end_access [[REG7]]
// CHECK:   dealloc_stack [[REG3]]
// CHECK:   [[REG12:%.*]] = begin_access [read] [unknown] [[REG0]]
// CHECK:   [[REG13:%.*]] = alloc_stack $(τ_0_0).Element
// CHECK:   [[REG14:%.*]] = witness_method $τ_0_0, #P.prop2!getter : <Self where Self : P> (Self) -> () -> Self.Element : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG15:%.*]] = apply [[REG14]]<τ_0_0>([[REG13]], [[REG12]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   end_access [[REG12]]
// CHECK:   [[REG17:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG18:%.*]] = witness_method $τ_0_0, #P.prop2!setter : <Self where Self : P> (inout Self) -> (Self.Element) -> () : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in τ_0_0.Element, @inout τ_0_0) -> ()
// CHECK:   [[REG19:%.*]] = apply [[REG18]]<τ_0_0>([[REG13]], [[REG17]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in τ_0_0.Element, @inout τ_0_0) -> ()
// CHECK:   end_access [[REG17]]
// CHECK:   dealloc_stack [[REG13]]
// CHECK:   [[REG22:%.*]] = begin_access [read] [unknown] [[REG0]]
// CHECK:   [[REG23:%.*]] = alloc_stack $(τ_0_0).Element
// CHECK:   [[REG24:%.*]] = witness_method $τ_0_0, #P.prop3!getter : <Self where Self : P> (Self) -> () -> Self.Element : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   [[REG25:%.*]] = apply [[REG24]]<τ_0_0>([[REG23]], [[REG22]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0.Element
// CHECK:   end_access [[REG22]]
// CHECK:   [[REG27:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG28:%.*]] = witness_method $τ_0_0, #P.prop3!setter : <Self where Self : P> (inout Self) -> (Self.Element) -> () : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in τ_0_0.Element, @inout τ_0_0) -> ()
// CHECK:   [[REG29:%.*]] = apply [[REG28]]<τ_0_0>([[REG23]], [[REG27]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in τ_0_0.Element, @inout τ_0_0) -> ()
// CHECK:   end_access [[REG27]]
// CHECK:   dealloc_stack [[REG23]]
// CHECK:   [[REG32:%.*]] = tuple ()
// CHECK:   return [[REG32]]
// CHECK: }
func testP3(_ p: inout some P) {
  // For Copyable types, all opaque set accesses go through set
  // Conforming types can synthesize the set from stored property, setter, _modify or mutate
  p.prop1 = p.prop1
  p.prop2 = p.prop2
  p.prop3 = p.prop3
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis8testNCP1yyxAA3NCPRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : NCP> (@in_guaranteed τ_0_0) -> () {
// CHECK: bb0([[REG0:%.*]] : $*τ_0_0):
// CHECK:   [[REG2:%.*]] = witness_method $τ_0_0, #NCP.prop1!read : <Self where Self : NCP, Self : ~Copyable> (Self) -> () -> () : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   ([[REG3:%.*]], [[REG4:%.*]]) = begin_apply [[REG2]]<τ_0_0>([[REG0]]) : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   [[REG5:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG3]]
// CHECK:   [[REG6:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<(τ_0_0).Element>([[REG5]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG8:%.*]] = end_apply [[REG4]] as $()
// CHECK:   [[REG9:%.*]] = witness_method $τ_0_0, #NCP.prop2!read : <Self where Self : NCP, Self : ~Copyable> (Self) -> () -> () : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   ([[REG10:%.*]], [[REG11:%.*]]) = begin_apply [[REG9]]<τ_0_0>([[REG0]]) : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   [[REG12:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG10]]
// CHECK:   [[REG13:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG14:%.*]] = apply [[REG13]]<(τ_0_0).Element>([[REG12]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG15:%.*]] = end_apply [[REG11]] as $()
// CHECK:   [[REG16:%.*]] = witness_method $τ_0_0, #NCP.prop3!read : <Self where Self : NCP, Self : ~Copyable> (Self) -> () -> () : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   [[REG19:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG17]]
// CHECK:   [[REG20:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG21:%.*]] = apply [[REG20]]<(τ_0_0).Element>([[REG19]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG22:%.*]] = end_apply [[REG18]] as $()
// CHECK: }
func testNCP1(_ p: some NCP) {
  use(p.prop1)
  use(p.prop2)
  use(p.prop3)
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis8testNCP2yyxzAA3NCPRzlF : $@convention(thin) <τ_0_0 where τ_0_0 : NCP> (@inout τ_0_0) -> () {
// CHECK: bb0([[REG0:%.*]] : $*τ_0_0):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = witness_method $τ_0_0, #NCP.prop1!modify : <Self where Self : NCP, Self : ~Copyable> (inout Self) -> () -> () : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   ([[REG4:%.*]], [[REG5:%.*]]) = begin_apply [[REG3]]<τ_0_0>([[REG2]]) : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   [[REG6:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG4]]
// CHECK:   [[REG7:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG8:%.*]] = apply [[REG7]]<(τ_0_0).Element>([[REG6]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG9:%.*]] = end_apply [[REG5]] as $()
// CHECK:   end_access [[REG2]]
// CHECK:   [[REG11:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG12:%.*]] = witness_method $τ_0_0, #NCP.prop2!modify : <Self where Self : NCP, Self : ~Copyable> (inout Self) -> () -> () : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   ([[REG13:%.*]], [[REG14:%.*]]) = begin_apply [[REG12]]<τ_0_0>([[REG11]]) : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   [[REG15:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG13]]
// CHECK:   [[REG16:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG17:%.*]] = apply [[REG16]]<(τ_0_0).Element>([[REG15]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG18:%.*]] = end_apply [[REG14]] as $()
// CHECK:   end_access [[REG11]]
// CHECK:   [[REG20:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG21:%.*]] = witness_method $τ_0_0, #NCP.prop3!modify : <Self where Self : NCP, Self : ~Copyable> (inout Self) -> () -> () : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   ([[REG22:%.*]], [[REG23:%.*]]) = begin_apply [[REG21]]<τ_0_0>([[REG20]]) : $@yield_once @convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@inout τ_0_0) -> @yields @inout τ_0_0.Element
// CHECK:   [[REG24:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG22]]
// CHECK:   [[REG25:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG26:%.*]] = apply [[REG25]]<(τ_0_0).Element>([[REG24]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG27:%.*]] = end_apply [[REG23]] as $()
// CHECK:   end_access [[REG20]]
// CHECK:   [[REG29:%.*]] = tuple ()
// CHECK:   return [[REG29]]
// CHECK: }
func testNCP2(_ p: inout some NCP) {
  mutate(&p.prop1)
  mutate(&p.prop2)
  mutate(&p.prop3)
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis6testW1yyAA1WVyxGlF : $@convention(thin) <T> (@in_guaranteed W<T>) -> () {
// CHECK: bb0([[REG0:%.*]] : $*W<T>):
// CHECK:   [[REG2:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop1xvb : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG3:%.*]] = apply [[REG2]]<T>([[REG0]]) : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG4:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG3]] to [init] [[REG4]]
// CHECK:   [[REG6:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<T>([[REG4]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG4]]
// CHECK:   dealloc_stack [[REG4]]
// CHECK:   [[REG10:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop2xvr : $@yield_once @convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   ([[REG11:%.*]], [[REG12:%.*]]) = begin_apply [[REG10]]<T>([[REG0]]) : $@yield_once @convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   [[REG13:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG11]] to [init] [[REG13]]
// CHECK:   [[REG15:%.*]] = end_apply [[REG12]] as $()
// CHECK:   [[REG16:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG17:%.*]] = apply [[REG16]]<T>([[REG13]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG13]]
// CHECK:   dealloc_stack [[REG13]]
// CHECK:   [[REG20:%.*]] = alloc_stack $T
// CHECK:   [[REG21:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop3xvg : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @out τ_0_0
// CHECK:   [[REG22:%.*]] = apply [[REG21]]<T>([[REG20]], [[REG0]]) : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @out τ_0_0
// CHECK:   [[REG23:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG24:%.*]] = apply [[REG23]]<T>([[REG20]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG20]]
// CHECK:   dealloc_stack [[REG20]]
// CHECK:   [[REG27:%.*]] = tuple ()
// CHECK:   return [[REG27]]
// CHECK: }
func testW1<T>(_ p: W<T>) {
  use(p.prop1) // resolves to borrow
  use(p.prop2) // resolves to _read
  use(p.prop3) // resolves to getter
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis6testW2yyAA1WVyxGzlF : $@convention(thin) <T> (@inout W<T>) -> () {
// CHECK: bb0([[REG0:%.*]] : $*W<T>):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop1xvz : $@convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[REG5:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG6:%.*]] = apply [[REG5]]<T>([[REG4]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   end_access [[REG2]]
// CHECK:   [[REG8:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG9:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop2xvM : $@yield_once @convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:   ([[REG10:%.*]], [[REG11:%.*]]) = begin_apply [[REG9]]<T>([[REG8]]) : $@yield_once @convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:   [[REG12:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG13:%.*]] = apply [[REG12]]<T>([[REG10]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG14:%.*]] = end_apply [[REG11]] as $()
// CHECK:   end_access [[REG8]]
// CHECK:   [[REG16:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG17:%.*]] = alloc_stack $T
// CHECK:   [[REG18:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop3xvg : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @out τ_0_0
// CHECK:   [[REG19:%.*]] = apply [[REG18]]<T>([[REG17]], [[REG16]]) : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @out τ_0_0
// CHECK:   [[REG20:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG21:%.*]] = apply [[REG20]]<T>([[REG17]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG22:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop3xvs : $@convention(method) <τ_0_0> (@in τ_0_0, @inout W<τ_0_0>) -> ()
// CHECK:   [[REG23:%.*]] = apply [[REG22]]<T>([[REG17]], [[REG16]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout W<τ_0_0>) -> ()
// CHECK:   end_access [[REG16]]
// CHECK:   dealloc_stack [[REG17]]
// CHECK: }
func testW2<T>(_ p: inout W<T>) {
  mutate(&p.prop1) // resolves to mutate
  mutate(&p.prop2) // resolves to _modify
  mutate(&p.prop3) // resolves to get, set, _modify
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis6testW3yyAA1WVyxGzlF : $@convention(thin) <T> (@inout W<T>) -> () {
// CHECK: bb0([[REG0:%.*]] : $*W<T>):
// CHECK:   [[REG2:%.*]] = begin_access [read] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop1xvb : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG5:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG4]] to [init] [[REG5]]
// CHECK:   end_access [[REG2]]
// CHECK:   [[REG8:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG9:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop1xvz : $@convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[REG10:%.*]] = apply [[REG9]]<T>([[REG8]]) : $@convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @inout τ_0_0
// CHECK:   copy_addr [take] [[REG5]] to [[REG10]]
// CHECK:   end_access [[REG8]]
// CHECK:   dealloc_stack [[REG5]]
// CHECK:   [[REG14:%.*]] = begin_access [read] [unknown] [[REG0]]
// CHECK:   [[REG15:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop2xvr : $@yield_once @convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   ([[REG16:%.*]], [[REG17:%.*]]) = begin_apply [[REG15]]<T>([[REG14]]) : $@yield_once @convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   [[REG18:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG16]] to [init] [[REG18]]
// CHECK:   [[REG20:%.*]] = end_apply [[REG17]] as $()
// CHECK:   end_access [[REG14]]
// CHECK:   [[REG22:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG23:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop2xvM : $@yield_once @convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:   ([[REG24:%.*]], [[REG25:%.*]]) = begin_apply [[REG23]]<T>([[REG22]]) : $@yield_once @convention(method) <τ_0_0> (@inout W<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:   copy_addr [take] [[REG18]] to [[REG24]]
// CHECK:   [[REG27:%.*]] = end_apply [[REG25]] as $()
// CHECK:   end_access [[REG22]]
// CHECK:   dealloc_stack [[REG18]]
// CHECK:   [[REG30:%.*]] = begin_access [read] [unknown] [[REG0]]
// CHECK:   [[REG31:%.*]] = alloc_stack $T
// CHECK:   [[REG32:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop3xvg : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @out τ_0_0
// CHECK:   [[REG33:%.*]] = apply [[REG32]]<T>([[REG31]], [[REG30]]) : $@convention(method) <τ_0_0> (@in_guaranteed W<τ_0_0>) -> @out τ_0_0
// CHECK:   end_access [[REG30]]
// CHECK:   [[REG35:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG36:%.*]] = function_ref @$s25borrow_accessor_synthesis1WV5prop3xvs : $@convention(method) <τ_0_0> (@in τ_0_0, @inout W<τ_0_0>) -> ()
// CHECK:   [[REG37:%.*]] = apply [[REG36]]<T>([[REG31]], [[REG35]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @inout W<τ_0_0>) -> ()
// CHECK:   end_access [[REG35]]
// CHECK:   dealloc_stack [[REG31]]
// CHECK: }
func testW3<T>(_ p: inout W<T>) {
  p.prop1 = p.prop1
  p.prop2 = p.prop2
  p.prop3 = p.prop3
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis7testNC1yyAA2NCVyxGlF : $@convention(thin) <T> (@in_guaranteed NC<T>) -> () {
// CHECK: bb0([[REG0:%.*]] : $*NC<T>):
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s25borrow_accessor_synthesis2NCVAARi_zrlE5prop1xvb : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed NC<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<T>([[REG2]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed NC<τ_0_0>) -> @guaranteed_address τ_0_0
// CHECK:   [[REG5:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG4]] to [init] [[REG5]]
// CHECK:   [[REG7:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG8:%.*]] = apply [[REG7]]<T>([[REG5]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG5]]
// CHECK:   dealloc_stack [[REG5]]
// CHECK:   [[REG11:%.*]] = function_ref @$s25borrow_accessor_synthesis2NCVAARi_zrlE5prop2xvr : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed NC<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   ([[REG12:%.*]], [[REG13:%.*]]) = begin_apply [[REG11]]<T>([[REG2]]) : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed NC<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   [[REG14:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG12]] to [init] [[REG14]]
// CHECK:   [[REG16:%.*]] = end_apply [[REG13]] as $()
// CHECK:   [[REG17:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG18:%.*]] = apply [[REG17]]<T>([[REG14]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG14]]
// CHECK:   dealloc_stack [[REG14]]
// CHECK:   [[REG21:%.*]] = struct_element_addr [[REG2]], #NC.prop3
// CHECK:   [[REG22:%.*]] = alloc_stack $T
// CHECK:   copy_addr [[REG21]] to [init] [[REG22]]
// CHECK:   [[REG24:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG25:%.*]] = apply [[REG24]]<T>([[REG22]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   destroy_addr [[REG22]]
// CHECK:   dealloc_stack [[REG22]]
// CHECK:   [[REG28:%.*]] = tuple ()
// CHECK:   return [[REG28]]
// CHECK: }
func testNC1<T>(_ p: borrowing NC<T>) {
  use(p.prop1) // resolves to borrow
  use(p.prop2) // resolves to _read
  use(p.prop3) // resolves to stored property
}

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis7testNC2yyAA2NCVyxGzlF : $@convention(thin) <T> (@inout NC<T>) -> () {
// CHECK: bb0([[REG0:%.*]] : $*NC<T>):
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [consumable_and_assignable] [[REG0]]
// CHECK:   [[REG3:%.*]] = begin_access [modify] [unknown] [[REG2]]
// CHECK:   [[REG4:%.*]] = function_ref @$s25borrow_accessor_synthesis2NCVAARi_zrlE5prop1xvz : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout NC<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[REG5:%.*]] = apply [[REG4]]<T>([[REG3]]) : $@convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout NC<τ_0_0>) -> @inout τ_0_0
// CHECK:   [[REG6:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<T>([[REG5]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   end_access [[REG3]]
// CHECK:   [[REG9:%.*]] = begin_access [modify] [unknown] [[REG2]]
// CHECK:   [[REG10:%.*]] = function_ref @$s25borrow_accessor_synthesis2NCVAARi_zrlE5prop2xvM : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout NC<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:   ([[REG11:%.*]], [[REG12:%.*]]) = begin_apply [[REG10]]<T>([[REG9]]) : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (@inout NC<τ_0_0>) -> @yields @inout τ_0_0
// CHECK:   [[REG13:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG14:%.*]] = apply [[REG13]]<T>([[REG11]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG15:%.*]] = end_apply [[REG12]] as $()
// CHECK:   end_access [[REG9]]
// CHECK:   [[REG17:%.*]] = begin_access [modify] [unknown] [[REG2]]
// CHECK:   [[REG18:%.*]] = struct_element_addr [[REG17]], #NC.prop3
// CHECK:   [[REG19:%.*]] = function_ref @$s25borrow_accessor_synthesis6mutateyyxzRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   [[REG20:%.*]] = apply [[REG19]]<T>([[REG18]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@inout τ_0_0) -> ()
// CHECK:   end_access [[REG17]]
// CHECK: }
func testNC2<T>(_ p: inout NC<T>) {
  mutate(&p.prop1) // resolves to mutate
  mutate(&p.prop2) // resolves to _modify
  mutate(&p.prop3) // resolves to stored property
}

public protocol Container: ~Copyable {
  associatedtype Element: ~Copyable
  subscript(index: Int) -> Element { get set }
}

public struct RigidWrapper<T: ~Copyable>: Container & ~Copyable {
  var _element: T

  public subscript(index: Int) -> T {
    borrow {
      return _element
    }
    mutate {
      return &_element
    }
  }
}

// CHECK: sil [ossa] @$s25borrow_accessor_synthesis12RigidWrapperVAARi_zrlEyxSicib : $@convention(method) <T where T : ~Copyable> (Int, @in_guaranteed RigidWrapper<T>) -> @guaranteed_address T {
// CHECK: bb0(%0 : $Int, %1 : $*RigidWrapper<T>):
// CHECK:   %4 = mark_unresolved_non_copyable_value [no_consume_or_assign] %1
// CHECK:   %5 = struct_element_addr %4, #RigidWrapper._element
// CHECK:   return %5
// CHECK: }

// CHECK: sil private [transparent] [thunk] [ossa] @$s25borrow_accessor_synthesis12RigidWrapperVyxGAA9ContainerAARi_zrlAaEPy7ElementQzSicirTW : $@yield_once @convention(witness_method: Container) <τ_0_0 where τ_0_0 : ~Copyable> @substituted <τ_0_0, τ_0_1> (Int, @in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_1 for <RigidWrapper<τ_0_0>, τ_0_0> {
// CHECK: bb0([[REG0:%.*]] : $Int, [[REG1:%.*]] : $*RigidWrapper<τ_0_0>):
// CHECK:   [[REG2:%.*]] = function_ref @$s25borrow_accessor_synthesis12RigidWrapperVAARi_zrlEyxSicir : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @in_guaranteed RigidWrapper<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   ([[REG3:%.*]], [[REG4:%.*]]) = begin_apply [[REG2]]<τ_0_0>([[REG0]], [[REG1]]) : $@yield_once @convention(method) <τ_0_0 where τ_0_0 : ~Copyable> (Int, @in_guaranteed RigidWrapper<τ_0_0>) -> @yields @in_guaranteed τ_0_0
// CHECK:   yield [[REG3]], resume bb1, unwind bb2
// CHECK: bb1:
// CHECK:   [[REG6:%.*]] = end_apply [[REG4]] as $()
// CHECK:   [[REG7:%.*]] = tuple ()
// CHECK:   return [[REG7]]
// CHECK: bb2:
// CHECK:   abort_apply [[REG4]]
// CHECK:   unwind
// CHECK: }

// CHECK: sil hidden [ossa] @$s25borrow_accessor_synthesis13testContaineryyx_SitAA0E0RzlF : $@convention(thin) <T where T : Container> (@in_guaranteed T, Int) -> () {
// CHECK: bb0([[REG0:%.*]] : $*T, [[REG1:%.*]] : $Int):
// CHECK:   [[REG4:%.*]] = witness_method $T, #Container.subscript!read : <Self where Self : Container, Self : ~Copyable> (Self) -> (Int) -> () : $@yield_once @convention(witness_method: Container) <τ_0_0 where τ_0_0 : Container, τ_0_0 : ~Copyable> (Int, @in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   ([[REG5:%.*]], [[REG6:%.*]]) = begin_apply [[REG4]]<T>([[REG1]], [[REG0]]) : $@yield_once @convention(witness_method: Container) <τ_0_0 where τ_0_0 : Container, τ_0_0 : ~Copyable> (Int, @in_guaranteed τ_0_0) -> @yields @in_guaranteed τ_0_0.Element
// CHECK:   [[REG7:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG5]]
// CHECK:   [[REG8:%.*]] = function_ref @$s25borrow_accessor_synthesis3useyyxRi_zlF : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG9:%.*]] = apply [[REG8]]<T.Element>([[REG7]]) : $@convention(thin) <τ_0_0 where τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> ()
// CHECK:   [[REG10:%.*]] = end_apply [[REG6]] as $()
// CHECK:   [[REG11:%.*]] = tuple ()
// CHECK:   return [[REG11]]
// CHECK: }
func testContainer<T: Container>(_ c: T, _ index: Int) {
  use(c[index]) // resolved to subscript.read which maybe synthesized using borrow accessor for conformance
}
