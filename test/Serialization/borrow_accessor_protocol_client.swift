// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-module -module-name borrow_accessor_protocol -enable-experimental-feature BorrowAndMutateAccessors -o %t %S/Inputs/borrow_accessor_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-silgen %s -verify | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

import borrow_accessor_protocol

public func use(_ s: NonTrivial) {
  print(s)
}

public func use(_ s: borrowing NC) {
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C01P_pF :
// CHECK: bb0([[REG0:%.*]] : $*any P):
// CHECK:   [[REG2:%.*]] = open_existential_addr immutable_access [[REG0]] to $*@opened("{{.*}}", any P) Self
// CHECK:   [[REG3:%.*]] = witness_method $@opened("{{.*}}", any P) Self, #P.id!borrow : <Self where Self : borrow_accessor_protocol.P> (Self) -> () -> borrow_accessor_protocol.NonTrivial, [[REG2:%.*]] : $*@opened("{{.*}}", any P) Self : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @guaranteed_address NonTrivial
// CHECK:   [[REG4:%.*]] = apply [[REG3]]<@opened("{{.*}}", any P) Self>([[REG2]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @guaranteed_address NonTrivial
// CHECK: }
public func foo(_ p: P) {
  use(p.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S1VF :
// CHECK: bb0([[REG0:%.*]] : $*S1):
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S1V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S1) -> @guaranteed_address NonTrivial
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@in_guaranteed S1) -> @guaranteed_address NonTrivial
// CHECK: }
public func foo(_ s1: S1) {
  use(s1.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S2VF :
// CHECK: bb0([[REG0:%.*]] : $*S2):
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol2S2V2idAA10NonTrivialVvg : $@convention(method) (@in_guaranteed S2) -> @out NonTrivial
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]], [[REG0]]) : $@convention(method) (@in_guaranteed S2) -> @out NonTrivial
// CHECK: }
public func foo(_ s2: S2) {
  use(s2.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S3VF :
// CHECK: bb0([[REG0:%.*]] : $*S3):
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S3V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S3) -> @guaranteed_address NonTrivial
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@in_guaranteed S3) -> @guaranteed_address NonTrivial
// CHECK: }
public func foo(_ s3: S3) {
  use(s3.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S4VF :
// CHECK: bb0([[REG0:%.*]] : $*S4):
// CHECK:   [[REG2:%.*]] = struct_element_addr [[REG0]], #S4.id
// CHECK: }
public func foo(_ s4: S4) {
  use(s4.id)
}

public func mutate(_ s: inout NonTrivial) {
  s.k = Klass()
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C01P_pzF :
// CHECK: bb0([[REG0:%.*]] : $*any P):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = open_existential_addr mutable_access [[REG2]] to $*@opened("{{.*}}", any P) Self
// CHECK:   [[REG4:%.*]] = alloc_stack $NonTrivial
// CHECK:   [[REG5:%.*]] = witness_method $@opened("{{.*}}", any P) Self, #P.id!borrow : <Self where Self : borrow_accessor_protocol.P> (Self) -> () -> borrow_accessor_protocol.NonTrivial, [[REG3:%.*]] : $*@opened("{{.*}}", any P) Self : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @guaranteed_address NonTrivial
// CHECK:   [[REG6:%.*]] = apply [[REG5]]<@opened("{{.*}}", any P) Self>([[REG3]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @guaranteed_address NonTrivial
// CHECK:   copy_addr [[REG6]] to [init] [[REG4]]
// CHECK:   [[REG8:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG9:%.*]] = apply [[REG8]]([[REG4]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG10:%.*]] = witness_method $@opened("{{.*}}", any P) Self, #P.id!mutate : <Self where Self : borrow_accessor_protocol.P> (inout Self) -> () -> borrow_accessor_protocol.NonTrivial, [[REG3:%.*]] : $*@opened("{{.*}}", any P) Self : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @inout NonTrivial
// CHECK:   [[REG11:%.*]] = apply [[REG10]]<@opened("{{.*}}", any P) Self>([[REG3]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@inout τ_0_0) -> @inout NonTrivial
// CHECK: }
public func bar(_ p: inout P) {
  mutate(&p.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C02S1VzF :
// CHECK: bb0([[REG0:%.*]] : $*S1):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = alloc_stack $NonTrivial
// CHECK:   [[REG4:%.*]] = function_ref @$s24borrow_accessor_protocol2S1V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S1) -> @guaranteed_address NonTrivial
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]]) : $@convention(method) (@in_guaranteed S1) -> @guaranteed_address NonTrivial
// CHECK:   copy_addr [[REG5]] to [init] [[REG3]]
// CHECK:   [[REG7:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG8:%.*]] = apply [[REG7]]([[REG3]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK: }
public func bar(_ s1: inout S1) {
  mutate(&s1.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C02S2VzF :
// CHECK: bb0([[REG0:%.*]] : $*S2):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol2S2V2idAA10NonTrivialVvM : $@yield_once @convention(method) (@inout S2) -> @yields @inout NonTrivial
// CHECK:   ([[REG4]], [[REG5]]) = begin_apply [[REG3]]([[REG2]]) : $@yield_once @convention(method) (@inout S2) -> @yields @inout NonTrivial
// CHECK:   [[REG6:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG7:%.*]] = apply [[REG6]]([[REG4]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG8:%.*]] = end_apply [[REG5]] as $()
// CHECK:   end_access [[REG2]]
// CHECK: }
public func bar(_ s2: inout S2) {
  mutate(&s2.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C02S3VzF :
// CHECK: bb0([[REG0:%.*]] : $*S3):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol2S3V2idAA10NonTrivialVvz : $@convention(method) (@inout S3) -> @inout NonTrivial
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@inout S3) -> @inout NonTrivial
// CHECK:   [[REG5:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG6:%.*]] = apply [[REG5]]([[REG4]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   end_access [[REG2]]
// CHECK: }
public func bar(_ s3: inout S3) {
  mutate(&s3.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C02S4VzF :
// CHECK: bb0([[REG0:%.*]] : $*S4):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #S4.id
// CHECK:   [[REG4:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG3]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   end_access [[REG2]]
// CHECK: }
public func bar(_ s4: inout S4) {
  mutate(&s4.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C01Q_pF : $@convention(thin) (@in_guaranteed any Q) -> () {
// CHECK: bb0([[REG0:%.*]] : $*any Q):
// CHECK:   [[REG2:%.*]] = open_existential_addr immutable_access [[REG0]] to $*@opened("{{.*}}", any Q) Self
// CHECK:   [[REG3:%.*]] = alloc_stack $NonTrivial
// CHECK:   [[REG4:%.*]] = witness_method $@opened("{{.*}}", any Q) Self, #Q.id!getter : <Self where Self : borrow_accessor_protocol.Q> (Self) -> () -> borrow_accessor_protocol.NonTrivial, [[REG2:%.*]] : $*@opened("{{.*}}", any Q) Self : $@convention(witness_method: Q) <τ_0_0 where τ_0_0 : Q> (@in_guaranteed τ_0_0) -> @out NonTrivial
// CHECK:   [[REG5:%.*]] = apply [[REG4]]<@opened("{{.*}}", any Q) Self>([[REG3]], [[REG2]]) : $@convention(witness_method: Q) <τ_0_0 where τ_0_0 : Q> (@in_guaranteed τ_0_0) -> @out NonTrivial
// CHECK: }
public func foo(_ q: Q) {
  use(q.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S5VF : $@convention(thin) (@in_guaranteed S5) -> () {
// CHECK: bb0([[REG0:%.*]] : $*S5):
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S5V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S5) -> @guaranteed_address NonTrivial // user: [[REG3]]
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@in_guaranteed S5) -> @guaranteed_address NonTrivial // user: [[REG5]]
// CHECK: } // end sil function '$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S5VF'
public func foo(_ s5: S5) {
  use(s5.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C02S6VF : $@convention(thin) (@in_guaranteed S6) -> () {
// CHECK: bb0([[REG0:%.*]] : $*S6):
// CHECK:   [[REG2:%.*]] = function_ref @$s24borrow_accessor_protocol2S6V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S6) -> @guaranteed_address NonTrivial
// CHECK:   [[REG3:%.*]] = apply [[REG2]]([[REG0]]) : $@convention(method) (@in_guaranteed S6) -> @guaranteed_address NonTrivial
// CHECK: }
public func foo(_ s6: S6) {
  use(s6.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C01Q_pzF : $@convention(thin) (@inout any Q) -> () {
// CHECK: bb0([[REG0:%.*]] : $*any Q):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = open_existential_addr mutable_access [[REG2]] to $*@opened("{{.*}}", any Q) Self
// CHECK:   [[REG4:%.*]] = witness_method $@opened("{{.*}}", any Q) Self, #Q.id!modify : <Self where Self : borrow_accessor_protocol.Q> (inout Self) -> @yield_once () yields (inout borrow_accessor_protocol.NonTrivial) -> (), [[REG3:%.*]] : $*@opened("{{.*}}", any Q) Self : $@yield_once @convention(witness_method: Q) <τ_0_0 where τ_0_0 : Q> (@inout τ_0_0) -> @yields @inout NonTrivial
// CHECK:   ([[REG5]], [[REG6]]) = begin_apply [[REG4]]<@opened("{{.*}}", any Q) Self>([[REG3]]) : $@yield_once @convention(witness_method: Q) <τ_0_0 where τ_0_0 : Q> (@inout τ_0_0) -> @yields @inout NonTrivial
// CHECK:   [[REG7:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG8:%.*]] = apply [[REG7]]([[REG5]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG9:%.*]] = end_apply [[REG6]] as $()
// CHECK:   end_access [[REG2]]
// CHECK: }
public func bar(_ q: inout Q) {
  mutate(&q.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C02S5VzF : $@convention(thin) (@inout S5) -> () {
// CHECK: bb0([[REG0:%.*]] : $*S5):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = alloc_stack $NonTrivial
// CHECK:   [[REG4:%.*]] = function_ref @$s24borrow_accessor_protocol2S5V2idAA10NonTrivialVvb : $@convention(method) (@in_guaranteed S5) -> @guaranteed_address NonTrivial
// CHECK:   [[REG5:%.*]] = apply [[REG4]]([[REG2]]) : $@convention(method) (@in_guaranteed S5) -> @guaranteed_address NonTrivial
// CHECK:   copy_addr [[REG5]] to [init] [[REG3]]
// CHECK:   [[REG7:%.*]] = function_ref @$s31borrow_accessor_protocol_client6mutateyy0a1_b1_C010NonTrivialVzF : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG8:%.*]] = apply [[REG7]]([[REG3]]) : $@convention(thin) (@inout NonTrivial) -> ()
// CHECK:   [[REG9:%.*]] = function_ref @$s24borrow_accessor_protocol2S5V2idAA10NonTrivialVvz : $@convention(method) (@inout S5) -> @inout NonTrivial
// CHECK:   [[REG10:%.*]] = apply [[REG9]]([[REG2]]) : $@convention(method) (@inout S5) -> @inout NonTrivial
// CHECK:   copy_addr [take] [[REG3]] to [[REG10]]
// CHECK: }
public func bar(_ s5: inout S5) {
  mutate(&s5.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3baryy0a1_b1_C02S6VzF : $@convention(thin) (@inout S6) -> () {
// CHECK: bb0([[REG0:%.*]] : $*S6):
// CHECK:   [[REG2:%.*]] = begin_access [modify] [unknown] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol2S6V2idAA10NonTrivialVvz : $@convention(method) (@inout S6) -> @inout NonTrivial
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@inout S6) -> @inout NonTrivial
// CHECK: }
public func bar(_ s6: inout S6) {
  mutate(&s6.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C03NCP_pF : $@convention(thin) (@in_guaranteed any NCP) -> () {
// CHECK: bb0([[REG0:%.*]] : @noImplicitCopy $*any NCP):
// CHECK:   [[REG1:%.*]] = copyable_to_moveonlywrapper_addr [[REG0]]
// CHECK:   [[REG3:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG1]]
// CHECK:   [[REG4:%.*]] = moveonlywrapper_to_copyable_addr [[REG3]]
// CHECK:   [[REG5:%.*]] = open_existential_addr immutable_access [[REG4]] to $*@opened("{{.*}}", any NCP) Self
// CHECK:   [[REG6:%.*]] = witness_method $@opened("{{.*}}", any NCP) Self, #NCP.id!borrow : <Self where Self : borrow_accessor_protocol.NCP, Self : ~Copyable> (Self) -> () -> borrow_accessor_protocol.NC, [[REG5:%.*]] : $*@opened("{{.*}}", any NCP) Self : $@convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @guaranteed_address NC
// CHECK:   [[REG7:%.*]] = apply [[REG6]]<@opened("{{.*}}", any NCP) Self>([[REG5]]) : $@convention(witness_method: NCP) <τ_0_0 where τ_0_0 : NCP, τ_0_0 : ~Copyable> (@in_guaranteed τ_0_0) -> @guaranteed_address NC
// CHECK: }
public func foo(_ p: borrowing NCP) {
  use(p.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C010NCWrapper1VF : $@convention(thin) (@in_guaranteed NCWrapper1) -> () {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper1):
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol10NCWrapper1V2idAA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper1) -> @guaranteed_address NC
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@in_guaranteed NCWrapper1) -> @guaranteed_address NC
// CHECK: }
public func foo(_ nc1: borrowing NCWrapper1) {
  use(nc1.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C010NCWrapper2VF : $@convention(thin) (@in_guaranteed NCWrapper2) -> () {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper2):
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol10NCWrapper2V2idAA2NCVvr : $@yield_once @convention(method) (@in_guaranteed NCWrapper2) -> @yields @in_guaranteed NC
// CHECK:   ([[REG4]], [[REG5]]) = begin_apply [[REG3]]([[REG2]]) : $@yield_once @convention(method) (@in_guaranteed NCWrapper2) -> @yields @in_guaranteed NC
// CHECK: }
public func foo(_ nc2: borrowing NCWrapper2) {
  use(nc2.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C010NCWrapper3VF : $@convention(thin) (@in_guaranteed NCWrapper3) -> () {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper3):
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = function_ref @$s24borrow_accessor_protocol10NCWrapper3V2idAA2NCVvb : $@convention(method) (@in_guaranteed NCWrapper3) -> @guaranteed_address NC
// CHECK:   [[REG4:%.*]] = apply [[REG3]]([[REG2]]) : $@convention(method) (@in_guaranteed NCWrapper3) -> @guaranteed_address NC
// CHECK: }
public func foo(_ nc3: borrowing NCWrapper3) {
  use(nc3.id)
}

// CHECK: sil [ossa] @$s31borrow_accessor_protocol_client3fooyy0a1_b1_C010NCWrapper4VF : $@convention(thin) (@in_guaranteed NCWrapper4) -> () {
// CHECK: bb0([[REG0:%.*]] : $*NCWrapper4):
// CHECK:   [[REG2:%.*]] = mark_unresolved_non_copyable_value [no_consume_or_assign] [[REG0]]
// CHECK:   [[REG3:%.*]] = struct_element_addr [[REG2]], #NCWrapper4.id
// CHECK: }
public func foo(_ nc4: borrowing NCWrapper4) {
  use(nc4.id)
}
