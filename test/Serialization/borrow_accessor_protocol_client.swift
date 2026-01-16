// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -emit-module -module-name borrow_accessor_protocol -enable-experimental-feature BorrowAndMutateAccessors -o %t %S/Inputs/borrow_accessor_protocol.swift
// RUN: %target-swift-frontend -I %t -emit-silgen %s -verify | %FileCheck %s

// REQUIRES: swift_feature_BorrowAndMutateAccessors

import borrow_accessor_protocol

public func use(_ s: NonTrivial) {
  print(s)
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

