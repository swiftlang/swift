// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/opaque_conformance.swiftmodule -primary-file %S/Inputs/opaque_conformance.swift
// RUN: %target-swift-frontend -O -Xllvm -sil-print-types -emit-sil -primary-file %s -I %t -Xllvm -sil-disable-pass=late-deadfuncelim | %FileCheck %s

import opaque_conformance

public func callsPublicRequirement(_ c: Conformer) {
  c.publicRequirement()
}

// CHECK-LABEL: sil @$s21devirt_opaque_witness22callsPublicRequirementyy0B12_conformance9ConformerVF : $@convention(thin) (Conformer) -> () {
// CHECK:    bb0(%0 : $Conformer):
// CHECK:      [[BOX:%.*]] = alloc_stack $Conformer
// CHECK-NEXT: store %0 to [[BOX]] : $*Conformer
// CHECK:      [[FN:%.*]] = function_ref @$s18opaque_conformance9ConformerVAA14PublicProtocolA2aDP17publicRequirementyyFTW :
// CHECK-NEXT: [[RESULT:%.*]] = apply [[FN]]([[BOX]]) :
// CHECK-NEXT: dealloc_stack [[BOX]] : $*Conformer
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()

// Note that [transparent] here doesn't mean anything since there's no body.
// The important thing is that the witness_method call got devirtualized,
// and the thunk has public linkage and no serialized body (since the body
// references a private symbol of the module opaque_conformance).
// CHECK-LABEL: sil [transparent] [thunk] @$s18opaque_conformance9ConformerVAA14PublicProtocolA2aDP17publicRequirementyyFTW :

// CHECK-LABEL: sil_witness_table public_external Conformer: PublicProtocol module opaque_conformance {
// CHECK-NEXT:    method #PublicProtocol.publicRequirement: <Self where Self : PublicProtocol> (Self) -> () -> () : @$s18opaque_conformance9ConformerVAA14PublicProtocolA2aDP17publicRequirementyyFTW
// CHECK-NEXT: }
