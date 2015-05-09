// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module -o %t %S/sil_witness_tables_external_conformance.swift
// RUN: %target-swift-frontend -I %t -primary-file %s -emit-ir | FileCheck %s

// REQUIRES: CPU=x86_64

import sil_witness_tables_external_conformance

// FIXME: This should be a SIL test, but we can't parse sil_witness_tables
// yet.

protocol A {}

protocol P {
  typealias Assoc: A

  static func staticMethod()
  func instanceMethod()
}

protocol Q : P {
  func qMethod()
}

protocol QQ {
  func qMethod()
}

struct AssocConformer: A {}

struct Conformer: Q, QQ {
  typealias Assoc = AssocConformer

  static func staticMethod() {}
  func instanceMethod() {}
  func qMethod() {}
}

// CHECK: [[EXTERNAL_CONFORMER_EXTERNAL_P_WITNESS_TABLE:@_TWPV39sil_witness_tables_external_conformance17ExternalConformerS_9ExternalPS_]] = external global i8*
// CHECK: [[CONFORMER_P_WITNESS_TABLE:@_TWPV18sil_witness_tables9ConformerS_1PS_]] = hidden constant [4 x i8*] [
// -- FIXME: associated type and witness table
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* bitcast (void (%swift.type*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1PS_ZFS1_12staticMethoduRq_S1__fMq_FT_T_ to i8*),
// CHECK:   i8* bitcast (void (%V18sil_witness_tables9Conformer*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1PS_FS1_14instanceMethoduRq_S1__fq_FT_T_ to i8*)
// CHECK: ]
// CHECK: [[CONFORMER_Q_WITNESS_TABLE:@_TWPV18sil_witness_tables9ConformerS_1QS_]] = hidden constant [2 x i8*] [
// CHECK:   i8* bitcast ([4 x i8*]* [[CONFORMER_P_WITNESS_TABLE]] to i8*),
// CHECK:   i8* bitcast (void (%V18sil_witness_tables9Conformer*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1QS_FS1_7qMethoduRq_S1__fq_FT_T_ to i8*)
// CHECK: ]
// CHECK: [[CONFORMER2_P_WITNESS_TABLE:@_TWPV18sil_witness_tables10Conformer2S_1PS_]] = hidden constant [4 x i8*]

struct Conformer2: Q {
  typealias Assoc = AssocConformer

  static func staticMethod() {}
  func instanceMethod() {}
  func qMethod() {}
}

// CHECK-LABEL: define hidden void @_TF18sil_witness_tables7erasureFT1cVS_9Conformer_PS_2QQ_(%P18sil_witness_tables2QQ_* noalias sret)
// CHECK:         [[WITNESS_TABLE_ADDR:%.*]] = getelementptr inbounds %P18sil_witness_tables2QQ_, %P18sil_witness_tables2QQ_* %0, i32 0, i32 2
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* [[CONFORMER_QQ_WITNESS_TABLE:@_TWP.*]], i32 0, i32 0), i8*** [[WITNESS_TABLE_ADDR]], align 8
func erasure(#c: Conformer) -> QQ {
  return c
}

// CHECK-LABEL: define hidden void @_TF18sil_witness_tables15externalErasureFT1cV39sil_witness_tables_external_conformance17ExternalConformer_PS0_9ExternalP_(%P39sil_witness_tables_external_conformance9ExternalP_* noalias sret)
// CHECK:         [[WITNESS_TABLE_ADDR:%.*]] = getelementptr inbounds %P39sil_witness_tables_external_conformance9ExternalP_, %P39sil_witness_tables_external_conformance9ExternalP_* %0, i32 0, i32 2
// CHECK-NEXT:    store i8** [[EXTERNAL_CONFORMER_EXTERNAL_P_WITNESS_TABLE]], i8*** %2, align 8
func externalErasure(#c: ExternalConformer) -> ExternalP {
  return c
}
