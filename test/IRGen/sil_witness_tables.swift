// RUN: rm -rf %t && mkdir -p %t
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-module -o %t %S/sil_witness_tables_external_conformance.swift
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -I %t -primary-file %s -emit-ir | %FileCheck %s

// REQUIRES: CPU=x86_64

import sil_witness_tables_external_conformance

// FIXME: This should be a SIL test, but we can't parse sil_witness_tables
// yet.

protocol A {}

protocol P {
  associatedtype Assoc: A

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

// CHECK: [[EXTERNAL_CONFORMER_EXTERNAL_P_WITNESS_TABLE:@_T039sil_witness_tables_external_conformance17ExternalConformerVAA0F1PAAWP]] = external global i8*, align 8
// CHECK: [[CONFORMER_Q_WITNESS_TABLE:@_T018sil_witness_tables9ConformerVAA1QAAWP]] = hidden constant [2 x i8*] [
// CHECK:   i8* bitcast ([4 x i8*]* [[CONFORMER_P_WITNESS_TABLE:@_T018sil_witness_tables9ConformerVAA1PAAWP]] to i8*),
// CHECK:   i8* bitcast (void (%T18sil_witness_tables9ConformerV*, %swift.type*, i8**)* @_T018sil_witness_tables9ConformerVAA1QAaaDP7qMethod{{[_0-9a-zA-Z]*}}FTW to i8*)
// CHECK: ]
// CHECK: [[CONFORMER_P_WITNESS_TABLE]] = hidden constant [4 x i8*] [
// CHECK:   i8* bitcast (%swift.type* ()* @_T018sil_witness_tables14AssocConformerVMa to i8*),
// CHECK:   i8* bitcast (i8** ()* @_T018sil_witness_tables14AssocConformerVAA1AAAWa to i8*)
// CHECK:   i8* bitcast (void (%swift.type*, %swift.type*, i8**)* @_T018sil_witness_tables9ConformerVAA1PAaaDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW to i8*),
// CHECK:   i8* bitcast (void (%T18sil_witness_tables9ConformerV*, %swift.type*, i8**)* @_T018sil_witness_tables9ConformerVAA1PAaaDP14instanceMethod{{[_0-9a-zA-Z]*}}FTW to i8*)
// CHECK: ]
// CHECK: [[CONFORMER2_P_WITNESS_TABLE:@_T018sil_witness_tables10Conformer2VAA1PAAWP]] = hidden constant [4 x i8*]

struct Conformer2: Q {
  typealias Assoc = AssocConformer

  static func staticMethod() {}
  func instanceMethod() {}
  func qMethod() {}
}

// CHECK-LABEL: define hidden swiftcc void @_T018sil_witness_tables7erasureAA2QQ_pAA9ConformerV1c_tF(%T18sil_witness_tables2QQP* noalias nocapture sret)
// CHECK:         [[WITNESS_TABLE_ADDR:%.*]] = getelementptr inbounds %T18sil_witness_tables2QQP, %T18sil_witness_tables2QQP* %0, i32 0, i32 2
// CHECK-NEXT:    store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* [[CONFORMER_QQ_WITNESS_TABLE:@_T0.*WP]], i32 0, i32 0), i8*** [[WITNESS_TABLE_ADDR]], align 8
func erasure(c c: Conformer) -> QQ {
  return c
}

// CHECK-LABEL: define hidden swiftcc void @_T018sil_witness_tables15externalErasure0a1_b1_c1_D12_conformance9ExternalP_pAC0G9ConformerV1c_tF(%T39sil_witness_tables_external_conformance9ExternalPP* noalias nocapture sret)
// CHECK:         [[WITNESS_TABLE_ADDR:%.*]] = getelementptr inbounds %T39sil_witness_tables_external_conformance9ExternalPP, %T39sil_witness_tables_external_conformance9ExternalPP* %0, i32 0, i32 2
// CHECK-NEXT:    store i8** [[EXTERNAL_CONFORMER_EXTERNAL_P_WITNESS_TABLE]], i8*** %2, align 8
func externalErasure(c c: ExternalConformer) -> ExternalP {
  return c
}

// FIXME: why do these have different linkages?

// CHECK-LABEL: define hidden %swift.type* @_T018sil_witness_tables14AssocConformerVMa()
// CHECK:         ret %swift.type* bitcast (i64* getelementptr inbounds {{.*}} @_T018sil_witness_tables14AssocConformerVMf, i32 0, i32 1) to %swift.type*)

// CHECK-LABEL: define hidden i8** @_T018sil_witness_tables9ConformerVAA1PAAWa()
// CHECK:         ret i8** getelementptr inbounds ([4 x i8*], [4 x i8*]* @_T018sil_witness_tables9ConformerVAA1PAAWP, i32 0, i32 0)
