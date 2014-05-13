// RUN: %swift -target x86_64-apple-darwin10 -I %S -enable-source-import %s -emit-ir | FileCheck %s

import sil_witness_tables_external_conformance

// FIXME: This should be a SIL test, but we can't parse sil_witness_tables
// yet.

protocol A {}

protocol P {
  typealias Assoc: A

  class func staticMethod()
  func instanceMethod()
}

protocol Q : P {
  func qMethod()
}

struct AssocConformer: A {}

struct Conformer: Q {
  typealias Assoc = AssocConformer

  static func staticMethod() {}
  func instanceMethod() {}
  func qMethod() {}
}

// CHECK: [[EXTERNAL_CONFORMER_EXTERNAL_P_WITNESS_TABLE:@_TWPV39sil_witness_tables_external_conformance17ExternalConformerS_9ExternalP]] = external global i8*
// CHECK: [[CONFORMER_P_WITNESS_TABLE:@_TWPV18sil_witness_tables9ConformerS_1P]] = constant [4 x i8*] [
// -- FIXME: associated type and witness table
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* bitcast (void (%swift.type*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1PFS1_12staticMethodUS1__US_1A__fMQPS1_FT_T_ to i8*),
// CHECK:   i8* bitcast (void (%V18sil_witness_tables9Conformer*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1PFS1_14instanceMethodUS1__US_1A__fRQPS1_FT_T_ to i8*)
// CHECK: ]
// CHECK: [[CONFORMER_Q_WITNESS_TABLE:@_TWPV18sil_witness_tables9ConformerS_1Q]] = constant [2 x i8*] [
// CHECK:   i8* bitcast ([4 x i8*]* [[CONFORMER_P_WITNESS_TABLE]] to i8*),
// CHECK:   i8* bitcast (void (%V18sil_witness_tables9Conformer*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1QFS1_7qMethodUS1__US_1A__fRQPS1_FT_T_ to i8*)
// CHECK: ]
// CHECK: [[CONFORMER2_P_WITNESS_TABLE:@_TWPV18sil_witness_tables10Conformer2S_1P]] = constant [4 x i8*]

struct Conformer2: Q {
  typealias Assoc = AssocConformer

  static func staticMethod() {}
  func instanceMethod() {}
  func qMethod() {}
}

// CHECK-LABEL: define void @_TF18sil_witness_tables7erasureFT1cVS_9Conformer_PS_1Q_(%P18sil_witness_tables1Q_* noalias sret)
// CHECK:         [[WITNESS_TABLE_ADDR:%.*]] = getelementptr inbounds %P18sil_witness_tables1Q_* %0, i32 0, i32 2
// CHECK-NEXT:    store i8** getelementptr inbounds ([2 x i8*]* [[CONFORMER_Q_WITNESS_TABLE]], i32 0, i32 0), i8*** [[WITNESS_TABLE_ADDR]], align 8
func erasure(#c: Conformer) -> Q {
  return c
}

// CHECK-LABEL: define void @_TF18sil_witness_tables15externalErasureFT1cV39sil_witness_tables_external_conformance17ExternalConformer_PS0_9ExternalP_(%P39sil_witness_tables_external_conformance9ExternalP_* noalias sret)
// CHECK:         [[WITNESS_TABLE_ADDR:%.*]] = getelementptr inbounds %P39sil_witness_tables_external_conformance9ExternalP_* %0, i32 0, i32 2
// CHECK-NEXT:    store i8** [[EXTERNAL_CONFORMER_EXTERNAL_P_WITNESS_TABLE]], i8*** %2, align 8
func externalErasure(#c: ExternalConformer) -> ExternalP {
  return c
}
