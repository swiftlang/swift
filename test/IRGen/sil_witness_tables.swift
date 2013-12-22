// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -emit-sil-protocol-witness-tables | FileCheck %s

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

struct AssocConformer: A {}

struct Conformer: Q {
  typealias Assoc = AssocConformer

  static func staticMethod() {}
  func instanceMethod() {}
  func qMethod() {}
}

// CHECK: [[P_WITNESS_TABLE:@witness_table.*]] = internal constant [4 x i8*] [
// -- FIXME: associated type and witness table
// CHECK:   i8* null,
// CHECK:   i8* null,
// CHECK:   i8* bitcast (void (%swift.type*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1PS_FS1_12staticMethodU_fMQPS1_FT_T_ to i8*),
// CHECK:   i8* bitcast (void (%V18sil_witness_tables9Conformer*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1PS_FS1_14instanceMethodU_fRQPS1_FT_T_ to i8*)
// CHECK: ]
// CHECK: [[Q_WITNESS_TABLE:@witness_table.*]] = internal constant [2 x i8*] 
// CHECK:   i8* bitcast ([4 x i8*]* [[P_WITNESS_TABLE]] to i8*),
// CHECK:   i8* bitcast (void (%V18sil_witness_tables9Conformer*, %swift.type*)* @_TTWV18sil_witness_tables9ConformerS_1QS_FS1_7qMethodU_fRQPS1_FT_T_ to i8*)
// CHECK: ]

// FIXME: Witness table generation shouldn't be lazy.
var q: Q = Conformer()
