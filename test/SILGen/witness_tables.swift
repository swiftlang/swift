// RUN: %swift -emit-sil-protocol-witness-tables -emit-silgen -parse-stdlib %s | FileCheck -check-prefix=TABLE %s
// RUN: %swift -emit-sil-protocol-witness-tables -emit-silgen -parse-stdlib %s | FileCheck -check-prefix=SYMBOL %s

import witness_tables_b

struct Arg {}

operator infix <~> {}

protocol AssocReqt {
  func requiredMethod()
}

protocol ArchetypeReqt {
  func requiredMethod()
}

protocol AnyProtocol {
  typealias AssocType
  typealias AssocWithReqt: AssocReqt

  func method(x: Arg, y: Self)
  func generic<A: ArchetypeReqt>(x: A, y: Self)

  func assocTypesMethod(x: AssocType, y: AssocWithReqt)

  static func staticMethod(x: Self)

  @infix func <~>(x: Self, y: Self)
}

@class_protocol protocol ClassProtocol {

  typealias AssocType
  typealias AssocWithReqt: AssocReqt

  func method(x: Arg, y: Self)
  func generic<B: ArchetypeReqt>(x: B, y: Self)

  func assocTypesMethod(x: AssocType, y: AssocWithReqt)

  static func staticMethod(x: Self)

  @infix func <~>(x: Self, y: Self)
}

@objc @class_protocol protocol ObjCProtocol {
  typealias AssocType
  typealias AssocWithReqt: AssocReqt

  func method(x: Arg, y: Self)
  func generic<C: ArchetypeReqt>(x: C, y: Self)

  func assocTypesMethod(x: AssocType, y: AssocWithReqt)

  static func staticMethod(x: Self)

  @infix func <~>(x: Self, y: Self)
}

class SomeAssoc {}

struct ConformingAssoc : AssocReqt {
  func requiredMethod() {}
}
// TABLE:      sil_witness_table ConformingAssoc: AssocReqt module witness_tables {
// TABLE-NEXT:   method #AssocReqt.requiredMethod!1: @_TTWV14witness_tables15ConformingAssocS_9AssocReqtS_S1_14requiredMethodU_fRQPS1_FT_T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWV14witness_tables15ConformingAssocS_9AssocReqtS_S1_14requiredMethodU_fRQPS1_FT_T_ : $@cc(method) @thin (@inout ConformingAssoc) -> ()

struct ConformingStruct : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingStruct) {}
  func generic<D: ArchetypeReqt>(x: D, y: ConformingStruct) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: ConformingStruct) {}
}
func <~>(x: ConformingStruct, y: ConformingStruct) {}
// TABLE:      sil_witness_table ConformingStruct: AnyProtocol module witness_tables {
// TABLE-NEXT:   associated_type AssocType: SomeAssoc
// TABLE-NEXT:   associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:   associated_type_protocol (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:   method #AnyProtocol.method!1: @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.generic!1: @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.assocTypesMethod!1: @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_
// TABLE-NEXT:   method #AnyProtocol.staticMethod!1: @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_
// TABLE-NEXT:   method #AnyProtocol.<~>!1: @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_ : $@cc(method) @thin (Arg, @in ConformingStruct, @inout ConformingStruct) -> ()
// SYMBOL:     sil @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_ : $@cc(method) @thin <A : ArchetypeReqt> (@in A, @in ConformingStruct, @inout ConformingStruct) -> ()
// SYMBOL:     sil @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_ : $@cc(method) @thin (@in SomeAssoc, @in ConformingAssoc, @inout ConformingStruct) -> ()
// SYMBOL:     sil @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_ : $@thin (@in ConformingStruct, ConformingStruct.metatype) -> ()
// SYMBOL:     sil @_TTWV14witness_tables16ConformingStructS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_ : $@thin (@in ConformingStruct, @in ConformingStruct, ConformingStruct.metatype) -> ()

struct ConformingAddressOnlyStruct : AnyProtocol {
  var p: AnyProtocol // force address-only layout with a protocol-type field

  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingAddressOnlyStruct) {}
  func generic<E: ArchetypeReqt>(x: E, y: ConformingAddressOnlyStruct) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: ConformingAddressOnlyStruct) {}
}
func <~>(x: ConformingAddressOnlyStruct, y: ConformingAddressOnlyStruct) {}
// TABLE:      sil_witness_table ConformingAddressOnlyStruct: AnyProtocol module witness_tables {
// TABLE-NEXT:   associated_type AssocType: SomeAssoc
// TABLE-NEXT:   associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:   associated_type_protocol (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:   method #AnyProtocol.method!1: @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.generic!1: @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.assocTypesMethod!1: @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_
// TABLE-NEXT:   method #AnyProtocol.staticMethod!1: @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_
// TABLE-NEXT:   method #AnyProtocol.<~>!1: @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_ : $@cc(method) @thin (Arg, @in ConformingAddressOnlyStruct, @inout ConformingAddressOnlyStruct) -> ()
// SYMBOL:     sil @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_ : $@cc(method) @thin <A : ArchetypeReqt> (@in A, @in ConformingAddressOnlyStruct, @inout ConformingAddressOnlyStruct) -> ()
// SYMBOL:     sil @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_ : $@cc(method) @thin (@in SomeAssoc, @in ConformingAssoc, @inout ConformingAddressOnlyStruct) -> ()
// SYMBOL:     sil @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_ : $@thin (@in ConformingAddressOnlyStruct, ConformingAddressOnlyStruct.metatype) -> ()
// SYMBOL:     sil @_TTWV14witness_tables27ConformingAddressOnlyStructS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_ : $@thin (@in ConformingAddressOnlyStruct, @in ConformingAddressOnlyStruct, ConformingAddressOnlyStruct.metatype) -> ()

class ConformingClass : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingClass) {}
  func generic<F: ArchetypeReqt>(x: F, y: ConformingClass) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: ConformingClass) {}
}
func <~>(x: ConformingClass, y: ConformingClass) {}
// TABLE:      sil_witness_table ConformingClass: AnyProtocol module witness_tables {
// TABLE-NEXT:   associated_type AssocType: SomeAssoc
// TABLE-NEXT:   associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:   associated_type_protocol (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:   method #AnyProtocol.method!1: @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.generic!1: @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.assocTypesMethod!1: @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_
// TABLE-NEXT:   method #AnyProtocol.staticMethod!1: @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_
// TABLE-NEXT:   method #AnyProtocol.<~>!1: @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_ : $@cc(method) @thin (Arg, @in ConformingClass, @inout ConformingClass) -> ()
// SYMBOL:     sil @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_ : $@cc(method) @thin <A : ArchetypeReqt> (@in A, @in ConformingClass, @inout ConformingClass) -> ()
// SYMBOL:     sil @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_ : $@cc(method) @thin (@in SomeAssoc, @in ConformingAssoc, @inout ConformingClass) -> ()
// SYMBOL:     sil @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_ : $@thin (@in ConformingClass, ConformingClass.metatype) -> ()
// SYMBOL:     sil @_TTWC14witness_tables15ConformingClassS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_ : $@thin (@in ConformingClass, @in ConformingClass, ConformingClass.metatype) -> ()

struct ConformsByExtension {}
extension ConformsByExtension : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformsByExtension) {}
  func generic<G: ArchetypeReqt>(x: G, y: ConformsByExtension) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: ConformsByExtension) {}
}
func <~>(x: ConformsByExtension, y: ConformsByExtension) {}
// TABLE:      sil_witness_table ConformsByExtension: AnyProtocol module witness_tables {
// TABLE-NEXT:   associated_type AssocType: SomeAssoc
// TABLE-NEXT:   associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:   associated_type_protocol (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:   method #AnyProtocol.method!1: @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.generic!1: @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.assocTypesMethod!1: @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_
// TABLE-NEXT:   method #AnyProtocol.staticMethod!1: @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_
// TABLE-NEXT:   method #AnyProtocol.<~>!1: @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_ : $@cc(method) @thin (Arg, @in ConformsByExtension, @inout ConformsByExtension) -> ()
// SYMBOL:     sil @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_ : $@cc(method) @thin <A : ArchetypeReqt> (@in A, @in ConformsByExtension, @inout ConformsByExtension) -> ()
// SYMBOL:     sil @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_ : $@cc(method) @thin (@in SomeAssoc, @in ConformingAssoc, @inout ConformsByExtension) -> ()
// SYMBOL:     sil @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_ : $@thin (@in ConformsByExtension, ConformsByExtension.metatype) -> ()
// SYMBOL:     sil @_TTWV14witness_tables19ConformsByExtensionS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_ : $@thin (@in ConformsByExtension, @in ConformsByExtension, ConformsByExtension.metatype) -> ()

extension OtherModuleStruct : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: OtherModuleStruct) {}
  func generic<H: ArchetypeReqt>(x: H, y: OtherModuleStruct) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: OtherModuleStruct) {}
}
func <~>(x: OtherModuleStruct, y: OtherModuleStruct) {}
// TABLE:      sil_witness_table OtherModuleStruct: AnyProtocol module witness_tables {
// TABLE-NEXT:   associated_type AssocType: SomeAssoc
// TABLE-NEXT:   associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:   associated_type_protocol (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:   method #AnyProtocol.method!1: @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_6methodU_fRQPS2_FT1xVS1_3Arg1yS3__T_
// TABLE-NEXT:   method #AnyProtocol.generic!1: @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_7genericU_fRQPS2_US1_13ArchetypeReqt__FT1xQ_1yS3__T_
// TABLE-NEXT:   method #AnyProtocol.assocTypesMethod!1: @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_16assocTypesMethodU_fRQPS2_FT1xQS3_9AssocType1yQS3_13AssocWithReqt_T_
// TABLE-NEXT:   method #AnyProtocol.staticMethod!1: @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_12staticMethodU_fMQPS2_FT1xS3__T_
// TABLE-NEXT:   method #AnyProtocol.<~>!1: @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_oi3ltgU_fMQPS2_FT1xS3_1yS3__T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_6methodU_fRQPS2_FT1xVS1_3Arg1yS3__T_ : $@cc(method) @thin (Arg, @in OtherModuleStruct, @inout OtherModuleStruct) -> ()
// SYMBOL:     sil @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_7genericU_fRQPS2_US1_13ArchetypeReqt__FT1xQ_1yS3__T_ : $@cc(method) @thin <A : ArchetypeReqt> (@in A, @in OtherModuleStruct, @inout OtherModuleStruct) -> ()
// SYMBOL:     sil @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_16assocTypesMethodU_fRQPS2_FT1xQS3_9AssocType1yQS3_13AssocWithReqt_T_ : $@cc(method) @thin (@in SomeAssoc, @in ConformingAssoc, @inout OtherModuleStruct) -> ()
// SYMBOL:     sil @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_12staticMethodU_fMQPS2_FT1xS3__T_ : $@thin (@in OtherModuleStruct, OtherModuleStruct.metatype) -> ()
// SYMBOL:     sil @_TTWV16witness_tables_b17OtherModuleStruct14witness_tables11AnyProtocolS1_S2_oi3ltgU_fMQPS2_FT1xS3_1yS3__T_ : $@thin (@in OtherModuleStruct, @in OtherModuleStruct, OtherModuleStruct.metatype) -> ()

protocol OtherProtocol {}

struct ConformsWithMoreGenericWitnesses : AnyProtocol, OtherProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method<I, J>(x: I, y: J) {}
  func generic<K, L>(x: K, y: L) {}

  func assocTypesMethod<M, N>(x: M, y: N) {}

  static func staticMethod<O>(x: O) {}
}
func <~> <P: OtherProtocol>(x: P, y: P) {}
// TABLE:      sil_witness_table ConformsWithMoreGenericWitnesses: AnyProtocol module witness_tables {
// TABLE-NEXT:   associated_type AssocType: SomeAssoc
// TABLE-NEXT:   associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:   associated_type_protocol (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:   method #AnyProtocol.method!1: @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.generic!1: @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_
// TABLE-NEXT:   method #AnyProtocol.assocTypesMethod!1: @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_
// TABLE-NEXT:   method #AnyProtocol.staticMethod!1: @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_
// TABLE-NEXT:   method #AnyProtocol.<~>!1: @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_
// TABLE-NEXT: }
// SYMBOL:     sil @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_6methodU_fRQPS1_FT1xVS_3Arg1yS2__T_ : $@cc(method) @thin (Arg, @in ConformsWithMoreGenericWitnesses, @inout ConformsWithMoreGenericWitnesses) -> ()
// SYMBOL:     sil @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_7genericU_fRQPS1_US_13ArchetypeReqt__FT1xQ_1yS2__T_ : $@cc(method) @thin <A : ArchetypeReqt> (@in A, @in ConformsWithMoreGenericWitnesses, @inout ConformsWithMoreGenericWitnesses) -> ()
// SYMBOL:     sil @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_16assocTypesMethodU_fRQPS1_FT1xQS2_9AssocType1yQS2_13AssocWithReqt_T_ : $@cc(method) @thin (@in SomeAssoc, @in ConformingAssoc, @inout ConformsWithMoreGenericWitnesses) -> ()
// SYMBOL:     sil @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_12staticMethodU_fMQPS1_FT1xS2__T_ : $@thin (@in ConformsWithMoreGenericWitnesses, ConformsWithMoreGenericWitnesses.metatype) -> ()
// SYMBOL:     sil @_TTWV14witness_tables32ConformsWithMoreGenericWitnessesS_11AnyProtocolS_S1_oi3ltgU_fMQPS1_FT1xS2_1yS2__T_ : $@thin (@in ConformsWithMoreGenericWitnesses, @in ConformsWithMoreGenericWitnesses, ConformsWithMoreGenericWitnesses.metatype) -> ()
