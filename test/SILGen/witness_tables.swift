
// RUN: %target-swift-emit-silgen -module-name witness_tables  -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module -enable-objc-interop > %t.sil
// RUN: %FileCheck -check-prefix=TABLE -check-prefix=TABLE-ALL %s < %t.sil
// RUN: %FileCheck -check-prefix=SYMBOL %s < %t.sil

// RUN: %target-swift-emit-silgen -module-name witness_tables  -I %S/Inputs -enable-source-import %s -disable-objc-attr-requires-foundation-module -enable-objc-interop -enable-testing > %t.testable.sil
// RUN: %FileCheck -check-prefix=TABLE-TESTABLE -check-prefix=TABLE-ALL %s < %t.testable.sil
// RUN: %FileCheck -check-prefix=SYMBOL-TESTABLE %s < %t.testable.sil

import witness_tables_b

struct Arg {}

@objc class ObjCClass {}

infix operator <~>

protocol AssocReqt {
  func requiredMethod()
}

protocol ArchetypeReqt {
  func requiredMethod()
}

protocol AnyProtocol {
  associatedtype AssocType
  associatedtype AssocWithReqt: AssocReqt

  func method(x: Arg, y: Self)
  func generic<A: ArchetypeReqt>(x: A, y: Self)

  func assocTypesMethod(x: AssocType, y: AssocWithReqt)

  static func staticMethod(x: Self)

  static func <~>(x: Self, y: Self)
}

protocol ClassProtocol : class {
  associatedtype AssocType
  associatedtype AssocWithReqt: AssocReqt

  func method(x: Arg, y: Self)
  func generic<B: ArchetypeReqt>(x: B, y: Self)

  func assocTypesMethod(x: AssocType, y: AssocWithReqt)

  static func staticMethod(x: Self)

  static func <~>(x: Self, y: Self)
}

@objc protocol ObjCProtocol {
  func method(x: ObjCClass)
  static func staticMethod(y: ObjCClass)
}

class SomeAssoc {}

struct ConformingAssoc : AssocReqt {
  func requiredMethod() {}
}
// TABLE-LABEL: sil_witness_table hidden ConformingAssoc: AssocReqt module witness_tables {
// TABLE-TESTABLE-LABEL: sil_witness_table [serialized] ConformingAssoc: AssocReqt module witness_tables {
// TABLE-ALL-NEXT:    method #AssocReqt.requiredMethod: {{.*}} : @$s14witness_tables15ConformingAssocVAA0D4ReqtA2aDP14requiredMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-ALL-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables15ConformingAssocVAA0D4ReqtA2aDP14requiredMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL-TESTABLE:      sil shared [transparent] [serialized] [thunk] [ossa] @$s14witness_tables15ConformingAssocVAA0D4ReqtA2aDP14requiredMethod{{[_0-9a-zA-Z]*}}FTW :

struct ConformingStruct : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingStruct) {}
  func generic<D: ArchetypeReqt>(x: D, y: ConformingStruct) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: ConformingStruct) {}
}
func <~>(x: ConformingStruct, y: ConformingStruct) {}
// TABLE-LABEL: sil_witness_table hidden ConformingStruct: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW {{.*}}: ArchetypeReqt> (@in_guaranteed τ_0_0, @in_guaranteed ConformingStruct, @in_guaranteed ConformingStruct) -> ()
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL-TESTABLE:      sil shared [transparent] [serialized] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL-TESTABLE:      sil shared [transparent] [serialized] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL-TESTABLE:      sil shared [transparent] [serialized] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL-TESTABLE:      sil shared [transparent] [serialized] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL-TESTABLE:      sil shared [transparent] [serialized] [thunk] [ossa] @$s14witness_tables16ConformingStructVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

protocol AddressOnly {}

struct ConformingAddressOnlyStruct : AnyProtocol {
  var p: AddressOnly // force address-only layout with a protocol-type field

  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingAddressOnlyStruct) {}
  func generic<E: ArchetypeReqt>(x: E, y: ConformingAddressOnlyStruct) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: ConformingAddressOnlyStruct) {}
}
func <~>(x: ConformingAddressOnlyStruct, y: ConformingAddressOnlyStruct) {}
// TABLE-LABEL: sil_witness_table hidden ConformingAddressOnlyStruct: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables27ConformingAddressOnlyStructVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

class ConformingClass : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingClass) {}
  func generic<F: ArchetypeReqt>(x: F, y: ConformingClass) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  class func staticMethod(x: ConformingClass) {}
}
func <~>(x: ConformingClass, y: ConformingClass) {}
// TABLE-LABEL: sil_witness_table hidden ConformingClass: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables15ConformingClassCAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

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
// TABLE-LABEL: sil_witness_table hidden ConformsByExtension: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables19ConformsByExtensionVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

extension OtherModuleStruct : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: OtherModuleStruct) {}
  func generic<H: ArchetypeReqt>(x: H, y: OtherModuleStruct) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: OtherModuleStruct) {}
}
func <~>(x: OtherModuleStruct, y: OtherModuleStruct) {}
// TABLE-LABEL: sil_witness_table hidden OtherModuleStruct: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s16witness_tables_b17OtherModuleStructV0a1_B011AnyProtocolA2dEP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

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
// TABLE-LABEL: sil_witness_table hidden ConformsWithMoreGenericWitnesses: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables32ConformsWithMoreGenericWitnessesVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

class ConformingClassToClassProtocol : ClassProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc
  
  func method(x: Arg, y: ConformingClassToClassProtocol) {}
  func generic<Q: ArchetypeReqt>(x: Q, y: ConformingClassToClassProtocol) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  class func staticMethod(x: ConformingClassToClassProtocol) {}
}
func <~>(x: ConformingClassToClassProtocol,
         y: ConformingClassToClassProtocol) {}
// TABLE-LABEL: sil_witness_table hidden ConformingClassToClassProtocol: ClassProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #ClassProtocol.method: {{.*}} : @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #ClassProtocol.generic: {{.*}} : @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #ClassProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #ClassProtocol.staticMethod: {{.*}} : @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #ClassProtocol."<~>": {{.*}} : @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// SYMBOL:  sil private [transparent] [thunk] [ossa] @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP6method{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:  sil private [transparent] [thunk] [ossa] @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP7generic{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:  sil private [transparent] [thunk] [ossa] @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:  sil private [transparent] [thunk] [ossa] @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW :
// SYMBOL:  sil private [transparent] [thunk] [ossa] @$s14witness_tables017ConformingClassToD8ProtocolCAA0dF0A2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW :

class ConformingClassToObjCProtocol : ObjCProtocol {
  @objc func method(x: ObjCClass) {}
  @objc class func staticMethod(y: ObjCClass) {}
}
// TABLE-NOT:  sil_witness_table hidden ConformingClassToObjCProtocol

struct ConformingGeneric<R: AssocReqt> : AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = R

  func method(x: Arg, y: ConformingGeneric) {}
  func generic<Q: ArchetypeReqt>(x: Q, y: ConformingGeneric) {}

  func assocTypesMethod(x: SomeAssoc, y: R) {}

  static func staticMethod(x: ConformingGeneric) {}
}
func <~> <R: AssocReqt>(x: ConformingGeneric<R>, y: ConformingGeneric<R>) {}
// TABLE-LABEL: sil_witness_table hidden <R where R : AssocReqt> ConformingGeneric<R>: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): dependent
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: R
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables17ConformingGenericVyxGAA11AnyProtocolA2aEP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables17ConformingGenericVyxGAA11AnyProtocolA2aEP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables17ConformingGenericVyxGAA11AnyProtocolA2aEP16assocTypesMetho{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables17ConformingGenericVyxGAA11AnyProtocolA2aEP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables17ConformingGenericVyxGAA11AnyProtocolA2aEP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }

protocol AnotherProtocol {}

struct ConformingGenericWithMoreGenericWitnesses<S: AssocReqt>
  : AnyProtocol, AnotherProtocol
{
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = S

  func method<T, U>(x: T, y: U) {}
  func generic<V, W>(x: V, y: W) {}

  func assocTypesMethod<X, Y>(x: X, y: Y) {}

  static func staticMethod<Z>(x: Z) {}
}
func <~> <AA: AnotherProtocol, BB: AnotherProtocol>(x: AA, y: BB) {}
// TABLE-LABEL: sil_witness_table hidden <S where S : AssocReqt> ConformingGenericWithMoreGenericWitnesses<S>: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): dependent
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: S
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables025ConformingGenericWithMoreD9WitnessesVyxGAA11AnyProtocolA2aEP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables025ConformingGenericWithMoreD9WitnessesVyxGAA11AnyProtocolA2aEP7{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables025ConformingGenericWithMoreD9WitnessesVyxGAA11AnyProtocolA2aEP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables025ConformingGenericWithMoreD9WitnessesVyxGAA11AnyProtocolA2aEP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables025ConformingGenericWithMoreD9WitnessesVyxGAA11AnyProtocolA2aEP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }

protocol InheritedProtocol1 : AnyProtocol {
  func inheritedMethod()
}

protocol InheritedProtocol2 : AnyProtocol {
  func inheritedMethod()
}

protocol InheritedClassProtocol : class, AnyProtocol {
  func inheritedMethod()
}

struct InheritedConformance : InheritedProtocol1 {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc

  func method(x: Arg, y: InheritedConformance) {}
  func generic<H: ArchetypeReqt>(x: H, y: InheritedConformance) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: InheritedConformance) {}

  func inheritedMethod() {}
}
func <~>(x: InheritedConformance, y: InheritedConformance) {}
// TABLE-LABEL: sil_witness_table hidden InheritedConformance: InheritedProtocol1 module witness_tables {
// TABLE-NEXT:    base_protocol AnyProtocol: InheritedConformance: AnyProtocol module witness_tables
// TABLE-NEXT:    method #InheritedProtocol1.inheritedMethod: {{.*}} : @$s14witness_tables20InheritedConformanceVAA0C9Protocol1A2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:  }
// TABLE-LABEL: sil_witness_table hidden InheritedConformance: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables20InheritedConformanceVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables20InheritedConformanceVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables20InheritedConformanceVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables20InheritedConformanceVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables20InheritedConformanceVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }

struct RedundantInheritedConformance : InheritedProtocol1, AnyProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc

  func method(x: Arg, y: RedundantInheritedConformance) {}
  func generic<H: ArchetypeReqt>(x: H, y: RedundantInheritedConformance) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: RedundantInheritedConformance) {}

  func inheritedMethod() {}
}
func <~>(x: RedundantInheritedConformance, y: RedundantInheritedConformance) {}
// TABLE-LABEL: sil_witness_table hidden RedundantInheritedConformance: InheritedProtocol1 module witness_tables {
// TABLE-NEXT:    base_protocol AnyProtocol: RedundantInheritedConformance: AnyProtocol module witness_tables
// TABLE-NEXT:    method #InheritedProtocol1.inheritedMethod: {{.*}} : @$s14witness_tables29RedundantInheritedConformanceVAA0D9Protocol1A2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:  }
// TABLE-LABEL: sil_witness_table hidden RedundantInheritedConformance: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables29RedundantInheritedConformanceVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables29RedundantInheritedConformanceVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables29RedundantInheritedConformanceVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables29RedundantInheritedConformanceVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables29RedundantInheritedConformanceVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }

struct DiamondInheritedConformance : InheritedProtocol1, InheritedProtocol2 {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc

  func method(x: Arg, y: DiamondInheritedConformance) {}
  func generic<H: ArchetypeReqt>(x: H, y: DiamondInheritedConformance) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  static func staticMethod(x: DiamondInheritedConformance) {}

  func inheritedMethod() {}
}
func <~>(x: DiamondInheritedConformance, y: DiamondInheritedConformance) {}
// TABLE-LABEL: sil_witness_table hidden DiamondInheritedConformance: InheritedProtocol1 module witness_tables {
// TABLE-NEXT:    base_protocol AnyProtocol: DiamondInheritedConformance: AnyProtocol module witness_tables
// TABLE-NEXT:    method #InheritedProtocol1.inheritedMethod: {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA0D9Protocol1A2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:  }
// TABLE-LABEL: sil_witness_table hidden DiamondInheritedConformance: InheritedProtocol2 module witness_tables {
// TABLE-NEXT:    base_protocol AnyProtocol: DiamondInheritedConformance: AnyProtocol module witness_tables
// TABLE-NEXT:    method #InheritedProtocol2.inheritedMethod: {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA0D9Protocol2A2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:  }
// TABLE-LABEL: sil_witness_table hidden DiamondInheritedConformance: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables27DiamondInheritedConformanceVAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }

class ClassInheritedConformance : InheritedClassProtocol {
  typealias AssocType = SomeAssoc
  typealias AssocWithReqt = ConformingAssoc

  func method(x: Arg, y: ClassInheritedConformance) {}
  func generic<H: ArchetypeReqt>(x: H, y: ClassInheritedConformance) {}

  func assocTypesMethod(x: SomeAssoc, y: ConformingAssoc) {}

  class func staticMethod(x: ClassInheritedConformance) {}

  func inheritedMethod() {}
}
func <~>(x: ClassInheritedConformance, y: ClassInheritedConformance) {}
// TABLE-LABEL: sil_witness_table hidden ClassInheritedConformance: InheritedClassProtocol module witness_tables {
// TABLE-NEXT:    base_protocol AnyProtocol: ClassInheritedConformance: AnyProtocol module witness_tables
// TABLE-NEXT:    method #InheritedClassProtocol.inheritedMethod: {{.*}} : @$s14witness_tables25ClassInheritedConformanceCAA0dC8ProtocolA2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:  }
// TABLE-LABEL: sil_witness_table hidden ClassInheritedConformance: AnyProtocol module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocWithReqt: AssocReqt): ConformingAssoc: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: SomeAssoc
// TABLE-NEXT:    associated_type AssocWithReqt: ConformingAssoc
// TABLE-NEXT:    method #AnyProtocol.method: {{.*}} : @$s14witness_tables25ClassInheritedConformanceCAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.generic: {{.*}} : @$s14witness_tables25ClassInheritedConformanceCAA11AnyProtocolA2aDP7generic{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.assocTypesMethod: {{.*}} : @$s14witness_tables25ClassInheritedConformanceCAA11AnyProtocolA2aDP16assocTypesMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:    method #AnyProtocol.staticMethod: {{.*}} : @$s14witness_tables25ClassInheritedConformanceCAA11AnyProtocolA2aDP12staticMethod{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:    method #AnyProtocol."<~>": {{.*}} : @$s14witness_tables25ClassInheritedConformanceCAA11AnyProtocolA2aDP3ltgoi{{[_0-9a-zA-Z]*}}FZTW
// TABLE-NEXT:  }
// -- Witnesses have the 'self' abstraction level of their protocol.
//    AnyProtocol has no class bound, so its witnesses treat Self as opaque.
//    InheritedClassProtocol has a class bound, so its witnesses treat Self as
//    a reference value.
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables25ClassInheritedConformanceCAA0dC8ProtocolA2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW :
// SYMBOL:      sil private [transparent] [thunk] [ossa] @$s14witness_tables25ClassInheritedConformanceCAA11AnyProtocolA2aDP6method{{[_0-9a-zA-Z]*}}FTW :

struct GenericAssocType<T> : AssocReqt {
  func requiredMethod() {}
}

protocol AssocTypeWithReqt {
  associatedtype AssocType : AssocReqt
}

struct ConformsWithDependentAssocType1<CC: AssocReqt> : AssocTypeWithReqt {
  typealias AssocType = CC
}
// TABLE-LABEL: sil_witness_table hidden <CC where CC : AssocReqt> ConformsWithDependentAssocType1<CC>: AssocTypeWithReqt module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocType: AssocReqt): dependent
// TABLE-NEXT:    associated_type AssocType: CC
// TABLE-NEXT:  }

struct ConformsWithDependentAssocType2<DD> : AssocTypeWithReqt {
  typealias AssocType = GenericAssocType<DD>
}
// TABLE-LABEL: sil_witness_table hidden <DD> ConformsWithDependentAssocType2<DD>: AssocTypeWithReqt module witness_tables {
// TABLE-NEXT:    associated_conformance (AssocType: AssocReqt): <T> GenericAssocType<T>: AssocReqt module witness_tables
// TABLE-NEXT:    associated_type AssocType: GenericAssocType<DD>
// TABLE-NEXT:  }

protocol InheritedFromObjC : ObjCProtocol {
  func inheritedMethod()
}

class ConformsInheritedFromObjC : InheritedFromObjC {
  @objc func method(x: ObjCClass) {}
  @objc class func staticMethod(y: ObjCClass) {}
  func inheritedMethod() {}
}
// TABLE-LABEL: sil_witness_table hidden ConformsInheritedFromObjC: InheritedFromObjC module witness_tables {
// TABLE-NEXT:    method #InheritedFromObjC.inheritedMethod: {{.*}} : @$s14witness_tables25ConformsInheritedFromObjCCAA0deF1CA2aDP15inheritedMethod{{[_0-9a-zA-Z]*}}FTW
// TABLE-NEXT:  }

protocol ObjCAssoc {
  associatedtype AssocType : ObjCProtocol
}

struct HasObjCAssoc : ObjCAssoc {
  typealias AssocType = ConformsInheritedFromObjC
}
// TABLE-LABEL: sil_witness_table hidden HasObjCAssoc: ObjCAssoc module witness_tables {
// TABLE-NEXT:    associated_type AssocType: ConformsInheritedFromObjC
// TABLE-NEXT:  }

protocol Initializer {
  init(arg: Arg)
}

// TABLE-LABEL: sil_witness_table hidden HasInitializerStruct: Initializer module witness_tables {
// TABLE-NEXT:  method #Initializer.init!allocator: {{.*}} : @$s14witness_tables20HasInitializerStructVAA0D0A2aDP{{[_0-9a-zA-Z]*}}fCTW
// TABLE-NEXT: }
// SYMBOL: sil private [transparent] [thunk] [ossa] @$s14witness_tables20HasInitializerStructVAA0D0A2aDP{{[_0-9a-zA-Z]*}}fCTW :
struct HasInitializerStruct : Initializer { 
  init(arg: Arg) { }
}

// TABLE-LABEL: sil_witness_table hidden HasInitializerClass: Initializer module witness_tables {
// TABLE-NEXT:  method #Initializer.init!allocator: {{.*}} : @$s14witness_tables19HasInitializerClassCAA0D0A2aDP{{[_0-9a-zA-Z]*}}fCTW
// TABLE-NEXT: }
// SYMBOL: sil private [transparent] [thunk] [ossa] @$s14witness_tables19HasInitializerClassCAA0D0A2aDP{{[_0-9a-zA-Z]*}}fCTW :
class HasInitializerClass : Initializer {
  required init(arg: Arg) { }
}

// TABLE-LABEL: sil_witness_table hidden HasInitializerEnum: Initializer module witness_tables {
// TABLE-NEXT:  method #Initializer.init!allocator: {{.*}} : @$s14witness_tables18HasInitializerEnumOAA0D0A2aDP{{[_0-9a-zA-Z]*}}fCTW
// TABLE-NEXT: }
// SYMBOL: sil private [transparent] [thunk] [ossa] @$s14witness_tables18HasInitializerEnumOAA0D0A2aDP{{[_0-9a-zA-Z]*}}fCTW :
enum HasInitializerEnum : Initializer {
  case A

  init(arg: Arg) { self = .A }
}
  
