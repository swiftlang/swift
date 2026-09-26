// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes
// REQUIRES: PTRSIZE=64

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend \
// RUN:   -internal-import-bridging-header %t/Hidden.h \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info \
// RUN:   -parse-as-library -emit-module -module-name Library \
// RUN:   -emit-module-path %t/Library.swiftmodule %t/Library.swift \
// RUN:   > %t/serialization.txt
// RUN: %FileCheck %s --input-file %t/serialization.txt \
// RUN:   --check-prefixes=CHECK-BOTH,CHECK-LIBRARY --match-full-lines \
// RUN:   --implicit-check-not='{{.}}'

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client %t/Client.swift \
// RUN:   > %t/recovery.txt
// RUN: %FileCheck %s --input-file %t/recovery.txt \
// RUN:   --check-prefixes=CHECK-BOTH,CHECK-CLIENT --match-full-lines \
// RUN:   --implicit-check-not='{{.}}'

//--- Hidden.h
typedef struct {
  int count;
  double value;
} HiddenCStruct;

//--- Library.swift
public struct Wrapper {
  private var hidden: HiddenCStruct

  public init(count: Int32, value: Double) {
    hidden = HiddenCStruct(count: count, value: value)
  }
}

//--- Client.swift
import Library

public func passThrough(_ value: Wrapper) -> Wrapper {
  value
}

// CHECK-BOTH: === Abstract type layout information ===
// CHECK-BOTH-NEXT: mangledName: $sSo13HiddenCStructa
// CHECK-LIBRARY-NEXT: origin: serialization
// CHECK-CLIENT-NEXT: origin: recovery
// CHECK-BOTH-NEXT: TypeLowering:
// CHECK-BOTH-NEXT:   class: TrivialTypeLowering
// CHECK-BOTH-NEXT:   Expansion: Maximal
// CHECK-BOTH-NEXT:   isTrivial: true.
// CHECK-BOTH-NEXT:   isEscapable: true.
// CHECK-BOTH-NEXT:   isNonTrivialOnlyBecauseNonEscapable: false.
// CHECK-BOTH-NEXT:   isReferenceCounted: false.
// CHECK-BOTH-NEXT:   isFixedABI: true.
// CHECK-BOTH-NEXT:   isAddressOnly: false.
// CHECK-BOTH-NEXT:   isResilient: false.
// CHECK-BOTH-NEXT:   isTypeExpansionSensitive: false.
// CHECK-BOTH-NEXT:   isInfinite: false.
// CHECK-BOTH-NEXT:   isOrContainsRawPointer: false.
// CHECK-BOTH-NEXT:   isLexical: false.
// CHECK-BOTH-NEXT:   isOrContainsPack: false.
// CHECK-BOTH-NEXT:   isAddressableForDependencies: true.
// CHECK-BOTH-NEXT:   isOrContainsRawLayout: false.
// CHECK-BOTH-NEXT:   hasOnlyDefaultDeinit: true.
// CHECK-BOTH-NEXT:   isVeryLargeType: false.
// CHECK-BOTH-NEXT:   definitelyIsAddressableForDependencies: true.
// CHECK-BOTH-NEXT:   definitelyIsOrContainsRawLayout: false.
// CHECK-BOTH-EMPTY:
// CHECK-BOTH-NEXT: SILType:
// CHECK-LIBRARY-NEXT:   type: $HiddenCStruct
// CHECK-CLIENT-NEXT:   type: $@_hidden("$sSo13HiddenCStructa")
// CHECK-BOTH-NEXT:   category: object
// CHECK-BOTH-NEXT:   isVoid: false
// CHECK-BOTH-NEXT:   isTuple: false
// CHECK-BOTH-NEXT:   isFunction: false
// CHECK-BOTH-NEXT:   isMetatype: false
// TODO: Preserve aggregate and nominal classification in AbstractTypeLayout.
// CHECK-LIBRARY-NEXT:   isAggregate: true
// CHECK-CLIENT-NEXT:   isAggregate: false
// CHECK-BOTH-NEXT:   isOrHasEnum: false
// CHECK-LIBRARY-NEXT:   nominalKind: struct
// CHECK-CLIENT-NEXT:   nominalKind: none
// CHECK-BOTH-NEXT:   optionalObjectType: none
// CHECK-BOTH-NEXT:   isForeignReferenceType: false
// CHECK-BOTH-NEXT:   isSensitive: false
// TODO: Preserve Copyable conformance information in AbstractTypeLayout.
// CHECK-LIBRARY-NEXT:   isMoveOnly: false
// CHECK-CLIENT-NEXT:   isMoveOnly: true
// CHECK-BOTH-NEXT:   isValueTypeWithDeinit: false
// CHECK-BOTH-NEXT:   hasTypeParameter: false
// CHECK-BOTH-NEXT:   hasArchetype: false
// CHECK-BOTH-NEXT:   hasPrimaryArchetype: false
// CHECK-BOTH-NEXT:   hasLocalArchetype: false
// CHECK-BOTH-NEXT:   hasOpaqueArchetype: false
// CHECK-BOTH-NEXT:   hasOpenedExistential: false
// CHECK-BOTH-NEXT:   hasElementArchetype: false
// CHECK-BOTH-NEXT:   hasParameterPack: false
// CHECK-BOTH-NEXT:   hasPack: false
// CHECK-BOTH-NEXT:   hasPackArchetype: false
// CHECK-BOTH-NEXT:   hasAnyPack: false
// CHECK-BOTH-NEXT:   hasParameterizedExistential: false
// CHECK-BOTH-NEXT:   hasDynamicSelf: false
// CHECK-BOTH-NEXT:   hasUnboundGeneric: false
// CHECK-BOTH-NEXT:   hasError: false
// CHECK-BOTH-NEXT:   hasBareError: false
// CHECK-BOTH-NEXT:   hasReferenceSemantics: false
// CHECK-BOTH-NEXT:   isAnyClassReferenceType: false
// CHECK-BOTH-NEXT:   hasRetainablePointerRepresentation: false
// CHECK-BOTH-NEXT:   isConstraintType: false
// CHECK-BOTH-NEXT:   isExistentialType: false
// CHECK-BOTH-NEXT:   isAnyExistentialType: false
// CHECK-BOTH-NEXT:   isClassExistentialType: false
// CHECK-BOTH-NEXT:   existentialRepresentation: none
// CHECK-BOTH-NEXT:   isBridgeableObjectType: false
// CHECK-BOTH-NEXT:   isClassOrClassMetatype: false
// CHECK-BOTH-NEXT:   isAddressableForDependencies: true
// CHECK-BOTH-NEXT:   hasRawLayout: false
// CHECK-BOTH-NEXT:   rawLayoutLikeType: none
// CHECK-BOTH-NEXT:   rawLayoutCountType: none
// CHECK-BOTH-NEXT: TypeInfo:
// CHECK-BOTH-NEXT: class: LoadableClangRecordTypeInfo
// CHECK-LIBRARY-NEXT: storageType: %TSo13HiddenCStructa = type <{ %Ts5Int32V, {{\[}}4 x i8{{\]}}, %TSd }>
// CHECK-CLIENT-NEXT: storageType: <{ <{ i32 }>, {{\[}}4 x i8{{\]}}, <{ double }> }>
// CHECK-BOTH-NEXT: kind: loadable
// CHECK-BOTH-NEXT: bestKnownAlignment: 8
// CHECK-BOTH-NEXT: abiAccessible: true
// CHECK-BOTH-NEXT: triviallyDestroyable: true
// CHECK-BOTH-NEXT: copyable: true
// CHECK-BOTH-NEXT: bitwiseTakable: true
// CHECK-BOTH-NEXT: bitwiseBorrowable: true
// CHECK-BOTH-NEXT: fixedSizeMinimal: true
// CHECK-BOTH-NEXT: fixedSizeMaximal: true
// CHECK-BOTH-NEXT: loadable: true
// CHECK-BOTH-NEXT: singleRetainablePointer: false
// CHECK-BOTH-NEXT: fixedSize: 16
// CHECK-BOTH-NEXT: fixedAlignment: 8
// CHECK-BOTH-NEXT: fixedStride: 16
// CHECK-BOTH-NEXT: spareBits: 18446744069414584320
// CHECK-BOTH-NEXT: extraInhabitantCount: 0
// CHECK-BOTH-NEXT: extraInhabitantMask: 0
// CHECK-BOTH-NEXT: explosionSchema:
// CHECK-BOTH-NEXT: - index: 0
// CHECK-BOTH-NEXT: kind: scalar
// CHECK-BOTH-NEXT: type: i32
// CHECK-BOTH-NEXT: - index: 1
// CHECK-BOTH-NEXT: kind: scalar
// CHECK-BOTH-NEXT: type: double
// CHECK-BOTH-NEXT: nativeParameterSchema:
// CHECK-BOTH-NEXT: requiresIndirect: false
// CHECK-BOTH-NEXT: components:
// CHECK-BOTH-NEXT: - index: 0
// CHECK-BOTH-NEXT: begin: 0
// CHECK-BOTH-NEXT: end: 4
// CHECK-BOTH-NEXT: type: i32
// CHECK-BOTH-NEXT: - index: 1
// CHECK-BOTH-NEXT: begin: 8
// CHECK-BOTH-NEXT: end: 16
// CHECK-BOTH-NEXT: type: double
// CHECK-BOTH-NEXT: nativeReturnSchema:
// CHECK-BOTH-NEXT: requiresIndirect: false
// CHECK-BOTH-NEXT: components:
// CHECK-BOTH-NEXT: - index: 0
// CHECK-BOTH-NEXT: begin: 0
// CHECK-BOTH-NEXT: end: 4
// CHECK-BOTH-NEXT: type: i32
// CHECK-BOTH-NEXT: - index: 1
// CHECK-BOTH-NEXT: begin: 8
// CHECK-BOTH-NEXT: end: 16
// CHECK-BOTH-NEXT: type: double
// CHECK-BOTH-NEXT:   fieldTypeInfos:
// CHECK-BOTH-NEXT:     - index: 0
// CHECK-BOTH-NEXT:       TypeInfo:
// CHECK-BOTH-NEXT:         class: LoadableStructTypeInfo
// CHECK-BOTH-NEXT:         storageType: %Ts5Int32V = type <{ i32 }>
// CHECK-BOTH-NEXT:         kind: loadable
// CHECK-BOTH-NEXT:         bestKnownAlignment: 4
// CHECK-BOTH-NEXT:         abiAccessible: true
// CHECK-BOTH-NEXT:         triviallyDestroyable: true
// CHECK-BOTH-NEXT:         copyable: true
// CHECK-BOTH-NEXT:         bitwiseTakable: true
// CHECK-BOTH-NEXT:         bitwiseBorrowable: true
// CHECK-BOTH-NEXT:         fixedSizeMinimal: true
// CHECK-BOTH-NEXT:         fixedSizeMaximal: true
// CHECK-BOTH-NEXT:         loadable: true
// CHECK-BOTH-NEXT:         singleRetainablePointer: false
// CHECK-BOTH-NEXT:         fixedSize: 4
// CHECK-BOTH-NEXT:         fixedAlignment: 4
// CHECK-BOTH-NEXT:         fixedStride: 4
// CHECK-BOTH-NEXT:         spareBits: 0
// CHECK-BOTH-NEXT:         extraInhabitantCount: 0
// CHECK-BOTH-NEXT:         extraInhabitantMask: 0
// CHECK-BOTH-NEXT:         explosionSchema:
// CHECK-BOTH-NEXT:           - index: 0
// CHECK-BOTH-NEXT:             kind: scalar
// CHECK-BOTH-NEXT:             type: i32
// CHECK-BOTH-NEXT:         nativeParameterSchema:
// CHECK-BOTH-NEXT:           requiresIndirect: false
// CHECK-BOTH-NEXT:           components:
// CHECK-BOTH-NEXT:             - index: 0
// CHECK-BOTH-NEXT:               begin: 0
// CHECK-BOTH-NEXT:               end: 4
// CHECK-BOTH-NEXT:               type: i32
// CHECK-BOTH-NEXT:         nativeReturnSchema:
// CHECK-BOTH-NEXT:           requiresIndirect: false
// CHECK-BOTH-NEXT:           components:
// CHECK-BOTH-NEXT:             - index: 0
// CHECK-BOTH-NEXT:               begin: 0
// CHECK-BOTH-NEXT:               end: 4
// CHECK-BOTH-NEXT:               type: i32
// CHECK-BOTH-NEXT:         fieldTypeInfos:
// CHECK-BOTH-NEXT:           - index: 0
// CHECK-BOTH-NEXT:             TypeInfo:
// CHECK-BOTH-NEXT:               class: PrimitiveTypeInfo
// CHECK-BOTH-NEXT:               storageType: i32
// CHECK-BOTH-NEXT:               kind: loadable
// CHECK-BOTH-NEXT:               bestKnownAlignment: 4
// CHECK-BOTH-NEXT:               abiAccessible: true
// CHECK-BOTH-NEXT:               triviallyDestroyable: true
// CHECK-BOTH-NEXT:               copyable: true
// CHECK-BOTH-NEXT:               bitwiseTakable: true
// CHECK-BOTH-NEXT:               bitwiseBorrowable: true
// CHECK-BOTH-NEXT:               fixedSizeMinimal: true
// CHECK-BOTH-NEXT:               fixedSizeMaximal: true
// CHECK-BOTH-NEXT:               loadable: true
// CHECK-BOTH-NEXT:               singleRetainablePointer: false
// CHECK-BOTH-NEXT:               fixedSize: 4
// CHECK-BOTH-NEXT:               fixedAlignment: 4
// CHECK-BOTH-NEXT:               fixedStride: 4
// CHECK-BOTH-NEXT:               spareBits: 0
// CHECK-BOTH-NEXT:               extraInhabitantCount: 0
// CHECK-BOTH-NEXT:               extraInhabitantMask: 4294967295
// CHECK-BOTH-NEXT:               explosionSchema:
// CHECK-BOTH-NEXT:                 - index: 0
// CHECK-BOTH-NEXT:                   kind: scalar
// CHECK-BOTH-NEXT:                   type: i32
// CHECK-BOTH-NEXT:               nativeParameterSchema:
// CHECK-BOTH-NEXT:                 requiresIndirect: false
// CHECK-BOTH-NEXT:                 components:
// CHECK-BOTH-NEXT:                   - index: 0
// CHECK-BOTH-NEXT:                     begin: 0
// CHECK-BOTH-NEXT:                     end: 4
// CHECK-BOTH-NEXT:                     type: i32
// CHECK-BOTH-NEXT:               nativeReturnSchema:
// CHECK-BOTH-NEXT:                 requiresIndirect: false
// CHECK-BOTH-NEXT:                 components:
// CHECK-BOTH-NEXT:                   - index: 0
// CHECK-BOTH-NEXT:                     begin: 0
// CHECK-BOTH-NEXT:                     end: 4
// CHECK-BOTH-NEXT:                     type: i32
// CHECK-BOTH-NEXT:     - index: 1
// CHECK-BOTH-NEXT:       TypeInfo:
// CHECK-BOTH-NEXT:         class: LoadableStructTypeInfo
// CHECK-BOTH-NEXT:         storageType: %TSd = type <{ double }>
// CHECK-BOTH-NEXT:         kind: loadable
// CHECK-BOTH-NEXT:         bestKnownAlignment: 8
// CHECK-BOTH-NEXT:         abiAccessible: true
// CHECK-BOTH-NEXT:         triviallyDestroyable: true
// CHECK-BOTH-NEXT:         copyable: true
// CHECK-BOTH-NEXT:         bitwiseTakable: true
// CHECK-BOTH-NEXT:         bitwiseBorrowable: true
// CHECK-BOTH-NEXT:         fixedSizeMinimal: true
// CHECK-BOTH-NEXT:         fixedSizeMaximal: true
// CHECK-BOTH-NEXT:         loadable: true
// CHECK-BOTH-NEXT:         singleRetainablePointer: false
// CHECK-BOTH-NEXT:         fixedSize: 8
// CHECK-BOTH-NEXT:         fixedAlignment: 8
// CHECK-BOTH-NEXT:         fixedStride: 8
// CHECK-BOTH-NEXT:         spareBits: 0
// CHECK-BOTH-NEXT:         extraInhabitantCount: 0
// CHECK-BOTH-NEXT:         extraInhabitantMask: 0
// CHECK-BOTH-NEXT:         explosionSchema:
// CHECK-BOTH-NEXT:           - index: 0
// CHECK-BOTH-NEXT:             kind: scalar
// CHECK-BOTH-NEXT:             type: double
// CHECK-BOTH-NEXT:         nativeParameterSchema:
// CHECK-BOTH-NEXT:           requiresIndirect: false
// CHECK-BOTH-NEXT:           components:
// CHECK-BOTH-NEXT:             - index: 0
// CHECK-BOTH-NEXT:               begin: 0
// CHECK-BOTH-NEXT:               end: 8
// CHECK-BOTH-NEXT:               type: double
// CHECK-BOTH-NEXT:         nativeReturnSchema:
// CHECK-BOTH-NEXT:           requiresIndirect: false
// CHECK-BOTH-NEXT:           components:
// CHECK-BOTH-NEXT:             - index: 0
// CHECK-BOTH-NEXT:               begin: 0
// CHECK-BOTH-NEXT:               end: 8
// CHECK-BOTH-NEXT:               type: double
// CHECK-BOTH-NEXT:         fieldTypeInfos:
// CHECK-BOTH-NEXT:           - index: 0
// CHECK-BOTH-NEXT:             TypeInfo:
// CHECK-BOTH-NEXT:               class: PrimitiveTypeInfo
// CHECK-BOTH-NEXT:               storageType: double
// CHECK-BOTH-NEXT:               kind: loadable
// CHECK-BOTH-NEXT:               bestKnownAlignment: 8
// CHECK-BOTH-NEXT:               abiAccessible: true
// CHECK-BOTH-NEXT:               triviallyDestroyable: true
// CHECK-BOTH-NEXT:               copyable: true
// CHECK-BOTH-NEXT:               bitwiseTakable: true
// CHECK-BOTH-NEXT:               bitwiseBorrowable: true
// CHECK-BOTH-NEXT:               fixedSizeMinimal: true
// CHECK-BOTH-NEXT:               fixedSizeMaximal: true
// CHECK-BOTH-NEXT:               loadable: true
// CHECK-BOTH-NEXT:               singleRetainablePointer: false
// CHECK-BOTH-NEXT:               fixedSize: 8
// CHECK-BOTH-NEXT:               fixedAlignment: 8
// CHECK-BOTH-NEXT:               fixedStride: 8
// CHECK-BOTH-NEXT:               spareBits: 0
// CHECK-BOTH-NEXT:               extraInhabitantCount: 0
// CHECK-BOTH-NEXT:               extraInhabitantMask: 18446744073709551615
// CHECK-BOTH-NEXT:               explosionSchema:
// CHECK-BOTH-NEXT:                 - index: 0
// CHECK-BOTH-NEXT:                   kind: scalar
// CHECK-BOTH-NEXT:                   type: double
// CHECK-BOTH-NEXT:               nativeParameterSchema:
// CHECK-BOTH-NEXT:                 requiresIndirect: false
// CHECK-BOTH-NEXT:                 components:
// CHECK-BOTH-NEXT:                   - index: 0
// CHECK-BOTH-NEXT:                     begin: 0
// CHECK-BOTH-NEXT:                     end: 8
// CHECK-BOTH-NEXT:                     type: double
// CHECK-BOTH-NEXT:               nativeReturnSchema:
// CHECK-BOTH-NEXT:                 requiresIndirect: false
// CHECK-BOTH-NEXT:                 components:
// CHECK-BOTH-NEXT:                   - index: 0
// CHECK-BOTH-NEXT:                     begin: 0
// CHECK-BOTH-NEXT:                     end: 8
// CHECK-BOTH-NEXT:                     type: double
