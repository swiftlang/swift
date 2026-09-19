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
// RUN: %diff %t/serialization.expected %t/serialization.txt

// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -dump-abstract-type-layout-info \
// RUN:   -I %t -emit-ir -o %t/Client.ll -module-name Client %t/Client.swift \
// RUN:   > %t/recovery.txt
// RUN: %diff %t/recovery.expected %t/recovery.txt

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

//--- serialization.expected
=== Abstract type layout information ===
mangledName: $sSo13HiddenCStructa
origin: serialization
CanType:
  type: HiddenCStruct
  hasTypeParameter: false
  hasArchetype: false
  hasPrimaryArchetype: false
  hasLocalArchetype: false
  hasOpaqueArchetype: false
  hasOpenedExistential: false
  hasElementArchetype: false
  hasParameterPack: false
  hasPack: false
  hasPackArchetype: false
  hasParameterizedExistential: false
  hasDynamicSelf: false
  hasUnboundGeneric: false
  hasError: false
  hasBareError: false
  hasReferenceSemantics: false
  isAnyClassReferenceType: false
  isConstraintType: false
  isExistentialType: false
Minimal Type Lowering for lowered type: $HiddenCStruct.
Expansion: Maximal
isTrivial: true.
isReferenceCounted: false.
isFixedABI: true.
isAddressOnly: false.
isResilient: false.
isTypeExpansionSensitive: false.
isInfinite: false.
isOrContainsRawPointer: false.
isLexical: false.
isOrContainsPack: false.
isAddressableForDependencies: true.
hasOnlyDefaultDeinit: true.
definitelyIsAddressableForDependencies: true.
definitelyIsOrContainsRawLayout: false.

IRGen Type Lowering for lowered type: $HiddenCStruct.
Expansion: Maximal
isTrivial: true.
isReferenceCounted: false.
isFixedABI: true.
isAddressOnly: false.
isResilient: false.
isTypeExpansionSensitive: false.
isInfinite: false.
isOrContainsRawPointer: false.
isLexical: false.
isOrContainsPack: false.
isAddressableForDependencies: true.
hasOnlyDefaultDeinit: true.
definitelyIsAddressableForDependencies: true.
definitelyIsOrContainsRawLayout: false.

TypeInfo:
  storageType: %TSo13HiddenCStructa = type <{ %Ts5Int32V, [4 x i8], %TSd }>
  kind: loadable
  bestKnownAlignment: 8
  abiAccessible: true
  triviallyDestroyable: true
  copyable: true
  bitwiseTakable: true
  bitwiseBorrowable: true
  fixedSizeMinimal: true
  fixedSizeMaximal: true
  loadable: true
  singleRetainablePointer: false
  fixedSize: 16
  fixedAlignment: 8
  fixedStride: 16
  spareBits: 18446744069414584320
  extraInhabitantCount: 0
  extraInhabitantMask: 0
  explosionSchema:
    - index: 0
      kind: scalar
      type: i32
    - index: 1
      kind: scalar
      type: double
  nativeParameterSchema:
    requiresIndirect: false
    components:
      - index: 0
        begin: 0
        end: 4
        type: i32
      - index: 1
        begin: 8
        end: 16
        type: double
  nativeReturnSchema:
    requiresIndirect: false
    components:
      - index: 0
        begin: 0
        end: 4
        type: i32
      - index: 1
        begin: 8
        end: 16
        type: double
//--- recovery.expected
=== Abstract type layout information ===
mangledName: $sSo13HiddenCStructa
origin: recovery
CanType:
  type: @_hidden("$sSo13HiddenCStructa")
  hasTypeParameter: false
  hasArchetype: false
  hasPrimaryArchetype: false
  hasLocalArchetype: false
  hasOpaqueArchetype: false
  hasOpenedExistential: false
  hasElementArchetype: false
  hasParameterPack: false
  hasPack: false
  hasPackArchetype: false
  hasParameterizedExistential: false
  hasDynamicSelf: false
  hasUnboundGeneric: false
  hasError: false
  hasBareError: false
  hasReferenceSemantics: false
  isAnyClassReferenceType: false
  isConstraintType: false
  isExistentialType: false
Minimal Type Lowering for lowered type: $@_hidden("$sSo13HiddenCStructa").
Expansion: Maximal
isTrivial: true.
isReferenceCounted: false.
isFixedABI: true.
isAddressOnly: false.
isResilient: false.
isTypeExpansionSensitive: false.
isInfinite: false.
isOrContainsRawPointer: false.
isLexical: false.
isOrContainsPack: false.
isAddressableForDependencies: true.
hasOnlyDefaultDeinit: true.
definitelyIsAddressableForDependencies: true.
definitelyIsOrContainsRawLayout: false.

IRGen Type Lowering for lowered type: $@_hidden("$sSo13HiddenCStructa").
Expansion: Maximal
isTrivial: true.
isReferenceCounted: false.
isFixedABI: true.
isAddressOnly: false.
isResilient: false.
isTypeExpansionSensitive: false.
isInfinite: false.
isOrContainsRawPointer: false.
isLexical: false.
isOrContainsPack: false.
isAddressableForDependencies: true.
hasOnlyDefaultDeinit: true.
definitelyIsAddressableForDependencies: true.
definitelyIsOrContainsRawLayout: false.

TypeInfo:
  storageType: <{ <{ i32 }>, [4 x i8], <{ double }> }>
  kind: loadable
  bestKnownAlignment: 8
  abiAccessible: true
  triviallyDestroyable: true
  copyable: true
  bitwiseTakable: true
  bitwiseBorrowable: true
  fixedSizeMinimal: true
  fixedSizeMaximal: true
  loadable: true
  singleRetainablePointer: false
  fixedSize: 16
  fixedAlignment: 8
  fixedStride: 16
  spareBits: 18446744069414584320
  extraInhabitantCount: 0
  extraInhabitantMask: 0
  explosionSchema:
    - index: 0
      kind: scalar
      type: i32
    - index: 1
      kind: scalar
      type: double
  nativeParameterSchema:
    requiresIndirect: false
    components:
      - index: 0
        begin: 0
        end: 4
        type: i32
      - index: 1
        begin: 8
        end: 16
        type: double
  nativeReturnSchema:
    requiresIndirect: false
    components:
      - index: 0
        begin: 0
        end: 4
        type: i32
      - index: 1
        begin: 8
        end: 16
        type: double
