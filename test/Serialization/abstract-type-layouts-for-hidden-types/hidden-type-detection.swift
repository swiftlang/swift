// Tests that hidden types which contribute to a module's ABI are scheduled
// for layout serialization.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/InternalModule

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -module-name Internal

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PublicStructIOIField.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name PublicStructIOIField
// RUN: %llvm-bcanalyzer -dump %t/PublicStructIOIField.swiftmodule | %FileCheck %s --check-prefix=PUBLIC-STRUCT-IOI-FIELD
// PUBLIC-STRUCT-IOI-FIELD: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/InternalStructIOIField.swiftmodule %t/InternalStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name InternalStructIOIField
// RUN: %llvm-bcanalyzer -dump %t/InternalStructIOIField.swiftmodule | %FileCheck %s --check-prefix=INTERNAL-STRUCT-IOI-FIELD
// INTERNAL-STRUCT-IOI-FIELD-NOT: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/NestedStructLeak.swiftmodule %t/NestedStructLeak.swift -I %t/InternalModule -parse-as-library -module-name NestedStructLeak
// RUN: %llvm-bcanalyzer -dump %t/NestedStructLeak.swiftmodule | %FileCheck %s --check-prefix=NESTED-STRUCT-LEAK
// NESTED-STRUCT-LEAK: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/FlagDisabled.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name FlagDisabled
// RUN: %llvm-bcanalyzer -dump %t/FlagDisabled.swiftmodule | %FileCheck %s --check-prefix=FLAG-DISABLED
// FLAG-DISABLED-NOT: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/TupleLeak.swiftmodule %t/TupleLeak.swift -I %t/InternalModule -parse-as-library -module-name TupleLeak
// RUN: %llvm-bcanalyzer -dump %t/TupleLeak.swiftmodule | %FileCheck %s --check-prefix=TUPLE-LEAK
// TUPLE-LEAK: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/SiblingStructLeak.swiftmodule %t/SiblingStructLeak.swift -I %t/InternalModule -parse-as-library -module-name SiblingStructLeak
// RUN: %llvm-bcanalyzer -dump %t/SiblingStructLeak.swiftmodule | %FileCheck %s --check-prefix=SIBLING-STRUCT-LEAK
// SIBLING-STRUCT-LEAK: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericHardcodedIOI.swiftmodule %t/GenericHardcodedIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericHardcodedIOI
// RUN: %llvm-bcanalyzer -dump %t/GenericHardcodedIOI.swiftmodule | %FileCheck %s --check-prefix=GENERIC-HARDCODED-IOI
// GENERIC-HARDCODED-IOI: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericSpecializedIOI.swiftmodule %t/GenericSpecializedIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericSpecializedIOI
// RUN: %llvm-bcanalyzer -dump %t/GenericSpecializedIOI.swiftmodule | %FileCheck %s --check-prefix=GENERIC-SPECIALIZED-IOI
// GENERIC-SPECIALIZED-IOI: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericSpecializationOrder.swiftmodule %t/GenericSpecializationOrder.swift -I %t/InternalModule -parse-as-library -module-name GenericSpecializationOrder
// RUN: %llvm-bcanalyzer -dump %t/GenericSpecializationOrder.swiftmodule | %FileCheck %s --check-prefix=GENERIC-SPECIALIZATION-ORDER
// GENERIC-SPECIALIZATION-ORDER: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericParentSpecialization.swiftmodule %t/GenericParentSpecialization.swift -I %t/InternalModule -parse-as-library -module-name GenericParentSpecialization
// RUN: %llvm-bcanalyzer -dump %t/GenericParentSpecialization.swiftmodule | %FileCheck %s --check-prefix=GENERIC-PARENT-SPECIALIZATION
// GENERIC-PARENT-SPECIALIZATION: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericPhantomIOI.swiftmodule %t/GenericPhantomIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericPhantomIOI
// RUN: %llvm-bcanalyzer -dump %t/GenericPhantomIOI.swiftmodule | %FileCheck %s --check-prefix=GENERIC-PHANTOM-IOI
// GENERIC-PHANTOM-IOI-NOT: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PublicNestedType.swiftmodule %t/PublicNestedType.swift -I %t/InternalModule -parse-as-library -module-name PublicNestedType
// RUN: %llvm-bcanalyzer -dump %t/PublicNestedType.swiftmodule | %FileCheck %s --check-prefix=PUBLIC-NESTED-TYPE
// PUBLIC-NESTED-TYPE: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/OptionalHiddenPayload.swiftmodule %t/OptionalHiddenPayload.swift -I %t/InternalModule -parse-as-library -module-name OptionalHiddenPayload
// RUN: %llvm-bcanalyzer -dump %t/OptionalHiddenPayload.swiftmodule | %FileCheck %s --check-prefix=OPTIONAL-HIDDEN-PAYLOAD
// OPTIONAL-HIDDEN-PAYLOAD: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PrivateEnumIOIPayload.swiftmodule %t/PrivateEnumIOIPayload.swift -I %t/InternalModule -parse-as-library -module-name PrivateEnumIOIPayload
// RUN: %llvm-bcanalyzer -dump %t/PrivateEnumIOIPayload.swiftmodule | %FileCheck %s --check-prefix=PRIVATE-ENUM-IOI-PAYLOAD
// PRIVATE-ENUM-IOI-PAYLOAD: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -internal-import-bridging-header %t/HiddenTypes.h -emit-module -emit-module-path %t/PublicStructInternalBridgingHeaderField.swiftmodule %t/PublicStructInternalBridgingHeaderField.swift -parse-as-library -module-name PublicStructInternalBridgingHeaderField
// RUN: %llvm-bcanalyzer -dump %t/PublicStructInternalBridgingHeaderField.swiftmodule | %FileCheck %s --check-prefix=PUBLIC-STRUCT-INTERNAL-BRIDGING-HEADER-FIELD
// PUBLIC-STRUCT-INTERNAL-BRIDGING-HEADER-FIELD: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -import-bridging-header %t/HiddenTypes.h -emit-module -emit-module-path %t/PublicStructRegularBridgingHeaderField.swiftmodule %t/PublicStructRegularBridgingHeaderField.swift -parse-as-library -module-name PublicStructRegularBridgingHeaderField
// RUN: %llvm-bcanalyzer -dump %t/PublicStructRegularBridgingHeaderField.swiftmodule | %FileCheck %s --check-prefix=PUBLIC-STRUCT-REGULAR-BRIDGING-HEADER-FIELD
// PUBLIC-STRUCT-REGULAR-BRIDGING-HEADER-FIELD-NOT: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -internal-import-bridging-header %t/HiddenTypes.h -emit-module -emit-module-path %t/TransitiveInternalBridgingHeaderField.swiftmodule %t/TransitiveInternalBridgingHeaderField.swift -parse-as-library -module-name TransitiveInternalBridgingHeaderField
// RUN: %llvm-bcanalyzer -dump %t/TransitiveInternalBridgingHeaderField.swiftmodule | %FileCheck %s --check-prefix=TRANSITIVE-INTERNAL-BRIDGING-HEADER-FIELD
// TRANSITIVE-INTERNAL-BRIDGING-HEADER-FIELD: HIDDEN_TYPE_LAYOUT_INFO

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -enable-library-evolution -emit-module -emit-module-path %t/LibraryEvolution.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name LibraryEvolution
// RUN: %llvm-bcanalyzer -dump %t/LibraryEvolution.swiftmodule | %FileCheck %s --check-prefix=LIBRARY-EVOLUTION
// LIBRARY-EVOLUTION-NOT: HIDDEN_TYPE_LAYOUT_INFO

//--- Internal.swift

public struct InternalType {
  public var x: Int64 = 1
  public var y: Int64 = 2
  public init() {}
}

//--- HiddenTypes.h
typedef struct {
  int value;
} HiddenCStruct;

//--- PublicStructIOIField.swift
@_implementationOnly import Internal

public struct PublicWrapper {
  private var hidden: InternalType
  public var visible: Int64 = 1
  public init() { self.hidden = InternalType() }
}

//--- InternalStructIOIField.swift
@_implementationOnly import Internal

struct InternalWrapper {
  var hidden: InternalType
  var visible: Int64 = 1
  init() { self.hidden = InternalType() }
}

//--- NestedStructLeak.swift
@_implementationOnly import Internal

public struct A {
  private struct B {
    var ioi: InternalType
    init() { self.ioi = InternalType() }
  }
  private var b: B
  public var visible: Int64 = 1
  public init() { self.b = B() }
}

//--- TupleLeak.swift
@_implementationOnly import Internal

public struct TupleWrapper {
  private var hidden: (InternalType, Int64)
  public var visible: Int64 = 1
  public init() { self.hidden = (InternalType(), 0) }
}

//--- SiblingStructLeak.swift
@_implementationOnly import Internal

struct InternalHelper {
  var ioi: InternalType
  init() { self.ioi = InternalType() }
}

public struct PublicUser {
  private var helper: InternalHelper
  public var visible: Int64 = 1
  public init() { self.helper = InternalHelper() }
}

//--- GenericHardcodedIOI.swift
@_implementationOnly import Internal

struct GenericWithHardcoded<T> {
  var value: T
  var ioi: InternalType
  init(value: T) {
    self.value = value
    self.ioi = InternalType()
  }
}

public struct UsesGenericHardcoded {
  private var g: GenericWithHardcoded<Int64>
  public var visible: Int64 = 1
  public init() { self.g = GenericWithHardcoded(value: 0) }
}

//--- GenericSpecializedIOI.swift
@_implementationOnly import Internal

struct GenericWrapper<T> {
  var value: T
  init(value: T) { self.value = value }
}

public struct UsesGenericSpecialized {
  private var g: GenericWrapper<InternalType>
  public var visible: Int64 = 1
  public init() { self.g = GenericWrapper(value: InternalType()) }
}

//--- GenericSpecializationOrder.swift
@_implementationOnly import Internal

struct OrderDependentGeneric<T> {
  var value: T
}

public struct UsesMultipleSpecializations {
  private var visible: OrderDependentGeneric<Int64>
  private var hidden: OrderDependentGeneric<InternalType>

  public init() {
    self.visible = OrderDependentGeneric(value: 0)
    self.hidden = OrderDependentGeneric(value: InternalType())
  }
}

//--- GenericParentSpecialization.swift
@_implementationOnly import Internal

struct GenericParent<T> {
  struct Nested {
    var value: T
  }
}

public struct UsesMultipleParentSpecializations {
  private var visible: GenericParent<Int64>.Nested
  private var hidden: GenericParent<InternalType>.Nested

  public init() {
    self.visible = GenericParent<Int64>.Nested(value: 0)
    self.hidden = GenericParent<InternalType>.Nested(value: InternalType())
  }
}

//--- GenericPhantomIOI.swift
@_implementationOnly import Internal

struct GenericStructParameterDoesNotDefinedStorage<Tag> {
  var data: Int64
  init(data: Int64) { self.data = data }
}

public struct PublicStructUsingInternalGeneric {
  private var g: GenericStructParameterDoesNotDefinedStorage<InternalType>
  public var visible: Int64 = 1
  public init() { self.g = GenericStructParameterDoesNotDefinedStorage<InternalType>(data: 0) }
}

//--- PublicNestedType.swift
@_implementationOnly import Internal

public struct PublicOuter {
  public struct PublicNested {
    private var hidden: InternalType
    public init() { self.hidden = InternalType() }
  }

  public init() {}
}

//--- OptionalHiddenPayload.swift
@_implementationOnly import Internal

public struct UsesOptionalHiddenPayload {
  private var hidden: InternalType?
  public init() { self.hidden = InternalType() }
}

//--- PrivateEnumIOIPayload.swift
@_implementationOnly import Internal

private enum PrivateEnum {
  case a(InternalType)
  case b(Int64)
}

public struct PublicWithEnum {
  private var e: PrivateEnum
  public var visible: Int64 = 1
  public init() { self.e = .b(0) }
}

//--- PublicStructInternalBridgingHeaderField.swift
public struct PublicInternalBridgingHeaderWrapper {
  private var hidden: HiddenCStruct
  public var visible: Int64 = 1
  public init() { self.hidden = HiddenCStruct(value: 0) }
}

//--- PublicStructRegularBridgingHeaderField.swift
public struct PublicRegularBridgingHeaderWrapper {
  private var hidden: HiddenCStruct
  public var visible: Int64 = 1
  public init() { self.hidden = HiddenCStruct(value: 0) }
}

//--- TransitiveInternalBridgingHeaderField.swift
struct InternalBridgingHeaderHelper {
  var hidden: HiddenCStruct
  init() { self.hidden = HiddenCStruct(value: 0) }
}

public struct PublicTransitiveInternalBridgingHeaderWrapper {
  private var helper: InternalBridgingHeaderHelper
  public var visible: Int64 = 1
  public init() { self.helper = InternalBridgingHeaderHelper() }
}
