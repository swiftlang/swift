// Tests that hidden types which contribute to a module's ABI are scheduled
// for layout serialization.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/InternalModule

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -module-name Internal

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PublicStructIOIField.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name PublicStructIOIField -Rhidden-type-layout-serialization -verify -verify-additional-prefix implementation-only-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PublicClassIOIField.swiftmodule %t/PublicClassIOIField.swift -I %t/InternalModule -parse-as-library -module-name PublicClassIOIField -Rhidden-type-layout-serialization -verify -verify-additional-prefix implementation-only-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/ABIExposedInternalClassIOIField.swiftmodule %t/ABIExposedInternalClassIOIField.swift -I %t/InternalModule -parse-as-library -module-name ABIExposedInternalClassIOIField -Rhidden-type-layout-serialization -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/InternalStructIOIField.swiftmodule %t/InternalStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name InternalStructIOIField -Rhidden-type-layout-serialization -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/NestedStructLeak.swiftmodule %t/NestedStructLeak.swift -I %t/InternalModule -parse-as-library -module-name NestedStructLeak -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/FlagDisabled.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name FlagDisabled -Rhidden-type-layout-serialization -suppress-warnings -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/TupleLeak.swiftmodule %t/TupleLeak.swift -I %t/InternalModule -parse-as-library -module-name TupleLeak -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/SiblingStructLeak.swiftmodule %t/SiblingStructLeak.swift -I %t/InternalModule -parse-as-library -module-name SiblingStructLeak -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericHardcodedIOI.swiftmodule %t/GenericHardcodedIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericHardcodedIOI -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericSpecializedIOI.swiftmodule %t/GenericSpecializedIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericSpecializedIOI -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericSpecializationOrder.swiftmodule %t/GenericSpecializationOrder.swift -I %t/InternalModule -parse-as-library -module-name GenericSpecializationOrder -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericParentSpecialization.swiftmodule %t/GenericParentSpecialization.swift -I %t/InternalModule -parse-as-library -module-name GenericParentSpecialization -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericPhantomIOI.swiftmodule %t/GenericPhantomIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericPhantomIOI -Rhidden-type-layout-serialization -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PublicNestedType.swiftmodule %t/PublicNestedType.swift -I %t/InternalModule -parse-as-library -module-name PublicNestedType -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/GenericEnumHiddenPayload.swiftmodule %t/GenericEnumHiddenPayload.swift -I %t/InternalModule -parse-as-library -module-name GenericEnumHiddenPayload -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/PrivateEnumIOIPayload.swiftmodule %t/PrivateEnumIOIPayload.swift -I %t/InternalModule -parse-as-library -module-name PrivateEnumIOIPayload -Rhidden-type-layout-serialization -verify -verify-additional-prefix hidden-layout-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/IndirectEnumIOIPayload.swiftmodule %t/IndirectEnumIOIPayload.swift -I %t/InternalModule -parse-as-library -module-name IndirectEnumIOIPayload -Rhidden-type-layout-serialization -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -internal-import-bridging-header %t/HiddenTypes.h -emit-module -emit-module-path %t/PublicStructInternalBridgingHeaderField.swiftmodule %t/PublicStructInternalBridgingHeaderField.swift -parse-as-library -module-name PublicStructInternalBridgingHeaderField -Rhidden-type-layout-serialization -verify -verify-additional-prefix internal-bridging-header-
// RUN: %llvm-bcanalyzer -dump %t/PublicStructInternalBridgingHeaderField.swiftmodule | %FileCheck %s --check-prefix HIDDEN-CLANG-RECORD

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -enable-library-evolution -emit-module -emit-module-path %t/LibraryEvolution.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name LibraryEvolution -Rhidden-type-layout-serialization -suppress-warnings -verify

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

// HIDDEN-CLANG-RECORD: <HIDDEN_LOADABLE_CLANG_RECORD

//--- PublicStructIOIField.swift
@_implementationOnly import Internal

public struct PublicWrapper {
  // expected-implementation-only-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of struct 'PublicWrapper' through property 'hidden'}}
  private var hidden: InternalType
  public var visible: Int64 = 1
  public init() { self.hidden = InternalType() }
}

//--- PublicClassIOIField.swift
@_implementationOnly import Internal

public class PublicClassWrapper {
  // expected-implementation-only-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of class 'PublicClassWrapper' through property 'hidden'}}
  private var hidden: InternalType
  public init() { self.hidden = InternalType() }
}

//--- ABIExposedInternalClassIOIField.swift
@_implementationOnly import Internal

@usableFromInline
internal class ABIExposedInternalClassWrapper {
  private var hidden: InternalType
  init() { self.hidden = InternalType() }
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

// expected-hidden-layout-note@+1 {{layout of struct 'B' is ABI-exposed through struct 'A'}}
public struct A {
  private struct B {
    // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of struct 'B' through property 'ioi'}}
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
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of struct 'TupleWrapper' through property 'hidden'}}
  private var hidden: (InternalType, Int64)
  public var visible: Int64 = 1
  public init() { self.hidden = (InternalType(), 0) }
}

//--- SiblingStructLeak.swift
@_implementationOnly import Internal

struct InternalHelper {
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of struct 'InternalHelper' through property 'ioi'}}
  var ioi: InternalType
  init() { self.ioi = InternalType() }
}

// expected-hidden-layout-note@+1 {{layout of struct 'InternalHelper' is ABI-exposed through struct 'PublicUser'}}
public struct PublicUser {
  private var helper: InternalHelper
  public var visible: Int64 = 1
  public init() { self.helper = InternalHelper() }
}

//--- GenericHardcodedIOI.swift
@_implementationOnly import Internal

struct GenericWithHardcoded<T> {
  var value: T
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of generic struct 'GenericWithHardcoded' through property 'ioi'}}
  var ioi: InternalType
  init(value: T) {
    self.value = value
    self.ioi = InternalType()
  }
}

// expected-hidden-layout-note@+1 {{layout of generic struct 'GenericWithHardcoded' is ABI-exposed through struct 'UsesGenericHardcoded'}}
public struct UsesGenericHardcoded {
  private var g: GenericWithHardcoded<Int64>
  public var visible: Int64 = 1
  public init() { self.g = GenericWithHardcoded(value: 0) }
}

//--- GenericSpecializedIOI.swift
@_implementationOnly import Internal

struct GenericWrapper<T> {
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of generic struct 'GenericWrapper' through property 'value'}}
  var value: T
  init(value: T) { self.value = value }
}

// expected-hidden-layout-note@+1 {{layout of generic struct 'GenericWrapper' is ABI-exposed through struct 'UsesGenericSpecialized'}}
public struct UsesGenericSpecialized {
  private var g: GenericWrapper<InternalType>
  public var visible: Int64 = 1
  public init() { self.g = GenericWrapper(value: InternalType()) }
}

//--- GenericSpecializationOrder.swift
@_implementationOnly import Internal

struct OrderDependentGeneric<T> {
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of generic struct 'OrderDependentGeneric' through property 'value'}}
  var value: T
}

// expected-hidden-layout-note@+1 {{layout of generic struct 'OrderDependentGeneric' is ABI-exposed through struct 'UsesMultipleSpecializations'}}
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
    // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of struct 'Nested' through property 'value'}}
    var value: T
  }
}

// expected-hidden-layout-note@+1 {{layout of struct 'Nested' is ABI-exposed through struct 'UsesMultipleParentSpecializations'}}
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
    // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of struct 'PublicNested' through property 'hidden'}}
    private var hidden: InternalType
    public init() { self.hidden = InternalType() }
  }

  public init() {}
}

//--- GenericEnumHiddenPayload.swift
@_implementationOnly import Internal

private enum GenericPayload<T> {
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of generic enum 'GenericPayload' through enum case 'value'}}
  case value(T)
}

// expected-hidden-layout-note@+1 {{layout of generic enum 'GenericPayload' is ABI-exposed through struct 'UsesGenericEnumHiddenPayload'}}
public struct UsesGenericEnumHiddenPayload {
  private var hidden: GenericPayload<InternalType>
  public init() { self.hidden = .value(InternalType()) }
}

//--- PrivateEnumIOIPayload.swift
@_implementationOnly import Internal

private enum PrivateEnum {
  // expected-hidden-layout-remark@+1 {{serializing abstract layout for hidden type 'InternalType' because its defining module was imported with '@_implementationOnly' and it contributes to the ABI-exposed layout of enum 'PrivateEnum' through enum case 'a'}}
  case a(InternalType)
  case b(Int64)
}

// expected-hidden-layout-note@+1 {{layout of enum 'PrivateEnum' is ABI-exposed through struct 'PublicWithEnum'}}
public struct PublicWithEnum {
  private var e: PrivateEnum
  public var visible: Int64 = 1
  public init() { self.e = .b(0) }
}

//--- IndirectEnumIOIPayload.swift
@_implementationOnly import Internal

private indirect enum PrivateIndirectEnum {
  case hidden(InternalType)
  case empty
}

private enum PrivateEnumWithIndirectCase {
  indirect case hidden(InternalType)
  case empty
}

public struct PublicWithIndirectEnums {
  private var indirectEnum: PrivateIndirectEnum
  private var indirectCase: PrivateEnumWithIndirectCase

  public init() {
    self.indirectEnum = .empty
    self.indirectCase = .empty
  }
}

//--- PublicStructInternalBridgingHeaderField.swift
public struct PublicInternalBridgingHeaderWrapper {
  // expected-internal-bridging-header-remark@+1 {{serializing abstract layout for hidden type 'HiddenCStruct' because it was imported through '-internal-import-bridging-header' and contributes to the ABI-exposed layout of struct 'PublicInternalBridgingHeaderWrapper' through property 'hidden'}}
  private var hidden: HiddenCStruct
  public var visible: Int64 = 1
  public init() { self.hidden = HiddenCStruct(value: 0) }
}
