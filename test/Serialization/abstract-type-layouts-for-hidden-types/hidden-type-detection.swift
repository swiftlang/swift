// Tests that complete layout fallbacks are scheduled for non-client-visible
// stored properties of ABI-exposed structs.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/InternalModule

// UNSUPPORTED: CPU=wasm32
// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -module-name Internal

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -skip-writing-abstract-type-layouts -emit-module -emit-module-path %t/PublicStruct.swiftmodule %t/PublicStruct.swift -I %t/InternalModule -parse-as-library -module-name PublicStruct -Rhidden-type-layout-serialization -verify -verify-additional-prefix member-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -skip-writing-abstract-type-layouts -emit-module -emit-module-path %t/PublicClass.swiftmodule %t/PublicClass.swift -I %t/InternalModule -parse-as-library -module-name PublicClass -Rhidden-type-layout-serialization -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -skip-writing-abstract-type-layouts -emit-module -emit-module-path %t/InternalStruct.swiftmodule %t/InternalStruct.swift -I %t/InternalModule -parse-as-library -module-name InternalStruct -Rhidden-type-layout-serialization -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -skip-writing-abstract-type-layouts -emit-module -emit-module-path %t/NestedPublicStruct.swiftmodule %t/NestedPublicStruct.swift -I %t/InternalModule -parse-as-library -module-name NestedPublicStruct -Rhidden-type-layout-serialization -verify -verify-additional-prefix member-

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -skip-writing-abstract-type-layouts -emit-module -emit-module-path %t/CompositeFields.swiftmodule %t/CompositeFields.swift -I %t/InternalModule -parse-as-library -module-name CompositeFields -Rhidden-type-layout-serialization -verify -verify-additional-prefix member-

// RUN: %target-swift-frontend -emit-module -emit-module-path %t/FlagDisabled.swiftmodule %t/PublicStruct.swift -I %t/InternalModule -parse-as-library -module-name FlagDisabled -Rhidden-type-layout-serialization -suppress-warnings -verify
// RUN: %llvm-bcanalyzer -dump %t/FlagDisabled.swiftmodule | %FileCheck %s --check-prefix NO-ABSTRACT-MEMBER-LAYOUT

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -enable-library-evolution -emit-module -emit-module-path %t/LibraryEvolution.swiftmodule %t/PublicStruct.swift -I %t/InternalModule -parse-as-library -module-name LibraryEvolution -Rhidden-type-layout-serialization -suppress-warnings -verify

// RUN: %target-swift-frontend -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes -internal-import-bridging-header %t/HiddenTypes.h -emit-module -emit-module-path %t/InternalBridgingHeader.swiftmodule %t/InternalBridgingHeader.swift -parse-as-library -module-name InternalBridgingHeader -Rhidden-type-layout-serialization -verify -verify-additional-prefix member-
// RUN: %llvm-bcanalyzer -dump %t/InternalBridgingHeader.swiftmodule | %FileCheck %s --check-prefix ABSTRACT-MEMBER-LAYOUT

//--- Internal.swift

public struct InternalType {
  public var value: Int64 = 0
  public init() {}
}

public protocol InternalProtocol {}

//--- HiddenTypes.h

typedef struct {
  int value;
} HiddenCStruct;

// ABSTRACT-MEMBER-LAYOUT: <ABSTRACT_TYPE_LAYOUT_INFO
// ABSTRACT-MEMBER-LAYOUT: <SIL_TYPE_PROPERTIES
// ABSTRACT-MEMBER-LAYOUT-DAG: <TYPE_INFO
// ABSTRACT-MEMBER-LAYOUT-DAG: <FIXED_TYPE_INFO
// ABSTRACT-MEMBER-LAYOUT-DAG: <LOADABLE_TYPE_INFO
// ABSTRACT-MEMBER-LAYOUT-DAG: <EXPLOSION_SCHEMA_ELEMENT
// ABSTRACT-MEMBER-LAYOUT-DAG: <RECORD_TYPE_INFO
// ABSTRACT-MEMBER-LAYOUT-DAG: <RECORD_FIELD
// ABSTRACT-MEMBER-LAYOUT-DAG: <LOADABLE_CLANG_RECORD_TYPE_INFO
// ABSTRACT-MEMBER-LAYOUT-DAG: <AGG_LOWERING_INPUT
// ABSTRACT-MEMBER-LAYOUT-DAG: <LLVM_TYPE

// NO-ABSTRACT-MEMBER-LAYOUT: <MODULE_BLOCK
// NO-ABSTRACT-MEMBER-LAYOUT-NOT: <ABSTRACT_TYPE_LAYOUT_INFO
// NO-ABSTRACT-MEMBER-LAYOUT: </MODULE_BLOCK>

//--- PublicStruct.swift

@_implementationOnly import Internal

public struct PublicWrapper {
  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'hidden' because it defines storage in ABI-exposed struct 'PublicWrapper' but is not exposed to clients}}
  private var hidden: InternalType
  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'ordinary' because it defines storage in ABI-exposed struct 'PublicWrapper' but is not exposed to clients}}
  private var ordinary: Int64 = 0
  @usableFromInline
  internal var usableFromInline: Int64 = 0
  public var visible: Int64 = 0

  public init() {
    self.hidden = InternalType()
  }
}

//--- PublicClass.swift

@_implementationOnly import Internal

public class PublicClassWrapper {
  private var hidden: InternalType
  public init() { self.hidden = InternalType() }
}

//--- InternalStruct.swift

@_implementationOnly import Internal

internal struct InternalWrapper {
  private var hidden: InternalType
  init() { self.hidden = InternalType() }
}

//--- NestedPublicStruct.swift

@_implementationOnly import Internal

public struct PublicOuter {
  public struct PublicNested {
    // expected-member-remark@+1 {{serializing abstract layout fallback for property 'hidden' because it defines storage in ABI-exposed struct 'PublicNested' but is not exposed to clients}}
    private var hidden: InternalType
    public init() { self.hidden = InternalType() }
  }

  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'nested' because it defines storage in ABI-exposed struct 'PublicOuter' but is not exposed to clients}}
  private var nested: PublicNested
  public init() { self.nested = PublicNested() }
}

//--- CompositeFields.swift

@_implementationOnly import Internal

private struct GenericStorage<Value: InternalProtocol> {
  var data: Int64
}

private struct LocalValue: InternalProtocol {}

public struct PublicCompositeWrapper {
  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'tuple' because it defines storage in ABI-exposed struct 'PublicCompositeWrapper' but is not exposed to clients}}
  private var tuple: (InternalType, Int64)
  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'closure' because it defines storage in ABI-exposed struct 'PublicCompositeWrapper' but is not exposed to clients}}
  private var closure: (InternalType) -> Void
  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'generic' because it defines storage in ABI-exposed struct 'PublicCompositeWrapper' but is not exposed to clients}}
  private var generic: GenericStorage<LocalValue>

  public init() {
    self.tuple = (InternalType(), 0)
    self.closure = { _ in }
    self.generic = GenericStorage(data: 0)
  }
}

//--- InternalBridgingHeader.swift

public struct PublicInternalBridgingHeaderWrapper {
  // expected-member-remark@+1 {{serializing abstract layout fallback for property 'hidden' because it defines storage in ABI-exposed struct 'PublicInternalBridgingHeaderWrapper' but is not exposed to clients}}
  private var hidden: HiddenCStruct
  public var visible: Int64 = 0
  public init() { self.hidden = HiddenCStruct(value: 0) }
}
