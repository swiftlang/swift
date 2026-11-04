// Test that layout-affecting member deserialization failures are fatal.
// Members that can be safely omitted retain the existing recovery behavior.

// REQUIRES: swift_feature_SerializeAbstractTypeLayoutForHiddenTypes

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %empty-directory(%t/dependencies)
// RUN: %empty-directory(%t/libraries)
// RUN: %empty-directory(%t/client-dependencies)

// Build hidden dependencies on a separate module search path, then omit that
// path from client compilations so that member types cannot be resolved.
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name Hidden %t/Hidden.swift \
// RUN:   -emit-module-path %t/dependencies/Hidden.swiftmodule

//--- Hidden.swift

public struct HiddenType {
  public init() {}
}

public protocol HiddenProtocol {}

@propertyWrapper
public struct ImportedHiddenWrapper {
  public var wrappedValue: Int

  public init(wrappedValue: Int) {
    self.wrappedValue = wrappedValue
  }
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name StructStorage %t/StructStorage.swift -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/StructStorage.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/StructStorageClient.swift -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=STRUCT-STORAGE

//--- StructStorage.swift

@_implementationOnly import Hidden

// If the client can't load HiddenType, it
// will ldrop the hidden property from its view
// of HasHiddenStorage and derive the wrong layout

public struct HasHiddenStorage {
  private var hidden: HiddenType
  // STRUCT-STORAGE: *** DESERIALIZATION FAILURE ***

  public init() { hidden = HiddenType() }
}

//--- StructStorageClient.swift

import StructStorage

public func use(_ value: HasHiddenStorage) -> HasHiddenStorage { value }

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name EnumPayload %t/EnumPayload.swift -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/EnumPayload.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/EnumPayloadClient.swift -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=ENUM-PAYLOAD

//--- EnumPayload.swift

@_implementationOnly import Hidden

// Without HiddenType, the client will drop the hidden case. That will
// lead to deriving an incorrect layout of HasHiddenPayload, and
// thus an incorrect layout of ContainsHiddenPayload.
private enum HasHiddenPayload {
  case hidden(HiddenType)
  // ENUM-PAYLOAD: *** DESERIALIZATION FAILURE ***
  case empty
}

public struct ContainsHiddenPayload {
  private var value: HasHiddenPayload

  public init() { value = .empty }
}

//--- EnumPayloadClient.swift

import EnumPayload

public func use(_ value: ContainsHiddenPayload) -> ContainsHiddenPayload {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name IndirectEnumPayload %t/IndirectEnumPayload.swift \
// RUN:   -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/IndirectEnumPayload.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/IndirectEnumPayloadClient.swift \
// RUN:   -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=INDIRECT-ENUM-PAYLOAD

//--- IndirectEnumPayload.swift

@_implementationOnly import Hidden

// Even though indirect enums do not store their payload inline,
// failing to load HiddenType can still result in the incorrect layout.
// The client will drop the hidden case all together, and then treat
// the enum as having no payload at all. In that case, it may assume
// a layout that only stores discriminator bits, and no pointer to a payload.
// Once again getting that wrong, generates incorrect layout, which generates
// miscompiles.

private indirect enum HasHiddenIndirectPayload {
  case hidden(HiddenType)
  // INDIRECT-ENUM-PAYLOAD: *** DESERIALIZATION FAILURE ***
  case empty
}

public struct ContainsHiddenIndirectPayload {
  private var value: HasHiddenIndirectPayload

  public init() { value = .empty }
}

//--- IndirectEnumPayloadClient.swift

import IndirectEnumPayload

public func use(
  _ value: ContainsHiddenIndirectPayload
) -> ContainsHiddenIndirectPayload {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name EnumElementRecovery %t/EnumElementRecovery.swift \
// RUN:   -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/EnumElementRecovery.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/EnumElementRecoveryClient.swift \
// RUN:   -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=ENUM-ELEMENT-RECOVERY

//--- EnumElementRecovery.swift

@_implementationOnly import Hidden

// In this case, the hidden type HiddenProtocol has no
// effect on the layout of LocalPayload. However, the client
// won't be able to resolve the full type signature of LocalPayload,
// will drop cases that reference it, and then derive incorrect layout.

private struct LocalArgument: HiddenProtocol {}

private struct LocalPayload<T: HiddenProtocol> {
  var value: Int64
}

private enum HasIndirectlyHiddenPayload {
  case hidden(LocalPayload<LocalArgument>)
  // ENUM-ELEMENT-RECOVERY: *** DESERIALIZATION FAILURE ***
  case empty
}

public struct ContainsIndirectlyHiddenPayload {
  private var value: HasIndirectlyHiddenPayload

  public init() { value = .empty }
}

//--- EnumElementRecoveryClient.swift

import EnumElementRecovery

public func use(
  _ value: ContainsIndirectlyHiddenPayload
) -> ContainsIndirectlyHiddenPayload {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name PropertyWrapperStorage \
// RUN:   %t/PropertyWrapperStorage.swift -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/PropertyWrapperStorage.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/PropertyWrapperStorageClient.swift \
// RUN:   -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=PROPERTY-WRAPPER

//--- PropertyWrapperStorage.swift

@_implementationOnly import Hidden


// Property wrappers define storage, including the wrapped type.
// If the client cannot resolve HiddenWrapper it will drop the hidden
// property, and derive the wrong layout for HasHiddenPropertyWrapperStorage
@propertyWrapper
private struct HiddenWrapper {
  var wrappedValue: HiddenType
  var extraValue = 5
}

public struct HasHiddenPropertyWrapperStorage {
  @HiddenWrapper private var hidden: HiddenType
  // PROPERTY-WRAPPER: *** DESERIALIZATION FAILURE ***

  public init() { hidden = HiddenType() }
}

//--- PropertyWrapperStorageClient.swift

import PropertyWrapperStorage

public func use(
  _ value: HasHiddenPropertyWrapperStorage
) -> HasHiddenPropertyWrapperStorage {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name ClosureStorage %t/ClosureStorage.swift \
// RUN:   -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/ClosureStorage.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/ClosureStorageClient.swift -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=CLOSURE-STORAGE

//--- ClosureStorage.swift

@_implementationOnly import Hidden

// Even though the layout of the callback stored property is theoretically derivable
// without knowing information about HiddenType, de-serialization will give up and drop
// the entire stored property, getting the layout of HasHiddenClosureStorage wrong
public struct HasHiddenClosureStorage {
  private var callback: (HiddenType) -> Void
  // CLOSURE-STORAGE: *** DESERIALIZATION FAILURE ***

  public init() { callback = { _ in } }
}

//--- ClosureStorageClient.swift

import ClosureStorage

public func use(
  _ value: HasHiddenClosureStorage
) -> HasHiddenClosureStorage {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name GenericSpecializationStorage \
// RUN:   %t/GenericSpecializationStorage.swift -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/GenericSpecializationStorage.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/GenericSpecializationStorageClient.swift \
// RUN:   -I %t/libraries \
// RUN:   2>&1 | %FileCheck %s --check-prefix=GENERIC-SPECIALIZATION

//--- GenericSpecializationStorage.swift

@_implementationOnly import Hidden

public struct LayoutIndependent<T> {
  public init() {}
}

// Again even though HiddenType as a generic parameter does not
// affect layout, value will be dropped by de-serialization.

public struct HasHiddenGenericSpecializationStorage {
  private var value: LayoutIndependent<HiddenType>
  // GENERIC-SPECIALIZATION: *** DESERIALIZATION FAILURE ***

  public init() { value = LayoutIndependent() }
}

//--- GenericSpecializationStorageClient.swift

import GenericSpecializationStorage

public func use(
  _ value: HasHiddenGenericSpecializationStorage
) -> HasHiddenGenericSpecializationStorage {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name RecoverableMembers %t/RecoverableMembers.swift \
// RUN:   -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/RecoverableMembers.swiftmodule
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/RecoverableMembersClient.swift \
// RUN:   -I %t/libraries \
// RUN:   -o /dev/null

//--- RecoverableMembers.swift

@_implementationOnly import Hidden

// This is to show we don't crash in cases
// where dropping a member is fine. The client
// will drop use, and won't be able to call the method,
// but this won't lead to incorrect layout.

public struct HasHiddenMethodSignature {
  public init() {}
  private func use(_ value: HiddenType) {}
}

//--- RecoverableMembersClient.swift

import RecoverableMembers

public func use(_ value: HasHiddenMethodSignature) -> HasHiddenMethodSignature {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name StaticPropertyWrapper \
// RUN:   %t/StaticPropertyWrapper.swift -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/StaticPropertyWrapper.swiftmodule
// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/StaticPropertyWrapperClient.swift \
// RUN:   -I %t/libraries \
// RUN:   -o /dev/null

//--- StaticPropertyWrapper.swift

@_implementationOnly import Hidden

// Similarly, static properties are not stored with a given instance
// of a value type, so dropping them doesn't lead to incorrect layout
// of the enclosing value type.
public struct HasHiddenStaticPropertyWrapper {
  @ImportedHiddenWrapper private static var hidden = 0

  public init() {}
}

//--- StaticPropertyWrapperClient.swift

import StaticPropertyWrapper

public func use(
  _ value: HasHiddenStaticPropertyWrapper
) -> HasHiddenStaticPropertyWrapper {
  value
}

// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name OrdinaryDependency %t/OrdinaryDependencyV1.swift \
// RUN:   -emit-module-path %t/dependencies/OrdinaryDependency.swiftmodule
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name OrdinaryStorage %t/OrdinaryStorage.swift \
// RUN:   -I %t/dependencies \
// RUN:   -emit-module-path %t/libraries/OrdinaryStorage.swiftmodule
// An ordinary imported module is required to be present. Supply a version in
// which the referenced declaration has disappeared to reach member recovery.
// RUN: %target-swift-frontend -emit-module -parse-as-library \
// RUN:   -module-name OrdinaryDependency %t/OrdinaryDependencyV2.swift \
// RUN:   -emit-module-path \
// RUN:   %t/client-dependencies/OrdinaryDependency.swiftmodule
// RUN: not --crash %target-swift-frontend \
// RUN:   -enable-experimental-feature SerializeAbstractTypeLayoutForHiddenTypes \
// RUN:   -emit-ir -parse-as-library \
// RUN:   -module-name Client %t/OrdinaryStorageClient.swift -I %t/libraries \
// RUN:   -I %t/client-dependencies \
// RUN:   2>&1 | %FileCheck %s --check-prefix=ORDINARY-STORAGE

// This case demonstrates that we don't always fail because of
// issues related to @_implementationOnly. In this case, nothing is hidden,
// but the version of the module OrdinaryDependency that was provided when OrdinaryStorage
// was built, is different from the one OrdinaryStorageClient was given.
// Under the feature flag, we diagnose this, as it is just as unsafe.

//--- OrdinaryDependencyV1.swift

public struct OrdinaryType {}

//--- OrdinaryDependencyV2.swift

public struct ReplacementType {}

//--- OrdinaryStorage.swift

import OrdinaryDependency

public struct HasOrdinaryStorage {
  private var value: OrdinaryType
  // ORDINARY-STORAGE: *** DESERIALIZATION FAILURE ***
}

//--- OrdinaryStorageClient.swift

import OrdinaryStorage

public func use(_ value: HasOrdinaryStorage) -> HasOrdinaryStorage { value }
