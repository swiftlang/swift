// Tests that we emit layout representations for @_implementationOnly types
// in the appropriate circumstances.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: mkdir -p %t/InternalModule

// UNSUPPORTED: CPU=wasm32

// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalModule/Internal.swiftmodule %t/Internal.swift -parse-as-library -module-name Internal

// --- Public struct with private stored property of IOI type -- should emit a record
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/PublicStructIOIField.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name PublicStructIOIField
// RUN: %llvm-bcanalyzer -dump %t/PublicStructIOIField.swiftmodule | %FileCheck %s --check-prefix=PUBLIC-STRUCT-IOI-FIELD
// PUBLIC-STRUCT-IOI-FIELD: HIDDEN_STRUCT_TYPE

// --- Internal struct — not visible to clients -- should not emit a record
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/InternalStructIOIField.swiftmodule %t/InternalStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name InternalStructIOIField
// RUN: %llvm-bcanalyzer -dump %t/InternalStructIOIField.swiftmodule | %FileCheck %s --check-prefix=INTERNAL-STRUCT-IOI-FIELD
// INTERNAL-STRUCT-IOI-FIELD-NOT: HIDDEN_STRUCT_TYPE

// --- Leaking by defining storage on an ABI accessible but not API accessible nested struct -- should emit a record
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/NestedStructLeak.swiftmodule %t/NestedStructLeak.swift -I %t/InternalModule -parse-as-library -module-name NestedStructLeak
// RUN: %llvm-bcanalyzer -dump %t/NestedStructLeak.swiftmodule | %FileCheck %s --check-prefix=NESTED-STRUCT-LEAK
// NESTED-STRUCT-LEAK: HIDDEN_STRUCT_TYPE

// --- Feature flag not enabled — no hidden records even for leaking types.
// RUN: %target-swift-frontend -emit-module -emit-module-path %t/FlagDisabled.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name FlagDisabled
// RUN: %llvm-bcanalyzer -dump %t/FlagDisabled.swiftmodule | %FileCheck %s --check-prefix=FLAG-DISABLED
// FLAG-DISABLED-NOT: HIDDEN_STRUCT_TYPE

// --- Test that we recurse into tuples to detect leaking types
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/TupleLeak.swiftmodule %t/TupleLeak.swift -I %t/InternalModule -parse-as-library -module-name TupleLeak
// RUN: %llvm-bcanalyzer -dump %t/TupleLeak.swiftmodule | %FileCheck %s --check-prefix=TUPLE-LEAK
// TUPLE-LEAK: HIDDEN_STRUCT_TYPE

// --- Another case of ABI accessible but not API accessible struct, this time internal sibling struct instead of nested -- should emit a record
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/SiblingStructLeak.swiftmodule %t/SiblingStructLeak.swift -I %t/InternalModule -parse-as-library -module-name SiblingStructLeak
// RUN: %llvm-bcanalyzer -dump %t/SiblingStructLeak.swiftmodule | %FileCheck %s --check-prefix=SIBLING-STRUCT-LEAK
// SIBLING-STRUCT-LEAK: HIDDEN_STRUCT_TYPE

// --- Transitive leak through a sibling generic struct that contains a hardcoded IOI type (not via the generic parameter) -- should emit
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/GenericHardcodedIOI.swiftmodule %t/GenericHardcodedIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericHardcodedIOI
// RUN: %llvm-bcanalyzer -dump %t/GenericHardcodedIOI.swiftmodule | %FileCheck %s --check-prefix=GENERIC-HARDCODED-IOI
// GENERIC-HARDCODED-IOI: HIDDEN_STRUCT_TYPE

// --- Generic ABI accessible sibling struct specialized on IOI type that defines storage -- should emit
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/GenericSpecializedIOI.swiftmodule %t/GenericSpecializedIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericSpecializedIOI
// RUN: %llvm-bcanalyzer -dump %t/GenericSpecializedIOI.swiftmodule | %FileCheck %s --check-prefix=GENERIC-SPECIALIZED-IOI
// GENERIC-SPECIALIZED-IOI: HIDDEN_STRUCT_TYPE

// --- ABI accessible generic struct, specialized on IOI type, but not such that it defines storage -- should not emit
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/GenericPhantomIOI.swiftmodule %t/GenericPhantomIOI.swift -I %t/InternalModule -parse-as-library -module-name GenericPhantomIOI
// RUN: %llvm-bcanalyzer -dump %t/GenericPhantomIOI.swiftmodule | %FileCheck %s --check-prefix=GENERIC-PHANTOM-IOI
// GENERIC-PHANTOM-IOI-NOT: HIDDEN_STRUCT_TYPE

// --- Private enum with IOI payload, used as stored property on public struct -- should emit
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -emit-module -emit-module-path %t/PrivateEnumIOIPayload.swiftmodule %t/PrivateEnumIOIPayload.swift -I %t/InternalModule -parse-as-library -module-name PrivateEnumIOIPayload
// RUN: %llvm-bcanalyzer -dump %t/PrivateEnumIOIPayload.swiftmodule | %FileCheck %s --check-prefix=PRIVATE-ENUM-IOI-PAYLOAD
// PRIVATE-ENUM-IOI-PAYLOAD: HIDDEN_STRUCT_TYPE

// --- Library evolution enabled — never emit records, there is no need
// RUN: %target-swift-frontend -enable-experimental-feature SafeImplementationOnly -enable-library-evolution -emit-module -emit-module-path %t/LibraryEvolution.swiftmodule %t/PublicStructIOIField.swift -I %t/InternalModule -parse-as-library -module-name LibraryEvolution
// RUN: %llvm-bcanalyzer -dump %t/LibraryEvolution.swiftmodule | %FileCheck %s --check-prefix=LIBRARY-EVOLUTION
// LIBRARY-EVOLUTION-NOT: HIDDEN_STRUCT_TYPE

// REQUIRES: swift_feature_SafeImplementationOnly

//--- Internal.swift

public struct InternalType {
  public var x: Int64 = 1
  public var y: Int64 = 2
  public init() {}
}

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
