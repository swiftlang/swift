//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 9999, *)
extension AnonymousDescriptor {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt16
    
    @inlinable
    init(value: UInt16) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    static var hasMangledNameMask: UInt16 {
      0x1
    }
    
    @inlinable
    public var hasMangledName: Bool {
      value & Flags.hasMangledNameMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ConformanceDescriptor {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inline(__always)
    @inlinable
    static var typeReferenceKindMask: UInt32 {
      0x7 << 3
    }
    
    @inline(__always)
    @inlinable
    static var typeReferenceKindShift: UInt32 {
      3
    }
    
    @inline(__always)
    @inlinable
    static var isRetroactiveMask: UInt32 {
      0x1 << 6
    }
    
    @inline(__always)
    @inlinable
    static var isSynthesizedNonUniqueMask: UInt32 {
      0x1 << 7
    }
    
    @inline(__always)
    @inlinable
    static var numberOfConditionalRequirementsMask: UInt32 {
      0xFF << 8
    }
    
    @inline(__always)
    @inlinable
    static var numberOfConditionalRequirementsShift: UInt32 {
      8
    }
    
    @inline(__always)
    @inlinable
    static var hasResilientWitnessesMask: UInt32 {
      0x1 << 16
    }
    
    @inline(__always)
    @inlinable
    static var hasGenericWitnessTableMask: UInt32 {
      0x1 << 17
    }
    
    @inlinable
    public var typeReferenceKind: TypeReference.Kind {
      let masked = value & Flags.typeReferenceKindMask
      let shifted = masked &>> Flags.typeReferenceKindShift
      
      return TypeReference.Kind(UInt8(truncatingIfNeeded: shifted))
    }
    
    @inlinable
    public var isRetroactive: Bool {
      value & Flags.isRetroactiveMask != 0
    }
    
    @inlinable
    public var isSynthesizedNonUnique: Bool {
      value & Flags.isSynthesizedNonUniqueMask != 0
    }
    
    @inlinable
    public var numberOfConditionalRequirements: Int {
      let masked = value & Flags.numberOfConditionalRequirementsMask
      let shifted = masked &>> Flags.numberOfConditionalRequirementsShift
      
      return Int(truncatingIfNeeded: shifted)
    }
    
    @inlinable
    public var hasResilientWitnesses: Bool {
      value & Flags.hasResilientWitnessesMask != 0
    }
    
    @inlinable
    public var hasGenericWitnessTable: Bool {
      value & Flags.hasGenericWitnessTableMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ContextDescriptor {
  @frozen
  public struct Kind {
    @usableFromInline
    let value: UInt8
    
    @inlinable
    init(value: UInt8) {
      self.value = value
    }
    
    @inlinable
    @inline(__always)
    public static var module: Kind {
      Kind(value: 0)
    }
    
    @inlinable
    @inline(__always)
    public static var `extension`: Kind {
      Kind(value: 1)
    }
    
    @inlinable
    @inline(__always)
    public static var anonymous: Kind {
      Kind(value: 2)
    }
    
    @inlinable
    @inline(__always)
    public static var `protocol`: Kind {
      Kind(value: 3)
    }
    
    @inlinable
    @inline(__always)
    public static var opaqueType: Kind {
      Kind(value: 4)
    }
    
    @inlinable
    @inline(__always)
    public static var `class`: Kind {
      Kind(value: 16)
    }
    
    @inlinable
    @inline(__always)
    public static var `struct`: Kind {
      Kind(value: 17)
    }
    
    @inlinable
    @inline(__always)
    public static var `enum`: Kind {
      Kind(value: 18)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ContextDescriptor.Kind: Equatable {
  @inlinable
  public static func ==(
    lhs: ContextDescriptor.Kind,
    rhs: ContextDescriptor.Kind
  ) -> Bool {
    lhs.value == rhs.value
  }
}

@available(SwiftStdlib 9999, *)
extension ContextDescriptor {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inline(__always)
    @inlinable
    static var kindMask: UInt32 {
      0x1F
    }
    
    @inline(__always)
    @inlinable
    static var isUniqueMask: UInt32 {
      0x40
    }
    
    @inline(__always)
    @inlinable
    static var isGenericMask: UInt32 {
      0x80
    }
    
    @inline(__always)
    @inlinable
    static var versionMask: UInt32 {
      0xFF00
    }
    
    @inline(__always)
    @inlinable
    static var versionShift: UInt32 {
      8
    }
    
    @inline(__always)
    @inlinable
    static var kindSpecificFlagsMask: UInt32 {
      0xFFFF0000
    }
    
    @inline(__always)
    @inlinable
    static var kindSpecificFlagsShift: UInt32 {
      16
    }
    
    @inlinable
    public var kind: Kind {
      Kind(value: UInt8(truncatingIfNeeded: value & Flags.kindMask))
    }
    
    @inlinable
    public var isUnique: Bool {
      value & Flags.isUniqueMask != 0
    }
    
    @inlinable
    public var isGeneric: Bool {
      value & Flags.isGenericMask != 0
    }
    
    @inlinable
    public var version: UInt8 {
      let masked = value & Flags.versionMask
      let shifted = masked &>> Flags.versionShift
      
      return UInt8(truncatingIfNeeded: shifted)
    }
    
    @inlinable
    var kindSpecificFlags: UInt16 {
      let masked = value & Flags.kindSpecificFlagsMask
      let shifted = masked &>> Flags.kindSpecificFlagsShift
      
      return UInt16(truncatingIfNeeded: shifted)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ExtendedExistentialShape {
  @frozen
  public struct SpecialKind {
    @usableFromInline
    let value: UInt8
    
    @inlinable
    init(value: UInt8) {
      self.value = value
    }
    
    @inlinable
    @inline(__always)
    public static var none: SpecialKind {
      SpecialKind(value: 0x0)
    }
    
    @inlinable
    @inline(__always)
    public static var `class`: SpecialKind {
      SpecialKind(value: 0x1)
    }
    
    @inlinable
    @inline(__always)
    public static var metatype: SpecialKind {
      SpecialKind(value: 0x2)
    }
    
    @inlinable
    @inline(__always)
    public static var explicitLayout: SpecialKind {
      SpecialKind(value: 0x3)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ExtendedExistentialShape.SpecialKind: Equatable {
  @inlinable
  public static func ==(
    lhs: ExtendedExistentialShape.SpecialKind,
    rhs: ExtendedExistentialShape.SpecialKind
  ) -> Bool {
    lhs.value == rhs.value
  }
}

@available(SwiftStdlib 9999, *)
extension ExtendedExistentialShape {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inlinable
    @inline(__always)
    static var specialKindMask: UInt32 {
      0xFF
    }
    
    @inlinable
    @inline(__always)
    static var hasGeneralizationSignatureMask: UInt32 {
      0x100
    }
    
    @inlinable
    @inline(__always)
    static var hasTypeExpressionMask: UInt32 {
      0x200
    }
    
    @inlinable
    @inline(__always)
    static var hasSuggestedValueWitnessesMask: UInt32 {
      0x400
    }
    
    @inlinable
    @inline(__always)
    static var hasImplicitRequirementSignatureParametersMask: UInt32 {
      0x800
    }
    
    @inlinable
    @inline(__always)
    static var hasImplicitGeneralizationSignatureParametersMask: UInt32 {
      0x1000
    }
    
    @inlinable
    public var specialKind: SpecialKind {
      SpecialKind(
        value: UInt8(truncatingIfNeeded: value & Flags.specialKindMask)
      )
    }
    
    @inlinable
    public var hasGenerializationSignature: Bool {
      value & Flags.hasGeneralizationSignatureMask != 0
    }
    
    @inlinable
    public var hasTypeExpression: Bool {
      value & Flags.hasTypeExpressionMask != 0
    }
    
    @inlinable
    public var hasSuggestedValueWitnesses: Bool {
      value & Flags.hasSuggestedValueWitnessesMask != 0
    }
    
    @inlinable
    public var hasImplicitRequirementSignatureParameters: Bool {
      value & Flags.hasImplicitRequirementSignatureParametersMask != 0
    }
    
    @inlinable
    public var hasImplicitGeneralizationSignatureParameters: Bool {
      value & Flags.hasImplicitGeneralizationSignatureParametersMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension FieldDescriptor.Element {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inline(__always)
    @inlinable
    static var isIndirectCaseMask: UInt32 {
      0x1
    }
    
    @inline(__always)
    @inlinable
    static var isVarMask: UInt32 {
      0x2
    }
    
    @inlinable
    public var isIndirectCase: Bool {
      value & Flags.isIndirectCaseMask != 0
    }
    
    @inlinable
    public var isVar: Bool {
      value & Flags.isVarMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension GenericSignature {
  @frozen
  public struct ParameterDescriptor {
    @frozen
    public struct Kind {
      @usableFromInline
      let value: UInt8
      
      @inlinable
      init(value: UInt8) {
        self.value = value
      }
      
      @inline(__always)
      @inlinable
      public static var type: Kind {
        Kind(value: 0x0)
      }
    }
    
    @usableFromInline
    let value: UInt8
    
    @inline(__always)
    @inlinable
    static var kindMask: UInt8 {
      0x3F
    }
    
    @inline(__always)
    @inlinable
    static var hasExtraArgumentMask: UInt8 {
      0x40
    }
    
    @inline(__always)
    @inlinable
    static var hasKeyArgumentMask: UInt8 {
      0x80
    }
    
    @inlinable
    public var kind: Kind {
      Kind(value: value & ParameterDescriptor.kindMask)
    }
    
    @inlinable
    public var hasExtraArgument: Bool {
      value & ParameterDescriptor.hasExtraArgumentMask != 0
    }
    
    @inlinable
    public var hasKeyArgument: Bool {
      value & ParameterDescriptor.hasKeyArgumentMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension GenericSignature.RequirementDescriptor {
  @frozen
  public struct Kind {
    @usableFromInline
    let value: UInt8
    
    @inlinable
    init(value: UInt8) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    public static var `protocol`: Kind {
      Kind(value: 0x0)
    }
    
    @inline(__always)
    @inlinable
    public static var sameType: Kind {
      Kind(value: 0x1)
    }
    
    @inline(__always)
    @inlinable
    public static var baseClass: Kind {
      Kind(value: 0x2)
    }
    
    @inline(__always)
    @inlinable
    public static var sameConformance: Kind {
      Kind(value: 0x3)
    }
    
    @inline(__always)
    @inlinable
    public static var layout: Kind {
      Kind(value: 0x1F)
    }
  }
  
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inline(__always)
    @inlinable
    static var kindMask: UInt32 {
      0x1F
    }
    
    @inline(__always)
    @inlinable
    static var hasExtraArgumentMask: UInt32 {
      0x40
    }
    
    @inline(__always)
    @inlinable
    static var hasKeyArgumentMask: UInt32 {
      0x80
    }
    
    @inlinable
    public var kind: Kind {
      Kind(value: UInt8(truncatingIfNeeded: value & Flags.kindMask))
    }
    
    @inlinable
    public var hasExtraArgument: Bool {
      value & Flags.hasExtraArgumentMask != 0
    }
    
    @inlinable
    public var hasKeyArgument: Bool {
      value & Flags.hasKeyArgumentMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ProtocolDescriptor {
  @frozen
  public struct DispatchStrategy {
    @usableFromInline
    let value: UInt8
    
    @inlinable
    init(_ value: UInt8) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    public static var objc: DispatchStrategy {
      DispatchStrategy(0x0)
    }
    
    @inline(__always)
    @inlinable
    public static var swift: DispatchStrategy {
      DispatchStrategy(0x1)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ProtocolDescriptor.DispatchStrategy: Equatable {
  @inlinable
  public static func ==(
    lhs: ProtocolDescriptor.DispatchStrategy,
    rhs: ProtocolDescriptor.DispatchStrategy
  ) -> Bool {
    lhs.value == rhs.value
  }
}

@available(SwiftStdlib 9999, *)
extension ProtocolDescriptor {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt16
    
    @inlinable
    init(value: UInt16) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    static var isSwiftMask: UInt16 {
      0x1
    }
    
    @inline(__always)
    @inlinable
    static var isClassConstrainedMask: UInt16 {
      0x2
    }
    
    @inline(__always)
    @inlinable
    static var dispatchStrategyMask: UInt16 {
      0xF << 2
    }
    
    @inline(__always)
    @inlinable
    static var dispatchStrategyShift: UInt16 {
      2
    }
    
    @inline(__always)
    @inlinable
    static var specialProtocolMask: UInt16 {
      0x3C0
    }
    
    @inline(__always)
    @inlinable
    static var specialProtocolShift: UInt16 {
      6
    }
    
    @inline(__always)
    @inlinable
    static var isResilientMask: UInt16 {
      0x1 << 10
    }
    
    @inlinable
    public var isSwift: Bool {
      value & Flags.isSwiftMask != 0
    }
    
    @inlinable
    public var isClassConstrained: Bool {
      // 0 = Class, 1 = Any
      value & Flags.isClassConstrainedMask == 0
    }
    
    @inlinable
    public var dispatchStrategy: DispatchStrategy {
      let masked = value & Flags.dispatchStrategyMask
      let shifted = masked &>> Flags.dispatchStrategyShift
      
      return DispatchStrategy(UInt8(truncatingIfNeeded: shifted))
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ProtocolRequirement {
  @frozen
  public struct Kind {
    @usableFromInline
    let value: UInt8
    
    @inlinable
    init(value: UInt8) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    public static var baseProtocol: Kind {
      Kind(value: 0x0)
    }
    
    @inline(__always)
    @inlinable
    public static var method: Kind {
      Kind(value: 0x1)
    }
    
    @inline(__always)
    @inlinable
    public static var `init`: Kind {
      Kind(value: 0x2)
    }
    
    @inline(__always)
    @inlinable
    public static var getter: Kind {
      Kind(value: 0x3)
    }
    
    @inline(__always)
    @inlinable
    public static var setter: Kind {
      Kind(value: 0x4)
    }
    
    @inline(__always)
    @inlinable
    public static var readCoroutine: Kind {
      Kind(value: 0x5)
    }
    
    @inline(__always)
    @inlinable
    public static var modifyCoroutine: Kind {
      Kind(value: 0x6)
    }
    
    @inline(__always)
    @inlinable
    public static var associatedTypeAccessFunction: Kind {
      Kind(value: 0x7)
    }
    
    @inline(__always)
    @inlinable
    public static var associatedConformanceAccessFunction: Kind {
      Kind(value: 0x8)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension ProtocolRequirement.Kind: Equatable {
  @inlinable
  public static func ==(
    lhs: ProtocolRequirement.Kind,
    rhs: ProtocolRequirement.Kind
  ) -> Bool {
    lhs.value == rhs.value
  }
}

@available(SwiftStdlib 9999, *)
extension ProtocolRequirement {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inline(__always)
    @inlinable
    static var kindMask: UInt32 {
      0xF
    }
    
    @inline(__always)
    @inlinable
    static var isInstanceMask: UInt32 {
      0x10
    }
    
    @inline(__always)
    @inlinable
    static var isAsyncMask: UInt32 {
      0x20
    }
    
    @inline(__always)
    @inlinable
    static var extraDiscriminatorShift: UInt32 {
      16
    }
    
    @inlinable
    public var kind: Kind {
      Kind(value: UInt8(truncatingIfNeeded: value & Flags.kindMask))
    }
    
    @inlinable
    public var isInstance: Bool {
      value & Flags.isInstanceMask != 0
    }
    
    @inlinable
    public var isAsync: Bool {
      value & Flags.isAsyncMask != 0
    }
    
    @inlinable
    public var extraDiscriminator: UInt16 {
      UInt16(truncatingIfNeeded: value &>> Flags.extraDiscriminatorShift)
    }
  }
}

@frozen
public struct MetadataInitializationKind {
  @usableFromInline
  let value: UInt8
  
  @inlinable
  init(_ value: UInt8) {
    self.value = value
  }
  
  @inline(__always)
  @inlinable
  public static var none: MetadataInitializationKind {
    MetadataInitializationKind(0x0)
  }
  
  @inline(__always)
  @inlinable
  public static var single: MetadataInitializationKind {
    MetadataInitializationKind(0x1)
  }
  
  @inline(__always)
  @inlinable
  public static var foreign: MetadataInitializationKind {
    MetadataInitializationKind(0x2)
  }
}

@available(SwiftStdlib 9999, *)
extension TypeDescriptor {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt16
    
    @inlinable
    init(value: UInt16) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    static var metadataInitializationKindMask: UInt16 {
      0x3
    }
    
    @inline(__always)
    @inlinable
    static var hasImportInfoMask: UInt16 {
      0x4
    }
    
    @inline(__always)
    @inlinable
    static var hasCanonicalMetadataPrespecializationsMask: UInt16 {
      0x8
    }
    
    @inline(__always)
    @inlinable
    static var classIsActorMask: UInt16 {
      0x80
    }
    
    @inline(__always)
    @inlinable
    static var classIsDefaultActorMask: UInt16 {
      0x100
    }
    
    @inline(__always)
    @inlinable
    static var classResilientSuperclassReferenceKindMask: UInt16 {
      0xE00
    }
    
    @inline(__always)
    @inlinable
    static var classResilientSuperclassReferenceKindShift: UInt16 {
      9
    }
    
    @inline(__always)
    @inlinable
    static var classAreImmediateMembersNegativeMask: UInt16 {
      0x1000
    }
    
    @inline(__always)
    @inlinable
    static var classHasResilientSuperclassMask: UInt16 {
      0x2000
    }
    
    @inline(__always)
    @inlinable
    static var classHasOverrideTableMask: UInt16 {
      0x4000
    }
    
    @inline(__always)
    @inlinable
    static var classHasVtableMask: UInt16 {
      0x8000
    }
    
    @inlinable
    public var metadataInitializationKind: MetadataInitializationKind {
      MetadataInitializationKind(
        UInt8(truncatingIfNeeded: value & Flags.metadataInitializationKindMask)
      )
    }
    
    @inlinable
    public var hasImportInfo: Bool {
      value & Flags.hasImportInfoMask != 0
    }
    
    @inlinable
    public var hasCanonicalMetadataPrespecializations: Bool {
      value & Flags.hasCanonicalMetadataPrespecializationsMask != 0
    }
    
    @inlinable
    public var classIsActor: Bool {
      value & Flags.classIsActorMask != 0
    }
    
    @inlinable
    public var classIsDefaultActor: Bool {
      value & Flags.classIsDefaultActorMask != 0
    }
    
    public var classResilientSuperclassReferenceKind: TypeReference.Kind {
      let masked = value & Flags.classResilientSuperclassReferenceKindMask
      let shifted = masked &>> Flags.classResilientSuperclassReferenceKindShift
      
      return TypeReference.Kind(UInt8(truncatingIfNeeded: shifted))
    }
    
    @inlinable
    public var classAreImmediateMembersNegative: Bool {
      value & Flags.classAreImmediateMembersNegativeMask != 0
    }
    
    @inlinable
    public var classHasResilientSuperclass: Bool {
      value & Flags.classHasResilientSuperclassMask != 0
    }
    
    @inlinable
    public var classHasOverrideTable: Bool {
      value & Flags.classHasOverrideTableMask != 0
    }
    
    @inlinable
    public var classHasVtable: Bool {
      value & Flags.classHasVtableMask != 0
    }
  }
}

@available(SwiftStdlib 9999, *)
extension TypeReference {
  @frozen
  public struct Kind {
    @usableFromInline
    let value: UInt8
    
    @inlinable
    init(_ value: UInt8) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    public static var directDescriptor: Kind {
      Kind(0x0)
    }
    
    @inline(__always)
    @inlinable
    public static var indirectDescriptor: Kind {
      Kind(0x1)
    }
    
    @inline(__always)
    @inlinable
    public static var directObjCClass: Kind {
      Kind(0x2)
    }
    
    @inline(__always)
    @inlinable
    public static var indirectObjCClass: Kind {
      Kind(0x3)
    }
  }
}

@available(SwiftStdlib 9999, *)
extension TypeReference.Kind: Equatable {
  @inlinable
  public static func ==(
    lhs: TypeReference.Kind,
    rhs: TypeReference.Kind
  ) -> Bool {
    lhs.value == rhs.value
  }
}
