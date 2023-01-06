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

extension Metadata {
  @frozen
  public struct Kind {
    @usableFromInline
    let value: Int
    
    @inlinable
    init(_ value: Int) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    static var isRuntimePrivate: Int {
      0x100
    }
    
    @inline(__always)
    @inlinable
    static var isNonHeap: Int {
      0x200
    }
    
    @inline(__always)
    @inlinable
    static var isNonType: Int {
      0x400
    }
    
    @inline(__always)
    @inlinable
    public static var `class`: Kind {
      Kind(0x0)
    }
    
    @inline(__always)
    @inlinable
    public static var `struct`: Kind {
      Kind(0x0 | isNonHeap)
    }
    
    @inline(__always)
    @inlinable
    public static var `enum`: Kind {
      Kind(0x1 | isNonHeap)
    }
    
    @inline(__always)
    @inlinable
    public static var optional: Kind {
      Kind(0x2 | isNonHeap)
    }
    
    @inline(__always)
    @inlinable
    public static var foreignClass: Kind {
      Kind(0x3 | isNonHeap)
    }
    
    @inline(__always)
    @inlinable
    public static var foreignReferenceType: Kind {
      Kind(0x4 | isNonHeap)
    }
    
    public static var opaque: Kind {
      Kind(0x0 | isRuntimePrivate | isNonHeap)
    }
    
    public static var tuple: Kind {
      Kind(0x1 | isRuntimePrivate | isNonHeap)
    }
    
    public static var function: Kind {
      Kind(0x2 | isRuntimePrivate | isNonHeap)
    }
    
    public static var existential: Kind {
      Kind(0x3 | isRuntimePrivate | isNonHeap)
    }
    
    public static var metatype: Kind {
      Kind(0x4 | isRuntimePrivate | isNonHeap)
    }
    
    public static var objcClassWrapper: Kind {
      Kind(0x5 | isRuntimePrivate | isNonHeap)
    }
    
    public static var existentialMetatype: Kind {
      Kind(0x6 | isRuntimePrivate | isNonHeap)
    }
    
    public static var extendedExistential: Kind {
      Kind(0x7 | isRuntimePrivate | isNonHeap)
    }
    
    public static var heapLocalVariable: Kind {
      Kind(0x0 | isNonType)
    }
    
    public static var heapGenericLocalVariable: Kind {
      Kind(0x0 | isRuntimePrivate | isNonType)
    }
    
    public static var errorObject: Kind {
      Kind(0x1 | isRuntimePrivate | isNonType)
    }
    
    public static var task: Kind {
      Kind(0x2 | isRuntimePrivate | isNonType)
    }
    
    public static var job: Kind {
      Kind(0x3 | isRuntimePrivate | isNonType)
    }
  }
}

extension Metadata.Kind: Equatable {
  @inlinable
  public static func ==(_ lhs: Metadata.Kind, _ rhs: Metadata.Kind) -> Bool {
    // On Darwin platforms, the metadata kind for class types is the ObjC isa
    // pointer value which is guaranteed to be >= 0x800.
    switch (lhs.value, rhs.value) {
    case (0x800..., 0x0):
      return true
    case (0x0, 0x800...):
      return true
    case (0x800..., 0x800...):
      return true
    default:
      return lhs.value == rhs.value
    }
  }
}

extension Metadata {
  @frozen
  public struct Request {
    @usableFromInline
    let value: Int
    
    @inline(__always)
    @inlinable
    static var isNonBlockingMask: Int {
      0x100
    }
    
    @inlinable
    init(state: State, isBlocking: Bool = true) {
      if !isBlocking {
        self.value = state.value & Request.isNonBlockingMask
      } else {
        self.value = state.value
      }
    }
    
    @inline(__always)
    @inlinable
    public static var complete: Request {
      Request(state: .complete)
    }
  }
  
  @frozen
  @usableFromInline
  struct Response {
    @usableFromInline
    let metadata: Metadata
    
    let state: State
  }
  
  @frozen
  @usableFromInline
  struct State {
    @usableFromInline
    let value: Int
    
    @inlinable
    init(value: Int) {
      self.value = value
    }
    
    @inline(__always)
    @inlinable
    static var complete: State {
      State(value: 0x0)
    }
    
    @inline(__always)
    @inlinable
    static var nonTransitiveComplete: State {
      State(value: 0x1)
    }
    
    @inline(__always)
    @inlinable
    static var layoutComplete: State {
      State(value: 0x3F)
    }
    
    @inline(__always)
    @inlinable
    static var abstract: State {
      State(value: 0xFF)
    }
  }
}

extension ExistentialMetadata {
  internal struct Flags {
    let value: UInt32
    
    @inline(__always)
    static var numberOfWitnessTablesMask: UInt32 {
      0xFFFFFF
    }
    
    @inline(__always)
    static var specialProtocolMask: UInt32 {
      0x3F000000
    }
    
    @inline(__always)
    static var specialProtocolShift: UInt32 {
      24
    }
    
    @inline(__always)
    static var hasSuperclassConstraintMask: UInt32 {
      0x40000000
    }
    
    // Note: This is set if the existential is NOT class constrained.
    @inline(__always)
    static var hasClassConstraintMask: UInt32 {
      0x80000000
    }
    
    var numberOfWitnessTables: Int {
      Int(truncatingIfNeeded: value & Flags.numberOfWitnessTablesMask)
    }
    
    /// The kind of special protocol this is.
    var specialProtocol: UInt8 {
      let masked = value & Flags.specialProtocolMask
      let shifted = masked >> Flags.specialProtocolShift
      
      return UInt8(truncatingIfNeeded: shifted)
    }
    
    /// Whether this existential has a superclass constraint.
    var hasSuperclassConstraint: Bool {
      value & Flags.hasSuperclassConstraintMask != 0
    }
    
    /// Whether this existential is class constrained. E.g. AnyObject constraint.
    var isClassConstraint: Bool {
      // Note this is inverted on purpose
      value & Flags.hasClassConstraintMask == 0
    }
  }
}

extension FunctionMetadata {
  @frozen
  public struct Convention {
    let value: Int
    
    init(_ value: Int) {
      self.value = value
    }
    
    public static var swift: Convention {
      Convention(0x0)
    }
    
    public static var block: Convention {
      Convention(0x1)
    }
    
    public static var thin: Convention {
      Convention(0x2)
    }
    
    public static var c: Convention {
      Convention(0x3)
    }
  }
}

extension FunctionMetadata.Convention: Equatable {
  public static func ==(
    lhs: FunctionMetadata.Convention,
    rhs: FunctionMetadata.Convention
  ) -> Bool {
    lhs.value == rhs.value
  }
}

extension FunctionMetadata {
  @frozen
  public struct DifferentiableKind {
    let value: Int
    
    init(_ value: Int) {
      self.value = value
    }
    
    public static var nonDifferentiable: DifferentiableKind {
      DifferentiableKind(0)
    }
    
    public static var forward: DifferentiableKind {
      DifferentiableKind(1)
    }
    
    public static var reverse: DifferentiableKind {
      DifferentiableKind(2)
    }
    
    public static var normal: DifferentiableKind {
      DifferentiableKind(3)
    }
    
    public static var linear: DifferentiableKind {
      DifferentiableKind(4)
    }
  }
}

extension FunctionMetadata.DifferentiableKind: Equatable {
  public static func ==(
    lhs: FunctionMetadata.DifferentiableKind,
    rhs: FunctionMetadata.DifferentiableKind
  ) -> Bool {
    lhs.value == rhs.value
  }
}

extension FunctionMetadata {
  struct Flags {
    let value: Int
    
    @inline(__always)
    static var numberOfParametersMask: Int {
      0xFFFF
    }
    
    @inline(__always)
    static var conventionMask: Int {
      0xFF0000
    }
    
    @inline(__always)
    static var conventionShift: Int {
      16
    }
    
    @inline(__always)
    static var throwsMask: Int {
      0x1000000
    }
    
    @inline(__always)
    static var hasParameterFlagsMask: Int {
      0x2000000
    }
    
    @inline(__always)
    static var isEscapingMask: Int {
      0x4000000
    }
    
    @inline(__always)
    static var isDifferentialMask: Int {
      0x8000000
    }
    
    @inline(__always)
    static var hasGlobalActorMask: Int {
      0x10000000
    }
    
    @inline(__always)
    static var isAsyncMask: Int {
      0x20000000
    }
    
    @inline(__always)
    static var isSendableMask: Int {
      0x40000000
    }
    
    var numberOfParameters: Int {
      value & Flags.numberOfParametersMask
    }
    
    var convention: Convention {
      Convention(
        (value & Flags.conventionMask) &>> Flags.conventionShift
      )
    }
    
    var `throws`: Bool {
      value & Flags.throwsMask != 0
    }
    
    var hasParameterFlags: Bool {
      value & Flags.hasParameterFlagsMask != 0
    }
    
    var isEscaping: Bool {
      value & Flags.isEscapingMask != 0
    }
    
    var isDifferential: Bool {
      value & Flags.isDifferentialMask != 0
    }
    
    var hasGlobalActor: Bool {
      value & Flags.hasGlobalActorMask != 0
    }
    
    var isAsync: Bool {
      value & Flags.isAsyncMask != 0
    }
    
    var isSendable: Bool {
      value & Flags.isSendableMask != 0
    }
  }
}

extension FunctionMetadata {
  struct ParameterFlags {
    let value: UInt32
    
    @inline(__always)
    static var valueOwnershipMask: UInt32 {
      0x7F
    }
    
    @inline(__always)
    static var isVariadicMask: UInt32 {
      0x80
    }
    
    @inline(__always)
    static var isAutoclosureMask: UInt32 {
      0x100
    }
    
    var valueOwnership: ValueOwnership {
      let value = UInt8(
        truncatingIfNeeded: value & ParameterFlags.valueOwnershipMask
      )
      
      return ValueOwnership(rawValue: value).unsafelyUnwrapped
    }
    
    var isVariadic: Bool {
      value & ParameterFlags.isVariadicMask != 0
    }
    
    var isAutoclosure: Bool {
      value & ParameterFlags.isAutoclosureMask != 0
    }
  }
}

enum ValueOwnership: UInt8 {
  case `default` = 0
  case `inout` = 1
  case shared = 2
  case owned = 3
}

extension ValueWitnessTable {
  @frozen
  public struct Flags {
    @usableFromInline
    let value: UInt32
    
    @inline(__always)
    @inlinable
    static var alignmentMaskMask: UInt32 {
      0xFF
    }
    
    @inline(__always)
    @inlinable
    static var isNonPODMask: UInt32 {
      0x10000
    }
    
    @inline(__always)
    @inlinable
    static var isNonInlineMask: UInt32 {
      0x20000
    }
    
    @inline(__always)
    @inlinable
    static var hasSpareBitsMask: UInt32 {
      0x80000
    }
    
    @inline(__always)
    @inlinable
    static var isNonBitwiseTakableMask: UInt32 {
      0x100000
    }
    
    @inline(__always)
    @inlinable
    static var hasEnumWitnessesMask: UInt32 {
      0x200000
    }
    
    @inline(__always)
    @inlinable
    static var isIncompleteMask: UInt32 {
      0x400000
    }
    
    @inlinable
    public var alignmentMask: Int {
      Int(truncatingIfNeeded: value & Flags.alignmentMaskMask)
    }
    
    @inlinable
    public var alignment: Int {
      alignmentMask + 1
    }
    
    @inlinable
    public var isValueInline: Bool {
      value & Flags.isNonInlineMask == 0
    }
    
    @inlinable
    public var isPOD: Bool {
      value & Flags.isNonPODMask == 0
    }
    
    public var hasSpareBits: Bool {
      value & Flags.hasSpareBitsMask != 0
    }
    
    @inlinable
    public var isBitwiseTakable: Bool {
      value & Flags.isNonBitwiseTakableMask == 0
    }
    
    @inlinable
    public var hasEnumWitnesses: Bool {
      value & Flags.hasEnumWitnessesMask != 0
    }
    
    @inlinable
    public var isIncomplete: Bool {
      value & Flags.isIncompleteMask != 0
    }
  }
}
