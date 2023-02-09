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

@available(SwiftStdlib 5.9, *)
@frozen
public struct GenericSignature {
  public let header: Header
  public let parameters: BufferView<ParameterDescriptor>
  public let requirements: IndirectBufferView<RequirementDescriptor>

  @inlinable
  init(
    _ header: Header,
    _ parameters: BufferView<ParameterDescriptor>,
    _ requirements: IndirectBufferView<RequirementDescriptor>
  ) {
    self.header = header
    self.parameters = parameters
    self.requirements = requirements
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericSignature {
  @frozen
  public struct Header {
    @usableFromInline
    typealias Storage = (
      numberOfParameters: UInt16,
      numberOfRequirements: UInt16,
      numberOfKeyArguments: UInt16,
      numberOfExtraArguments: UInt16
    )

    @usableFromInline
    let storage: Storage

    @inlinable
    public var numberOfParameters: Int {
      Int(truncatingIfNeeded: storage.numberOfParameters)
    }

    public var numberOfRequirements: Int {
      Int(truncatingIfNeeded: storage.numberOfRequirements)
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericSignature {
  @frozen
  public struct RequirementDescriptor: PublicLayout {
    public typealias Layout = (
      flags: Flags,
      parameter: RelativeDirectPointer<CChar>,
      // This field is a union which represents the type of requirement
      // that this parameter is constrained to. It is represented by the following:
      // 1. Same type requirement (RelativeDirectPointer<CChar>)
      // 2. Base class requirement (RelativeDirectPointer<CChar>)
      // 3. Protocol requirement (RelativeIndirectablePointerIntPair<ProtocolDescriptor, Bool>)
      // 4. Conformance requirement (RelativeIndirectablePointer<ProtocolConformanceRecord>)
      // 5. Layout requirement (LayoutKind)
      requirement: Int32
    )

    public let ptr: UnsafeRawPointer

    @inlinable
    public init(_ ptr: UnsafeRawPointer) {
      self.ptr = ptr
    }
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericSignature.RequirementDescriptor {
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var flags: Flags {
    layout.flags
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var parameter: MangledTypeReference {
    MangledTypeReference(address(for: \.parameter))
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var `protocol`: ProtocolDescriptor {
    let addr = address(for: \.requirement)
      .relativeIndirectablePointerIntPairAddress(
        as: ProtocolDescriptor.self,
        and: UInt8.self
      )

    return ProtocolDescriptor(addr)
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var sameType: MangledTypeReference {
    MangledTypeReference(
      address(for: \.requirement).relativeDirectAddress(as: CChar.self)
    )
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var layoutKind: GenericSignature.LayoutKind {
    address(for: \.requirement).unprotectedLoad(
      as: GenericSignature.LayoutKind.self
    )
  }

  @available(SwiftStdlib 5.9, *)
  @inlinable
  public var baseClass: MangledTypeReference {
    sameType
  }
}

@available(SwiftStdlib 5.9, *)
extension GenericSignature.RequirementDescriptor {
  @available(SwiftStdlib 5.9, *)
  internal func parameterType(
    in context: ContextDescriptor,
    with argPtr: UnsafeRawPointer
  ) -> Any.Type? {
    _getTypeByMangledNameInContext(
      UnsafePointer(parameter.ptr._rawValue),
      UInt(truncatingIfNeeded: parameter.length),
      genericContext: context.ptr,
      genericArguments: argPtr
    )
  }

  @available(SwiftStdlib 5.9, *)
  public func checkProtocolConformance(
    in context: ContextDescriptor,
    with argPtr: UnsafeRawPointer
  ) -> WitnessTable? {
    guard flags.kind == .protocol else {
      return nil
    }

    guard let parameterTy = parameterType(in: context, with: argPtr) else {
      return nil
    }

    return swift_conformsToProtocol(Metadata(parameterTy), `protocol`)
  }

  // This function handles 2 cases:
  //
  //   1. Associated type same type constraints. E.g. 'T.Element == X'
  //   2. Same type constraints to other generic parameters. E.g. 'T == U'
  //
  // In the first case, we will have both a parameter type and same type type
  // be resolved. Simply check that they are the same and move on. For the
  // second case, either the parameter type or the same type type will not be
  // resolved because this type of constraint makes one of the parameters
  // non-key. For example, 'T == U' this constraint causes 'U' to be a non-key
  // argument, but 'T' still is however. In the same vein, 'U == [T]' causes
  // 'U' to be our key argument now.
  //
  // Same type constraints that look like 'T == String' cause 'T' to be a purely
  // syntactical generic parameter, thus it is resolved ealier in the stack and
  // not provided to our argument pointer.
  @available(SwiftStdlib 5.9, *)
  public func checkSameType(
    in context: ContextDescriptor,
    with argPtr: UnsafeRawPointer
  ) -> Bool {
    guard flags.kind == .sameType else {
      return false
    }

    guard let parameterTy = parameterType(in: context, with: argPtr) else {
      // Because of the 2nd case, there might not be a resolved parameter type
      // or same type type, so we have to return true to indicate that there is
      // no constraint to solve.
      return true
    }

    let sameTypeTy = _getTypeByMangledNameInContext(
      UnsafePointer(sameType.ptr._rawValue),
      UInt(sameType.length),
      genericContext: context.ptr,
      genericArguments: argPtr
    )

    guard let sameTypeTy = sameTypeTy else {
      // Because of the 2nd case, there might not be a resolved parameter type
      // or same type type, so we have to return true to indicate that there is
      // no constraint to solve.
      return true
    }

    return parameterTy == sameTypeTy
  }

  @available(SwiftStdlib 5.9, *)
  public func checkLayout(
    in context: ContextDescriptor,
    with argPtr: UnsafeRawPointer
  ) -> Bool {
    guard flags.kind == .layout else {
      return false
    }

    guard let parameterTy = parameterType(in: context, with: argPtr) else {
      return false
    }

    switch layoutKind {
    case .class:
      return Metadata(parameterTy).isAnyClass

    // There is currently only class layouts supported, but in case we somehow
    // find something else return false.
    default:
      return false
    }
  }

  @available(SwiftStdlib 5.9, *)
  public func checkBaseClass(
    in context: ContextDescriptor,
    with argPtr: UnsafeRawPointer
  ) -> Bool {
    guard flags.kind == .baseClass else {
      return false
    }

    guard let parameterTy = parameterType(in: context, with: argPtr),
          Metadata(parameterTy).isAnyClass else {
      return false
    }

    let baseClassTy = _getTypeByMangledNameInContext(
      UnsafePointer(baseClass.ptr._rawValue),
      UInt(baseClass.length),
      genericContext: context.ptr,
      genericArguments: argPtr
    )

    guard let baseClassTy = baseClassTy else {
      return false
    }

    return _isSubclass(Metadata(parameterTy), Metadata(baseClassTy))
  }
}

@available(SwiftStdlib 5.9, *)
@inlinable
func getGenericSignature(at address: UnsafeRawPointer) -> GenericSignature {
  var address = address

  let header = address.unprotectedLoad(as: GenericSignature.Header.self)
  address += MemoryLayout<GenericSignature.Header>.size

  let parameters = BufferView<GenericSignature.ParameterDescriptor>(
    start: address,
    count: header.numberOfParameters
  )
  // This accounts for padding
  address += (-header.numberOfParameters & 0x3) + header.numberOfParameters

  let requirements = IndirectBufferView<GenericSignature.RequirementDescriptor>(
    start: address,
    count: header.numberOfRequirements
  )

  return GenericSignature(header, parameters, requirements)
}
