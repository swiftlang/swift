//===--- Type.swift -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import ASTBridging

/// A Swift type.
/// It is not necessarily canonical, e.g. typealiases are not resolved.
public struct Type: TypeProperties, CustomStringConvertible, NoReflectionChildren {
  public enum TraitResult {
    case isNot
    case canBe
    case `is`
  }

  public enum MetatypeRepresentation {
    case thin
    case thick
    case objC
  };

  public let bridged: BridgedASTType

  // Needed to conform to TypeProperties
  public var rawType: Type { self }

  public init?(bridgedOrNil: BridgedASTType) {
    if bridgedOrNil.type == nil {
      return nil
    }
    self.bridged = bridgedOrNil
  }

  public init(bridged: BridgedASTType) {
    self.bridged = bridged
  }

  public var canonical: CanonicalType { CanonicalType(bridged: bridged.getCanonicalType()) }

  public var instanceTypeOfMetatype: Type { Type(bridged: bridged.getInstanceTypeOfMetatype()) }

  public var staticTypeOfDynamicSelf: Type { Type(bridged: bridged.getStaticTypeOfDynamicSelf()) }

  public var interfaceTypeOfArchetype: Type { Type(bridged: bridged.getInterfaceTypeOfArchetype()) }

  public var superClassType: Type? {
    precondition(isClass)
    let bridgedSuperClassTy = bridged.getSuperClassType()
    if bridgedSuperClassTy.type != nil {
      return Type(bridged: bridgedSuperClassTy)
    }
    return nil
  }

  public var builtinVectorElementType: Type { Type(bridged: bridged.getBuiltinVectorElementType()) }

  public var optionalObjectType: Type {
    assert(self.isOptional)
    return genericArgumentsOfBoundGenericType[0]
  }

  public var optionalType: Type {
    return Type(bridged: bridged.getOptionalType())
  }

  public func subst(with substitutionMap: SubstitutionMap) -> Type {
    return Type(bridged: bridged.subst(substitutionMap.bridged))
  }

  public func mapOutOfEnvironment() -> Type {
    return Type(bridged: bridged.mapOutOfEnvironment())
  }

  /// Returns a stronger canonicalization which folds away equivalent
  /// associated types, or type parameters that have been made concrete.
  public func getReducedType(of signature: GenericSignature) -> CanonicalType {
    CanonicalType(bridged: bridged.getReducedType(signature.bridged))
  }

  public var nameOfGenericTypeParameter: Identifier {
    bridged.GenericTypeParam_getName()
  }

  public var depthOfGenericTypeParameter: Int {
    bridged.GenericTypeParam_getDepth()
  }

  public var indexOfGenericTypeParameter: Int {
    bridged.GenericTypeParam_getIndex()
  }

  public var kindOfGenericTypeParameter: GenericTypeParameterKind {
    bridged.GenericTypeParam_getParamKind()
  }

  public var genericArgumentsOfBoundGenericType: TypeArray {
    TypeArray(bridged: bridged.BoundGenericType_getGenericArgs())
  }
}

/// A Type that is statically known to be canonical.
/// For example, typealiases are resolved.
public struct CanonicalType: TypeProperties, CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedCanType

  public init(bridged: BridgedCanType) { self.bridged = bridged }

  public var rawType: Type { Type(bridged: bridged.getRawType()) }

  public var instanceTypeOfMetatype: CanonicalType { rawType.instanceTypeOfMetatype.canonical }

  public var superClassType: CanonicalType? { rawType.superClassType?.canonical }

  public var builtinVectorElementType: CanonicalType { rawType.builtinVectorElementType.canonical }

  public func subst(with substitutionMap: SubstitutionMap) -> CanonicalType {
    return rawType.subst(with: substitutionMap).canonical
  }
}

/// Implements the common members of `AST.Type`, `AST.CanonicalType` and `SIL.Type`.
public protocol TypeProperties {
  var rawType: Type { get }
}

extension TypeProperties {
  public var description: String { String(taking: rawType.bridged.getDebugDescription()) }

  //===--------------------------------------------------------------------===//
  //                      Checks for different kinds of types
  //===--------------------------------------------------------------------===//

  public var isBuiltinInteger: Bool { rawType.bridged.isBuiltinInteger() }

  public func isBuiltinInteger(withFixedWidth width: Int) -> Bool {
    rawType.bridged.isBuiltinFixedWidthInteger(width)
  }

  public var isBuiltinFloat: Bool { rawType.bridged.isBuiltinFloat() }
  public var isBuiltinVector: Bool { rawType.bridged.isBuiltinVector() }
  public var isBuiltinFixedArray: Bool { rawType.bridged.isBuiltinFixedArray() }

  public var isClass: Bool {
    if let nominal = nominal, nominal is ClassDecl {
      return true
    }
    return false
  }

  public var isStruct: Bool {
    if let nominal = nominal, nominal is StructDecl {
      return true
    }
    return false
  }

  public var isEnum: Bool  {
    if let nominal = nominal, nominal is EnumDecl {
      return true
    }
    return false
  }

  public var isTuple: Bool { rawType.bridged.isTuple() }
  public var isFunction: Bool { rawType.bridged.isFunction() }
  public var isArchetype: Bool { rawType.bridged.isArchetype() }
  public var isExistentialArchetype: Bool { rawType.bridged.isExistentialArchetype() }
  public var isExistentialArchetypeWithError: Bool { rawType.bridged.isExistentialArchetypeWithError() }
  public var isRootArchetype: Bool { rawType.interfaceTypeOfArchetype.isGenericTypeParameter }
  public var isRootExistentialArchetype: Bool { isExistentialArchetype && isRootArchetype }
  public var isExistential: Bool { rawType.bridged.isExistential() }
  public var isClassExistential: Bool { rawType.bridged.isClassExistential() }
  public var isGenericTypeParameter: Bool { rawType.bridged.isGenericTypeParam() }
  public var isUnownedStorageType: Bool { return rawType.bridged.isUnownedStorageType() }
  public var isMetatype: Bool { rawType.bridged.isMetatypeType() }
  public var isExistentialMetatype: Bool { rawType.bridged.isExistentialMetatypeType() }
  public var isDynamicSelf: Bool { rawType.bridged.isDynamicSelf()}
  public var isBox: Bool { rawType.bridged.isBox() }
  public var isPack: Bool { rawType.bridged.isPack() }
  public var isSILPack: Bool { rawType.bridged.isSILPack() }

  public var canBeClass: Type.TraitResult { rawType.bridged.canBeClass().result }

  /// True if this type is a value type (struct/enum) that defines a `deinit`.
  public var isValueTypeWithDeinit: Bool {
    if let nominal = nominal, nominal.valueTypeDestructor != nil {
      return true
    }
    return false
  }

  //===--------------------------------------------------------------------===//
  //                      Checks for stdlib types
  //===--------------------------------------------------------------------===//

  /// True if this is the type which represents an integer literal used in a type position.
  /// For example `N` in `struct T<let N: Int> {}`
  public var isInteger: Bool { rawType.bridged.isInteger() }

  /// True if this the nominal type `Swift.Optional`.
  public var isOptional: Bool { rawType.bridged.isOptional() }

  /// A non-nil result type implies isUnsafe[Raw][Mutable]Pointer. A raw
  /// pointer has a `void` element type.
  public var unsafePointerElementType: Type? {
    Type(bridgedOrNil: rawType.bridged.getAnyPointerElementType())
  }

  public var isAnyUnsafePointer: Bool {
    unsafePointerElementType != nil
  }

  public var isAnyUnsafeBufferPointer: Bool {
    rawType.bridged.isUnsafeBufferPointerType()
      || rawType.bridged.isUnsafeMutableBufferPointerType()
      || rawType.bridged.isUnsafeRawBufferPointerType()
      || rawType.bridged.isUnsafeMutableRawBufferPointerType()
  }

  //===--------------------------------------------------------------------===//
  //                Properties of lowered `SILFunctionType`s
  //===--------------------------------------------------------------------===//

  public var isLoweredFunction: Bool { rawType.bridged.isLoweredFunction() }
  public var isNoEscapeFunction: Bool { rawType.bridged.isNoEscapeFunction() }
  public var isCalleeConsumedFunction: Bool { rawType.bridged.isCalleeConsumedFunction() }
  public var isThickFunction: Bool { rawType.bridged.isThickFunction() }
  public var isAsyncFunction: Bool { rawType.bridged.isAsyncFunction() }

  public var invocationGenericSignatureOfFunction: GenericSignature {
    GenericSignature(bridged: rawType.bridged.getInvocationGenericSignatureOfFunctionType())
  }

  public var functionTypeRepresentation: FunctionTypeRepresentation {
    switch rawType.bridged.getFunctionTypeRepresentation() {
      case .Thick:                 return .thick
      case .Block:                 return .block
      case .Thin:                  return .thin
      case .CFunctionPointer:      return .cFunctionPointer
      case .Method:                return .method
      case .ObjCMethod:            return .objCMethod
      case .WitnessMethod:         return .witnessMethod
      case .Closure:               return .closure
      case .CXXMethod:             return .cxxMethod
      case .KeyPathAccessorGetter: return .keyPathAccessorGetter
      case .KeyPathAccessorSetter: return .keyPathAccessorSetter
      case .KeyPathAccessorEquals: return .keyPathAccessorEquals
      case .KeyPathAccessorHash:   return .keyPathAccessorHash
      default: fatalError()
    }
  }

  //===--------------------------------------------------------------------===//
  //                             Type properties
  //===--------------------------------------------------------------------===//

  public var isLegalFormalType: Bool { rawType.bridged.isLegalFormalType() }
  public var hasArchetype: Bool { rawType.bridged.hasArchetype() }
  public var hasTypeParameter: Bool { rawType.bridged.hasTypeParameter() }
  public var hasLocalArchetype: Bool { rawType.bridged.hasLocalArchetype() }
  public var hasDynamicSelf: Bool { rawType.bridged.hasDynamicSelf() }
  public var isEscapable: Bool { rawType.bridged.isEscapable() }
  public var isNoEscape: Bool { rawType.bridged.isNoEscape() }
  public var isBuiltinType: Bool { rawType.bridged.isBuiltinType() }
  public var archetypeRequiresClass: Bool { rawType.bridged.archetypeRequiresClass() }

  public var representationOfMetatype: AST.`Type`.MetatypeRepresentation {
    rawType.bridged.getRepresentationOfMetatype().representation
  }

  public var builtinFixedArrayElementType: CanonicalType {
    CanonicalType(bridged: rawType.bridged.getBuiltinFixedArrayElementType())
  }
  public var builtinFixedArraySizeType: CanonicalType {
    CanonicalType(bridged: rawType.bridged.getBuiltinFixedArraySizeType())
  }

  /// Returns the value of an integer value type (see `isInteger`).
  /// Returns nil if the value is not representable in an `Int`.
  public var valueOfInteger: Int? {
    let optionalInt = rawType.bridged.getValueOfIntegerType()
    if optionalInt.hasValue {
      return optionalInt.value
    }
    return nil
  }

  /// Assumes this is a nominal type. Returns a substitution map that sends each
  /// generic parameter of the declaration's generic signature to the corresponding
  /// generic argument of this nominal type.
  ///
  /// Eg: Array<Int> ---> { Element := Int }
  public var contextSubstitutionMap: SubstitutionMap {
    SubstitutionMap(bridged: rawType.bridged.getContextSubstitutionMap())
  }

  // True if this type has generic parameters or it is in a context (e.g. an outer type) which has generic parameters.
  public var isGenericAtAnyLevel: Bool { rawType.bridged.isGenericAtAnyLevel() }

  public var nominal: NominalTypeDecl? {
    rawType.bridged.getNominalOrBoundGenericNominal().getAs(NominalTypeDecl.self)
  }

  /// Performs a global conformance lookup for this type for `protocol`.
  /// It checks conditional requirements.
  ///
  /// This type must be a contextualized type. It must not contain type parameters.
  ///
  /// The resulting conformance reference does not include "missing" conformances, which are synthesized for
  /// some protocols as an error recovery mechanism.
  ///
  /// Returns an invalid conformance if the search failed, otherwise an
  /// abstract, concrete or pack conformance, depending on the lookup type.
  public func checkConformance(to protocol: ProtocolDecl) -> Conformance {
    return Conformance(bridged: rawType.bridged.checkConformance(`protocol`.bridged))
  }

  /// The generic signature that the component types are specified in terms of, if any.
  public var substitutedGenericSignatureOfFunctionType: CanonicalGenericSignature {
    CanonicalGenericSignature(
      bridged: rawType.canonical.bridged.SILFunctionType_getSubstGenericSignature())
  }

  public var containsSILPackExpansionType: Bool {
    return rawType.bridged.containsSILPackExpansionType()
  }

  public var isSILPackElementAddress: Bool {
    return rawType.bridged.isSILPackElementAddress()
  }
}

public enum FunctionTypeRepresentation {
  /// A freestanding thick function.
  case thick

  /// A thick function that is represented as an Objective-C block.
  case block

  /// A freestanding thin function that needs no context.
  case thin

  /// A C function pointer, which is thin and also uses the C calling convention.
  case cFunctionPointer

  /// A Swift instance method.
  case method

  /// An Objective-C method.
  case objCMethod

  /// A Swift protocol witness.
  case witnessMethod

  /// A closure invocation function that has not been bound to a context.
  case closure

  /// A C++ method that takes a "this" argument (not a static C++ method or constructor).
  /// Except for handling the "this" argument, has the same behavior as "CFunctionPointer".
  case cxxMethod

  /// A KeyPath accessor function, which is thin and also uses the variadic length generic
  /// components serialization in trailing buffer. Each representation has a different convention
  /// for which parameters have serialized generic type info.
  case keyPathAccessorGetter, keyPathAccessorSetter, keyPathAccessorEquals, keyPathAccessorHash

  public var bridged: BridgedASTType.FunctionTypeRepresentation {
    switch self {
      case .thick:                 return .Thick
      case .block:                 return .Block
      case .thin:                  return .Thin
      case .cFunctionPointer:      return .CFunctionPointer
      case .method:                return .Method
      case .objCMethod:            return .ObjCMethod
      case .witnessMethod:         return .WitnessMethod
      case .closure:               return .Closure
      case .cxxMethod:             return .CXXMethod
      case .keyPathAccessorGetter: return .KeyPathAccessorGetter
      case .keyPathAccessorSetter: return .KeyPathAccessorSetter
      case .keyPathAccessorEquals: return .KeyPathAccessorEquals
      case .keyPathAccessorHash:   return .KeyPathAccessorHash
    }
  }
}

public struct TypeArray : RandomAccessCollection, CustomReflectable {
  public let bridged: BridgedASTTypeArray

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return bridged.getCount() }

  public init(bridged: BridgedASTTypeArray) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> Type {
    Type(bridged: bridged.getAt(index))
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}

extension BridgedASTType.TraitResult {
  var result: Type.TraitResult {
    switch self {
    case .IsNot: return .isNot
    case .CanBe: return .canBe
    case .Is:    return .is
    default:
      fatalError("wrong type TraitResult enum case")
    }
  }
}

extension BridgedASTType.MetatypeRepresentation {
  var representation: Type.MetatypeRepresentation {
    switch self {
    case .Thin:  return .thin
    case .Thick: return .thick
    case .ObjC:  return .objC
    default:
      fatalError("wrong type MetatypeRepresentation enum case")
    }
  }
}

extension Type: Equatable {
  public static func ==(lhs: Type, rhs: Type) -> Bool { 
    lhs.bridged.type == rhs.bridged.type
  }
}

extension CanonicalType: Equatable {
  public static func ==(lhs: CanonicalType, rhs: CanonicalType) -> Bool { 
    lhs.rawType == rhs.rawType
  }
}

public typealias GenericTypeParameterKind = swift.GenericTypeParamKind
