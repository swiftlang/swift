//===--- Type.swift - Value type ------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Basic
import AST
import SILBridging

/// A Swift type that has been lowered to a SIL representation type.
/// A `SIL.Type` is basically an `AST.CanonicalType` with the distinction between "object" and "address" type
/// (`*T` is the type of an address pointing at T).
/// Note that not all `CanonicalType`s can be represented as a `SIL.Type`.
public struct Type : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedType

  public var isAddress: Bool { bridged.isAddress() }
  public var isObject: Bool { !isAddress }

  public var addressType: Type { bridged.getAddressType().type }
  public var objectType: Type { bridged.getObjectType().type }

  public var canonicalASTType: CanonicalType { CanonicalType(bridged: bridged.getCanType()) }

  public func isTrivial(in function: Function) -> Bool {
    return bridged.isTrivial(function.bridged)
  }

  /// Returns true if the type is a trivial type and is and does not contain a Builtin.RawPointer.
  public func isTrivialNonPointer(in function: Function) -> Bool {
    return !bridged.isNonTrivialOrContainsRawPointer(function.bridged)
  }

  /// True if this type is a value type (struct/enum) that requires deinitialization beyond
  /// destruction of its members.
  public var isValueTypeWithDeinit: Bool { bridged.isValueTypeWithDeinit() }

  public func isLoadable(in function: Function) -> Bool {
    return bridged.isLoadable(function.bridged)
  }

  public func isReferenceCounted(in function: Function) -> Bool {
    return bridged.isReferenceCounted(function.bridged)
  }

  public var isUnownedStorageType: Bool {
    return bridged.isUnownedStorageType()
  }

  public var hasArchetype: Bool { bridged.hasArchetype() }

  public var isClass: Bool { bridged.isClassOrBoundGenericClass() }
  public var isStruct: Bool { bridged.isStructOrBoundGenericStruct() }
  public var isTuple: Bool { bridged.isTuple() }
  public var isEnum: Bool { bridged.isEnumOrBoundGenericEnum() }
  public var isFunction: Bool { bridged.isFunction() }
  public var isMetatype: Bool { bridged.isMetatype() }
  public var isClassExistential: Bool { bridged.isClassExistential() }
  public var isNoEscapeFunction: Bool { bridged.isNoEscapeFunction() }
  public var containsNoEscapeFunction: Bool { bridged.containsNoEscapeFunction() }
  public var isThickFunction: Bool { bridged.isThickFunction() }
  public var isAsyncFunction: Bool { bridged.isAsyncFunction() }

  public var canBeClass: BridgedType.TraitResult { bridged.canBeClass() }

  public var isMoveOnly: Bool { bridged.isMoveOnly() }

  // Note that invalid types are not considered Escapable. This makes it difficult to make any assumptions about
  // nonescapable types.
  public func isEscapable(in function: Function) -> Bool {
    bridged.isEscapable(function.bridged)
  }
  public func mayEscape(in function: Function) -> Bool {
    !isNoEscapeFunction && isEscapable(in: function)
  }

  /// Can only be used if the type is in fact a nominal type (`isNominal` is true).
  public var nominal: NominalTypeDecl? {
    bridged.getNominalOrBoundGenericNominal().getAs(NominalTypeDecl.self)
  }

  public var superClassType: Type? {
    precondition(isClass)
    return bridged.getSuperClassType().typeOrNil
  }

  public var contextSubstitutionMap: SubstitutionMap {
    SubstitutionMap(bridged: bridged.getContextSubstitutionMap())
  }

  public var isGenericAtAnyLevel: Bool { bridged.isGenericAtAnyLevel() }

  public var isOrContainsObjectiveCClass: Bool { bridged.isOrContainsObjectiveCClass() }

  public var isBuiltinInteger: Bool { bridged.isBuiltinInteger() }
  public var isBuiltinFloat: Bool { bridged.isBuiltinFloat() }
  public var isBuiltinVector: Bool { bridged.isBuiltinVector() }
  public var builtinVectorElementType: Type { bridged.getBuiltinVectorElementType().type }

  public func isBuiltinInteger(withFixedWidth width: Int) -> Bool {
    bridged.isBuiltinFixedWidthInteger(width)
  }

  public func isExactSuperclass(of type: Type) -> Bool {
    bridged.isExactSuperclassOf(type.bridged)
  }

  public var isVoid: Bool {
    bridged.isVoid()
  }

  public func isEmpty(in function: Function) -> Bool {
    bridged.isEmpty(function.bridged)
  }

  public var tupleElements: TupleElementArray { TupleElementArray(type: self) }

  public func getLoweredType(in function: Function) -> Type {
    function.bridged.getLoweredType(self.bridged).type
  }

  /// Can only be used if the type is in fact a nominal type (`isNominal` is true).
  /// Returns nil if the nominal is a resilient type because in this case the complete list
  /// of fields is not known.
  public func getNominalFields(in function: Function) -> NominalFieldsArray? {
    if nominal!.isResilient(in: function) {
      return nil
    }
    return NominalFieldsArray(type: self, function: function)
  }

  /// Can only be used if the type is in fact an enum type.
  /// Returns nil if the enum is a resilient type because in this case the complete list
  /// of cases is not known.
  public func getEnumCases(in function: Function) -> EnumCases? {
    if nominal!.isResilient(in: function) {
      return nil
    }
    return EnumCases(enumType: self, function: function)
  }

  public typealias MetatypeRepresentation = BridgedType.MetatypeRepresentation

  public func loweredInstanceTypeOfMetatype(in function: Function) -> Type {
    bridged.getLoweredInstanceTypeOfMetatype(function.bridged).type
  }

  public var isDynamicSelfMetatype: Bool {
    bridged.isDynamicSelfMetatype()
  }

  public func representationOfMetatype(in function: Function) -> MetatypeRepresentation {
    bridged.getRepresentationOfMetatype(function.bridged)
  }

  public var isCalleeConsumedFunction: Bool { bridged.isCalleeConsumedFunction() }

  public var isMarkedAsImmortal: Bool { bridged.isMarkedAsImmortal() }

  public func getIndexOfEnumCase(withName name: String) -> Int? {
    let idx = name._withBridgedStringRef {
      bridged.getCaseIdxOfEnumType($0)
    }
    return idx >= 0 ? idx : nil
  }

  /// Returns true if this is a struct, enum or tuple and `otherType` is contained in this type - or is the same type.
  public func aggregateIsOrContains(_ otherType: Type, in function: Function) -> Bool {
    if self == otherType {
      return true
    }
    if isStruct {
      guard let fields = getNominalFields(in: function) else {
        return true
      }
      return fields.contains { $0.aggregateIsOrContains(otherType, in: function) }
    }
    if isTuple {
      return tupleElements.contains { $0.aggregateIsOrContains(otherType, in: function) }
    }
    if isEnum {
      guard let cases = getEnumCases(in: function) else {
        return true
      }
      return cases.contains { $0.payload?.aggregateIsOrContains(otherType, in: function) ?? false }
    }
    return false
  }

// compiling bridged.getFunctionTypeWithNoEscape crashes the 5.10 Windows compiler
#if !os(Windows)
  // TODO: https://github.com/apple/swift/issues/73253
  public func getFunctionType(withNoEscape: Bool) -> Type {
    bridged.getFunctionTypeWithNoEscape(withNoEscape).type
  }
#endif

  public var description: String {
    String(taking: bridged.getDebugDescription())
  }
}

extension Value {
  public var isEscapable: Bool {
    type.objectType.isEscapable(in: parentFunction)
  }

  public var mayEscape: Bool {
    type.objectType.mayEscape(in: parentFunction)
  }
}

extension Type: Equatable {
  public static func ==(lhs: Type, rhs: Type) -> Bool { 
    lhs.bridged.opaqueValue == rhs.bridged.opaqueValue
  }
}

public struct TypeArray : RandomAccessCollection, CustomReflectable {
  public let bridged: BridgedSILTypeArray

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return bridged.getCount() }

  public init(bridged: BridgedSILTypeArray) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> Type {
    bridged.getAt(index).type
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}

public struct OptionalTypeArray : RandomAccessCollection, CustomReflectable {
  private let bridged: BridgedTypeArray

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return bridged.getCount() }

  public init(bridged: BridgedTypeArray) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> Type? {
    bridged.getAt(index).typeOrNil
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0 ?? "<invalid>") }
    return Mirror(self, children: c)
  }
}

public struct NominalFieldsArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let type: Type
  fileprivate let function: Function

  public var startIndex: Int { return 0 }
  public var endIndex: Int { Int(type.bridged.getNumNominalFields()) }

  public subscript(_ index: Int) -> Type {
    type.bridged.getFieldType(index, function.bridged).type
  }

  public func getIndexOfField(withName name: String) -> Int? {
    let idx = name._withBridgedStringRef {
      type.bridged.getFieldIdxOfNominalType($0)
    }
    return idx >= 0 ? idx : nil
  }

  public func getNameOfField(withIndex idx: Int) -> StringRef {
    StringRef(bridged: type.bridged.getFieldName(idx))
  }
}

public struct EnumCase {
  public let payload: Type?
  public let index: Int
}

public struct EnumCases : CollectionLikeSequence, IteratorProtocol {
  fileprivate let enumType: Type
  fileprivate let function: Function
  private var caseIterator: BridgedType.EnumElementIterator
  private var caseIndex = 0

  fileprivate init(enumType: Type, function: Function) {
    self.enumType = enumType
    self.function = function
    self.caseIterator = enumType.bridged.getFirstEnumCaseIterator()
  }

  public mutating func next() -> EnumCase? {
    if !enumType.bridged.isEndCaseIterator(caseIterator) {
      defer {
        caseIterator = caseIterator.getNext()
        caseIndex += 1
      }
      return EnumCase(payload: enumType.bridged.getEnumCasePayload(caseIterator, function.bridged).typeOrNil,
                      index: caseIndex)
    }
    return nil
  }
}

public struct TupleElementArray : RandomAccessCollection, FormattedLikeArray {
  fileprivate let type: Type

  public var startIndex: Int { return 0 }
  public var endIndex: Int { Int(type.bridged.getNumTupleElements()) }

  public subscript(_ index: Int) -> Type {
    type.bridged.getTupleElementType(index).type
  }
}

extension BridgedType {
  public var type: Type { Type(bridged: self) }
  var typeOrNil: Type? { isNull() ? nil : type }
}
