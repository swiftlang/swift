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
public struct Type : TypeProperties, CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedType

  public var description: String {
    String(taking: bridged.getDebugDescription())
  }

  public var isAddress: Bool { bridged.isAddress() }
  public var isObject: Bool { !isAddress }

  public var addressType: Type { bridged.getAddressType().type }
  public var objectType: Type { bridged.getObjectType().type }

  public var rawType: AST.`Type` { canonicalType.rawType }
  public var canonicalType: CanonicalType { CanonicalType(bridged: bridged.getCanType()) }

  public func getLoweredType(in function: Function) -> Type {
    function.bridged.getLoweredType(self.bridged).type
  }

  //===--------------------------------------------------------------------===//
  //                           Various type properties
  //===--------------------------------------------------------------------===//

  public func isTrivial(in function: Function) -> Bool {
    return bridged.isTrivial(function.bridged)
  }

  /// Returns true if the type is a trivial type and is and does not contain a Builtin.RawPointer.
  public func isTrivialNonPointer(in function: Function) -> Bool {
    return !bridged.isNonTrivialOrContainsRawPointer(function.bridged)
  }

  public func isLoadable(in function: Function) -> Bool {
    return bridged.isLoadable(function.bridged)
  }

  public func isReferenceCounted(in function: Function) -> Bool {
    return bridged.isReferenceCounted(function.bridged)
  }

  public var isMoveOnly: Bool { bridged.isMoveOnly() }

  /// Return true if this type conforms to Escapable.
  ///
  /// Note: invalid types are non-Escapable, so take care not to make assumptions about non-Escapable types.
  public func isEscapable(in function: Function) -> Bool {
    bridged.isEscapable(function.bridged)
  }

  /// Return true if this type conforms to Escapable and is not a noescape function type.
  ///
  /// Warning: may return true for (source-level) non-escaping closures. After SILGen, all partial_apply instructions
  /// have an escaping function type. ClosureLifetimeFixup only changes them to noescape function type if they are
  /// promoted to [on_stack]. But regardless of stack promotion, a non-escaping closure value's lifetime is constrained
  /// by the lifetime of its captures. Use Value.mayEscape instead to check for this case.
  public func mayEscape(in function: Function) -> Bool {
    !isNoEscapeFunction && isEscapable(in: function)
  }

  public func builtinVectorElementType(in function: Function) -> Type {
    canonicalType.builtinVectorElementType.loweredType(in: function)
  }

  public func builtinFixedArrayElementType(in function: Function, maximallyAbstracted: Bool = false) -> Type {
    canonicalType.builtinFixedArrayElementType.loweredType(in: function, maximallyAbstracted: maximallyAbstracted)
  }

  public var superClassType: Type? { canonicalType.superClassType?.silType }

  public func isExactSuperclass(of type: Type) -> Bool {
    bridged.isExactSuperclassOf(type.bridged)
  }

  public func loweredInstanceTypeOfMetatype(in function: Function) -> Type {
    return canonicalType.instanceTypeOfMetatype.loweredType(in: function)
  }

  public var isMarkedAsImmortal: Bool { bridged.isMarkedAsImmortal() }

  /// True if a value of this type can have its address taken by a lifetime-dependent value.
  public func isAddressableForDeps(in function: Function) -> Bool {
    bridged.isAddressableForDeps(function.bridged)
  }

  //===--------------------------------------------------------------------===//
  //                Properties of lowered `SILFunctionType`s
  //===--------------------------------------------------------------------===//

  public var containsNoEscapeFunction: Bool { bridged.containsNoEscapeFunction() }

  // Returns a new SILFunctionType with changed "escapeness".
  public func getFunctionType(withNoEscape: Bool) -> Type {
    bridged.getFunctionTypeWithNoEscape(withNoEscape).type
  }

  /// True if a function with this type can be code-generated in Embedded Swift.
  /// These are basically all non-generic functions. But also certain generic functions are supported:
  /// Generic function arguments which have a class-bound type are valid in Embedded Swift, because for
  /// such arguments, no metadata is needed, except the isa-pointer of the class.
  public var hasValidSignatureForEmbedded: Bool {
    let genericSignature = invocationGenericSignatureOfFunction
    for genParam in genericSignature.genericParameters {
      let mappedParam = genericSignature.mapTypeIntoContext(genParam)
      if mappedParam.isArchetype && !mappedParam.archetypeRequiresClass {
        return false
      }
    }
    return true
  }

  //===--------------------------------------------------------------------===//
  //                           Aggregates
  //===--------------------------------------------------------------------===//

  public var isVoid: Bool { isTuple && tupleElements.isEmpty }

  /// True if the type has no stored properties.
  /// For example an empty tuple or an empty struct or a combination of such.
  public func isEmpty(in function: Function) -> Bool {
    bridged.isEmpty(function.bridged)
  }

  public var tupleElements: TupleElementArray {
    precondition(isTuple)
    return TupleElementArray(type: self)
  }

  public func getBoxFields(in function: Function) -> BoxFieldsArray {
    precondition(isBox)
    return BoxFieldsArray(boxType: canonicalType, function: function)
  }

  /// Returns nil if the nominal is a resilient type because in this case the complete list
  /// of fields is not known.
  public func getNominalFields(in function: Function) -> NominalFieldsArray? {
    guard let nominal = nominal, !nominal.isResilient(in: function) else {
      return nil
    }
    if let structDecl = nominal as? StructDecl, structDecl.hasUnreferenceableStorage {
      return nil
    }
    return NominalFieldsArray(type: self, function: function)
  }

  /// Returns nil if the enum is a resilient type because in this case the complete list
  /// of cases is not known.
  public func getEnumCases(in function: Function) -> EnumCases? {
    guard let nominal = nominal,
          let en = nominal as? EnumDecl,
          !en.isResilient(in: function)
    else {
      return nil
    }
    return EnumCases(enumType: self, function: function)
  }

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

public struct BoxFieldsArray : RandomAccessCollection, FormattedLikeArray {
  public let boxType: CanonicalType
  public let function: Function

  public var startIndex: Int { return 0 }
  public var endIndex: Int { BridgedType.getNumBoxFields(boxType.bridged) }

  public subscript(_ index: Int) -> Type {
    BridgedType.getBoxFieldType(boxType.bridged, index, function.bridged).type
  }

  public func isMutable(fieldIndex: Int) -> Bool {
    BridgedType.getBoxFieldIsMutable(boxType.bridged, fieldIndex)
  }
}

extension Type: DiagnosticArgument {
  public func _withBridgedDiagnosticArgument(_ fn: (BridgedDiagnosticArgument) -> Void) {
    rawType._withBridgedDiagnosticArgument(fn)
  }
}

extension BridgedType {
  public var type: Type { Type(bridged: self) }
  var typeOrNil: Type? { isNull() ? nil : type }
}
