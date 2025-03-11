//===--- KeyPath.swift ----------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import AST
import SILBridging

//============================================================================//
// KeyPathPattern
//============================================================================//

public struct KeyPathPattern {
  public let bridged: BridgedKeyPathPattern

  public init(bridged: BridgedKeyPathPattern) {
    self.bridged = bridged
  }
}

extension KeyPathPattern {
  public var components: ComponentArray {
    ComponentArray(bridged: bridged.components())
  }

  public var objcString: StringRef {
    StringRef(bridged: bridged.getObjCString())
  }
}

//============================================================================//
// KeyPathPattern.Component
//============================================================================//

extension KeyPathPattern {
  public struct Component {
    public let bridged: BridgedKeyPathPatternComponent

    public init(bridged: BridgedKeyPathPatternComponent) {
      self.bridged = bridged
    }
  }
}

extension KeyPathPattern.Component {
  public struct ComputedPropertyId {
    public let bridged: BridgedKeyPathPatternComponent.ComputedPropertyId

    public init(bridged: BridgedKeyPathPatternComponent.ComputedPropertyId) {
      self.bridged = bridged
    }
  }

  public enum Kind {
    case storedProperty
    case gettableProperty
    case settableProperty
    case tupleElement
    case optionalChain
    case optionalForce
    case optionalWrap
  }

  public var kind: Kind {
    bridged.getKind().kind
  }

  public var componentType: CanonicalType {
    CanonicalType(bridged: bridged.getComponentType())
  }

  public var storedProperty: VarDecl {
    bridged.getStoredPropertyDecl().getAs(VarDecl.self)
  }

  public var tupleIndex: Int {
    bridged.getTupleIndex()
  }

  public var computedPropertyId: ComputedPropertyId {
    ComputedPropertyId(bridged: bridged.getComputedPropertyId())
  }

  public var computedPropertyGetter: Function {
    bridged.getComputedPropertyGetter().function
  }

  public var computedPropertySetter: Function {
    bridged.getComputedPropertySetter().function
  }

  public var subscriptIndices: BridgedArrayRef {
    bridged.getSubscriptIndices()
  }

  public var subscriptIndexEquals: Function {
    bridged.getSubscriptIndexEquals().function
  }

  public var subscriptIndexHash: Function {
    bridged.getSubscriptIndexHash().function
  }

  public var externalDecl: AbstractStorageDecl {
    bridged.getExternalDecl().getAs(AbstractStorageDecl.self)
  }

  public var externalSubstitutions: SubstitutionMap {
    SubstitutionMap(bridged: bridged.getExternalSubstitutions())
  }
}

extension KeyPathPattern.Component.Kind {
  public var bridged: BridgedKeyPathPatternComponent.Kind {
    switch self {
    case .storedProperty: .StoredProperty
    case .gettableProperty: .GettableProperty
    case .settableProperty: .SettableProperty
    case .tupleElement: .TupleElement
    case .optionalChain: .OptionalChain
    case .optionalForce: .OptionalForce
    case .optionalWrap: .OptionalWrap
    }
  }
}

extension BridgedKeyPathPatternComponent.Kind {
  public var kind: KeyPathPattern.Component.Kind {
    switch self {
    case .StoredProperty: .storedProperty
    case .GettableProperty: .gettableProperty
    case .SettableProperty: .settableProperty
    case .TupleElement: .tupleElement
    case .OptionalChain: .optionalChain
    case .OptionalForce: .optionalForce
    case .OptionalWrap: .optionalWrap
    default:
      fatalError("unsupported keypath component kind")
    }
  }
}

//============================================================================//
// KeyPathPattern.ComponentArray
//============================================================================//

extension KeyPathPattern {
  public struct ComponentArray {
    public let bridged: BridgedKeyPathPatternComponentArray

    public init(bridged: BridgedKeyPathPatternComponentArray) {
      self.bridged = bridged
    }
  }
}

extension KeyPathPattern.ComponentArray: RandomAccessCollection, CustomReflectable {
  public var startIndex: Int {
    0
  }

  public var endIndex: Int {
    bridged.count()
  }

  public subscript(_ index: Int) -> KeyPathPattern.Component {
    KeyPathPattern.Component(bridged: bridged.at(index))
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}
