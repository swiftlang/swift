//===--- Conformance.swift ------------------------------------------------===//
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

/// Describes how a particular type conforms to a given protocol, providing the mapping from the protocol
/// members to the type (or extension) members that provide the functionality for the concrete type.
///
///  TODO: Ideally, `Conformance` should be an enum
public struct Conformance: CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedConformance

  public init(bridged: BridgedConformance) {
    self.bridged = bridged
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  public var isConcrete: Bool { bridged.isConcrete() }

  public var isValid: Bool { bridged.isValid() }

  public var type: Type {
    assert(isConcrete)
    return Type(bridged: bridged.getType())
  }

  public var isSpecialized: Bool {
    assert(isConcrete)
    return bridged.isSpecializedConformance()
  }

  public var genericConformance: Conformance {
    assert(isSpecialized)
    return bridged.getGenericConformance().conformance
  }

  public var isInherited: Bool {
    assert(isConcrete)
    return bridged.isInheritedConformance()
  }

  public var inheritedConformance: Conformance {
    assert(isInherited)
    return bridged.getInheritedConformance().conformance
  }

  public var specializedSubstitutions: SubstitutionMap {
    assert(isSpecialized)
    return SubstitutionMap(bridged: bridged.getSpecializedSubstitutions())
  }
}

public struct ConformanceArray : RandomAccessCollection, CustomReflectable {
  public let bridged: BridgedConformanceArray

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return bridged.getCount() }

  public init(bridged: BridgedConformanceArray) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> Conformance {
    bridged.getAt(index).conformance
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}

extension BridgedConformance {
  public var conformance: Conformance { Conformance(bridged: self) }
}
