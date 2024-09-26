//===--- ProtocolConformance.swift ----------------------------------------===//
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

import SILBridging

// TODO: move `ProtocolConformance` to an AST module, once we have it

public struct ProtocolConformance {
  public let bridged: BridgedProtocolConformance

  init(bridged: BridgedProtocolConformance) {
    self.bridged = bridged
  }

  public var isConcrete: Bool { bridged.isConcrete() }

  public var isValid: Bool { bridged.isValid() }

  public var isSpecialized: Bool {
    assert(isConcrete)
    return bridged.isSpecializedConformance()
  }

  public var genericConformance: ProtocolConformance {
    assert(isSpecialized)
    return bridged.getGenericConformance().protocolConformance
  }

  public var specializedSubstitutions: SubstitutionMap {
    assert(isSpecialized)
    return SubstitutionMap(bridged: bridged.getSpecializedSubstitutions())
  }
}

public struct ProtocolConformanceArray : RandomAccessCollection, CustomReflectable {
  public let bridged: BridgedProtocolConformanceArray

  public var startIndex: Int { return 0 }
  public var endIndex: Int { return bridged.getCount() }

  public init(bridged: BridgedProtocolConformanceArray) {
    self.bridged = bridged
  }

  public subscript(_ index: Int) -> ProtocolConformance {
    bridged.getAt(index).protocolConformance
  }

  public var customMirror: Mirror {
    let c: [Mirror.Child] = map { (label: nil, value: $0) }
    return Mirror(self, children: c)
  }
}

extension BridgedProtocolConformance {
  public var protocolConformance: ProtocolConformance { ProtocolConformance(bridged: self) }
}
