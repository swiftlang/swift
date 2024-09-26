//===--- SubstitutionMap.swift --------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import SILBridging

public struct SubstitutionMap {
  public let bridged: BridgedSubstitutionMap

  public init(bridged: BridgedSubstitutionMap) {
    self.bridged = bridged
  }
  
  public init() {
    self.bridged = BridgedSubstitutionMap()
  }

  public var isEmpty: Bool { bridged.isEmpty() }

  public var hasAnySubstitutableParams: Bool { bridged.hasAnySubstitutableParams() }

  public var conformances: ConformanceArray { ConformanceArray(substitutionMap: self) }

  public var replacementTypes: OptionalTypeArray {
    let types = BridgedTypeArray.fromReplacementTypes(bridged)
    return OptionalTypeArray(bridged: types)
  }

  public func getMethodSubstitutions(for method: Function) -> SubstitutionMap {
    return SubstitutionMap(bridged: bridged.getMethodSubstitutions(method.bridged))
  }

  public struct ConformanceArray : BridgedRandomAccessCollection {
    fileprivate let bridgedSubs: BridgedSubstitutionMap
    public let count: Int

    init(substitutionMap: SubstitutionMap) {
      self.bridgedSubs = substitutionMap.bridged
      self.count = substitutionMap.bridged.getNumConformances()
    }

    public var startIndex: Int { return 0 }
    public var endIndex: Int { return count }

    public subscript(_ index: Int) -> ProtocolConformance {
      assert(index >= startIndex && index < endIndex)
      return ProtocolConformance(bridged: bridgedSubs.getConformance(index))
    }
  }
}
