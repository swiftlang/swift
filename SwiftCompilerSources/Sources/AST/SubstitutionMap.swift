//===--- SubstitutionMap.swift --------------------------------------------===//
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

/// SubstitutionMap describes the mapping of abstract types to replacement types,
/// together with associated conformances to use for deriving nested types and conformances.
///
/// Substitution maps are primarily used when performing substitutions into any entity that
/// can reference type parameters and conformances.
public struct SubstitutionMap: CustomStringConvertible {
  public let bridged: BridgedSubstitutionMap

  public init(bridged: BridgedSubstitutionMap) {
    self.bridged = bridged
  }
  
  public init() {
    self.bridged = BridgedSubstitutionMap()
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }

  public var isEmpty: Bool { bridged.isEmpty() }

  public var hasAnySubstitutableParams: Bool { bridged.hasAnySubstitutableParams() }

  public var conformances: ConformanceArray { ConformanceArray(substitutionMap: self) }

  public struct ConformanceArray : BridgedRandomAccessCollection {
    fileprivate let bridgedSubs: BridgedSubstitutionMap
    public let count: Int

    init(substitutionMap: SubstitutionMap) {
      self.bridgedSubs = substitutionMap.bridged
      self.count = substitutionMap.bridged.getNumConformances()
    }

    public var startIndex: Int { return 0 }
    public var endIndex: Int { return count }

    public subscript(_ index: Int) -> Conformance {
      assert(index >= startIndex && index < endIndex)
      return Conformance(bridged: bridgedSubs.getConformance(index))
    }
  }
}
