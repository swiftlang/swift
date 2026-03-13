//===--- WitnessTable.swift -----------------------------------------------===//
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

import AST
import SILBridging

public struct WitnessTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedWitnessTable

  public init(bridged: BridgedWitnessTable) { self.bridged = bridged }

  public enum Entry : CustomStringConvertible, NoReflectionChildren {

    case invalid

    /// A witness table entry describing the witness for a method.
    /// The witness can be nil in case dead function elimination has removed the method
    /// or if the method was not serialized (for de-serialized witness tables).
    case method(requirement: DeclRef, witness: Function?)

    /// A witness table entry describing the witness for an associated type.
    case associatedType(requirement: AssociatedTypeDecl, witness: CanonicalType)

    /// A witness table entry describing the witness for an associated type's protocol requirement.
    case associatedConformance(requirement: CanonicalType, witness: Conformance)

    /// A witness table entry referencing the protocol conformance for a refined base protocol.
    case baseProtocol(requirement: ProtocolDecl, witness: Conformance)

    fileprivate init(bridged: BridgedWitnessTableEntry) {
      switch bridged.getKind() {
      case .invalid:
        self = .invalid
      case .method:
        self = .method(requirement: DeclRef(bridged: bridged.getMethodRequirement()),
                       witness: bridged.getMethodWitness().function)
      case .associatedType:
        self = .associatedType(requirement: bridged.getAssociatedTypeRequirement().getAs(AssociatedTypeDecl.self),
                               witness: CanonicalType(bridged: bridged.getAssociatedTypeWitness()))
      case .associatedConformance:
        self = .associatedConformance(requirement: CanonicalType(bridged: bridged.getAssociatedConformanceRequirement()),
                                      witness: Conformance(bridged: bridged.getAssociatedConformanceWitness()))
      case .baseProtocol:
        self = .baseProtocol(requirement: bridged.getBaseProtocolRequirement().getAs(ProtocolDecl.self),
                             witness: Conformance(bridged: bridged.getBaseProtocolWitness()))
      default:
        fatalError("invalid witness table entry")
      }
    }

    public var description: String {
      return String(taking: bridged.getDebugDescription())
    }

    public var bridged: BridgedWitnessTableEntry {
      switch self {
      case .invalid:
        return BridgedWitnessTableEntry.createInvalid()
      case .method(let requirement, let witness):
        return BridgedWitnessTableEntry.createMethod(requirement.bridged,
                                                     OptionalBridgedFunction(obj: witness?.bridged.obj))
      case .associatedType(let requirement, let witness):
        return BridgedWitnessTableEntry.createAssociatedType(requirement.bridged, witness.bridged)
      case .associatedConformance(let requirement, let witness):
        return BridgedWitnessTableEntry.createAssociatedConformance(requirement.bridged,
                                                                    witness.bridged)
      case .baseProtocol(let requirement, let witness):
        return BridgedWitnessTableEntry.createBaseProtocol(requirement.bridged, witness.bridged)
      }
    }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridgedTable: BridgedWitnessTable
    public let count: Int
    
    init(witnessTable: WitnessTable) {
      self.bridgedTable = witnessTable.bridged
      self.count = witnessTable.bridged.getNumEntries()
    }

    public var startIndex: Int { 0 }
    public var endIndex: Int { count }

    public subscript(_ index: Int) -> Entry {
      precondition(index >= startIndex && index < endIndex)
      return Entry(bridged: bridgedTable.getEntry(index))
    }
  }

  /// A lookup for a specific method with O(n) complexity.
  public func lookup(method: DeclRef) -> Function? {
    for entry in entries {
      if case .method(let req, let impl) = entry, req == method {
        return impl
      }
    }
    return nil
  }

  public var entries: EntryArray { EntryArray(witnessTable: self) }

  public var isDefinition: Bool { !bridged.isDeclaration() }

  // True, if this is a specialized witness table (currently only used in embedded mode).
  public var isSpecialized: Bool { bridged.isSpecialized() }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }
}

public struct DefaultWitnessTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedDefaultWitnessTable

  public init(bridged: BridgedDefaultWitnessTable) { self.bridged = bridged }

  public typealias Entry = WitnessTable.Entry

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridgedTable: BridgedDefaultWitnessTable
    public let count: Int

    init(witnessTable: DefaultWitnessTable) {
      self.bridgedTable = witnessTable.bridged
      self.count = witnessTable.bridged.getNumEntries()
    }

    public var startIndex: Int { 0 }
    public var endIndex: Int { count }

    public subscript(_ index: Int) -> Entry {
      precondition(index >= startIndex && index < endIndex)
      return Entry(bridged: bridgedTable.getEntry(index))
    }
  }

  public var entries: EntryArray { EntryArray(witnessTable: self) }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }
}

extension OptionalBridgedWitnessTable {
  public var witnessTable: WitnessTable? {
    if let table = table {
      return WitnessTable(bridged: BridgedWitnessTable(table: table))
    }
    return nil
  }
}

extension OptionalBridgedDefaultWitnessTable {
  public var defaultWitnessTable: DefaultWitnessTable? {
    if let table = table {
      return DefaultWitnessTable(bridged: BridgedDefaultWitnessTable(table: table))
    }
    return nil
  }
}
