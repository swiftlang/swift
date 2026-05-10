//===--- VTable.swift -----------------------------------------------------===//
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

public struct VTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedVTable

  public init(bridged: BridgedVTable) { self.bridged = bridged }

  public struct Entry : CustomStringConvertible, NoReflectionChildren {
    public let bridged: BridgedVTableEntry

    public enum Kind {
      /// The vtable entry is for a method defined directly in this class.
      case normal
      /// The vtable entry is inherited from the superclass.
      case inherited
      /// The vtable entry is inherited from the superclass, and overridden in this class.
      case overridden
    }

    fileprivate init(bridged: BridgedVTableEntry) {
      self.bridged = bridged
    }

    public init(kind: Kind, isNonOverridden: Bool, methodDecl: DeclRef, implementation: Function) {
      let bridgedKind: BridgedVTableEntry.Kind
      switch kind {
        case .normal:     bridgedKind = .Normal
        case .inherited:  bridgedKind = .Inherited
        case .overridden: bridgedKind = .Override
      }
      self.bridged = BridgedVTableEntry.create(bridgedKind, isNonOverridden,
                                               methodDecl.bridged, implementation.bridged)
    }

    public var kind: Kind {
      switch bridged.getKind() {
        case .Normal:    return .normal
        case .Inherited: return .inherited
        case .Override:  return .overridden
        default: fatalError()
      }
    }

    public var isNonOverridden: Bool { bridged.isNonOverridden() }

    public var methodDecl: DeclRef { DeclRef(bridged: bridged.getMethodDecl()) }

    public var implementation: Function { bridged.getImplementation().function }

    public var description: String {
      return String(taking: bridged.getDebugDescription())
    }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridgedTable: BridgedVTable
    public let count: Int
    
    init(vTable: VTable) {
      self.bridgedTable = vTable.bridged
      self.count = vTable.bridged.getNumEntries()
    }

    public var startIndex: Int { return 0 }
    public var endIndex: Int { return count }
    
    public subscript(_ index: Int) -> Entry {
      assert(index >= startIndex && index < endIndex)
      return Entry(bridged: bridgedTable.getEntry(index))
    }
  }

  /// Conformance entries are used for fast conformance lookup, which doesn't
  /// need to query the runtime's conformance lookup table.
  /// A conformance entry specifies if the class conforms or does not conform
  /// to a protocol. At runtime, a type cast instruction to an existential can
  /// directly load the witness table pointer from the VTable. If null, the
  /// class does not conform to the protocol.
  public enum ConformanceEntry {
    case noConformance(ProtocolDecl)
    case conformance(Conformance)
  }

  public struct ConformanceEntryArray : BridgedRandomAccessCollection {
    fileprivate let bridgedTable: BridgedVTable
    public let count: Int

    init(vTable: VTable) {
      self.bridgedTable = vTable.bridged
      self.count = vTable.bridged.getNumConformanceEntries()
    }

    public var startIndex: Int { return 0 }
    public var endIndex: Int { return count }

    public subscript(_ index: Int) -> ConformanceEntry {
      assert(index >= startIndex && index < endIndex)
      if bridgedTable.hasConformance(index) {
        return .conformance(Conformance(bridged: bridgedTable.getConformance(index)))
      } else {
        return .noConformance(bridgedTable.getProtocol(index).getAs(ProtocolDecl.self))
      }
    }
  }

  public var entries: EntryArray { EntryArray(vTable: self) }

  public var conformances: ConformanceEntryArray { ConformanceEntryArray(vTable: self) }

  public var `class`: ClassDecl { bridged.getClass().getAs(ClassDecl.self) }

  /// Returns the concrete class type if this is a specialized vTable.
  public var specializedClassType: Type? { bridged.getSpecializedClassType().typeOrNil }

  public var isSpecialized: Bool { specializedClassType != nil }

  /// A lookup for a specific method with O(1) complexity.
  public func lookup(method: DeclRef) -> Entry? {
    let bridgedEntryOrNil = bridged.lookupMethod(method.bridged)
    if bridgedEntryOrNil.hasEntry {
      return Entry(bridged: bridgedEntryOrNil.entry)
    }
    return nil
  }

  public func append(conformance: ConformanceEntry) {
    switch conformance {
      case .noConformance(let proto): bridged.appendConformance(proto.bridged)
      case .conformance(let conf):    bridged.appendConformance(conf.bridged)
    }
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }
}

extension OptionalBridgedVTable {
  public var vTable: VTable? {
    if let table {
      return VTable(bridged: BridgedVTable(vTable: table))
    }
    return nil
  }
}
