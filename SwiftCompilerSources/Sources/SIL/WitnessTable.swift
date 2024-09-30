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

  public struct Entry : CustomStringConvertible, NoReflectionChildren {
    public let bridged: BridgedWitnessTableEntry

    public typealias Kind = BridgedWitnessTableEntry.Kind

    fileprivate init(bridged: BridgedWitnessTableEntry) {
      self.bridged = bridged
    }

    public init(methodRequirement: DeclRef, methodFunction: Function) {
      self.bridged = BridgedWitnessTableEntry.createMethod(methodRequirement.bridged, methodFunction.bridged)
    }

    public var kind: Kind {
      return bridged.getKind()
    }
    
    public var methodFunction: Function? {
      assert(kind == .method)
      return bridged.getMethodFunction().function
    }

    public var methodRequirement: DeclRef {
      assert(kind == .method)
      return DeclRef(bridged: bridged.getMethodRequirement())
    }

    public var description: String {
      return String(taking: bridged.getDebugDescription())
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

  public var entries: EntryArray { EntryArray(witnessTable: self) }

  public var isDefinition: Bool { !bridged.isDeclaration() }

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
