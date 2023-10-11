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

import SILBridging

public struct WitnessTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedWitnessTable

  public init(bridged: BridgedWitnessTable) { self.bridged = bridged }

  public struct Entry : CustomStringConvertible, NoReflectionChildren {
    fileprivate let bridged: BridgedWitnessTableEntry
    
    public typealias Kind = BridgedWitnessTableEntry.Kind

    public var kind: Kind {
      return bridged.getKind()
    }
    
    public var methodFunction: Function? {
      assert(kind == .Method)
      return bridged.getMethodFunction().function
    }

    public var description: String {
      return String(taking: bridged.getDebugDescription())
    }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let base: BridgedWitnessTableEntry
    public let count: Int
    
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return count }
    
    public subscript(_ index: Int) -> Entry {
      assert(index >= startIndex && index < endIndex)
      return Entry(bridged: base.advanceBy(index))
    }
  }

  public var entries: EntryArray {
    let entries = bridged.getEntries()
    return EntryArray(base: entries.base, count: entries.count)
  }

  public var description: String {
    return String(taking: bridged.getDebugDescription())
  }
}

public struct DefaultWitnessTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedDefaultWitnessTable

  public init(bridged: BridgedDefaultWitnessTable) { self.bridged = bridged }

  public typealias Entry = WitnessTable.Entry
  public typealias EntryArray = WitnessTable.EntryArray

  public var entries: EntryArray {
    let entries = bridged.getEntries()
    return EntryArray(base: entries.base, count: entries.count)
  }

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
