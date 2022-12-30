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
    
    public typealias Kind = swift.SILWitnessTable.WitnessKind
    
    public var kind: Kind {
      return SILWitnessTableEntry_getKind(bridged)
    }
    
    public var methodFunction: Function? {
      assert(kind == .Method)
      return SILWitnessTableEntry_getMethodFunction(bridged).function
    }

    public var description: String {
      let stdString = SILWitnessTableEntry_debugDescription(bridged)
      return String(_cxxString: stdString)
    }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridged: BridgedArrayRef
    
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return Int(bridged.numElements) }
    
    public subscript(_ index: Int) -> Entry {
      assert(index >= 0 && index < endIndex)
      return Entry(bridged: BridgedWitnessTableEntry(ptr: bridged.data! + index &* BridgedWitnessTableEntrySize))
    }
  }

  public var entries: EntryArray {
    EntryArray(bridged: SILWitnessTable_getEntries(bridged))
  }

  public var description: String {
    let stdString = SILWitnessTable_debugDescription(bridged)
    return String(_cxxString: stdString)
  }
}

public struct DefaultWitnessTable : CustomStringConvertible, NoReflectionChildren {
  public let bridged: BridgedDefaultWitnessTable

  public init(bridged: BridgedDefaultWitnessTable) { self.bridged = bridged }

  public typealias Entry = WitnessTable.Entry
  public typealias EntryArray = WitnessTable.EntryArray

  public var entries: EntryArray {
    EntryArray(bridged: SILDefaultWitnessTable_getEntries(bridged))
  }

  public var description: String {
    let stdString = SILDefaultWitnessTable_debugDescription(bridged)
    return String(_cxxString: stdString)
  }
}

extension OptionalBridgedWitnessTable {
  public var table: WitnessTable? {
    if let p = ptr {
      return WitnessTable(bridged: BridgedWitnessTable(ptr: p))
    }
    return nil
  }
}

extension OptionalBridgedDefaultWitnessTable {
  public var table: DefaultWitnessTable? {
    if let p = ptr {
      return DefaultWitnessTable(bridged: BridgedDefaultWitnessTable(ptr: p))
    }
    return nil
  }
}
