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

public struct WitnessTable : CustomStringConvertible, CustomReflectable {
  public let bridged: BridgedWitnessTable

  public init(bridged: BridgedWitnessTable) { self.bridged = bridged }

  public struct Entry : CustomStringConvertible, CustomReflectable {
    fileprivate let bridged: BridgedWitnessTableEntry
    
    public enum Kind {
      case method
      case associatedType
      case associatedTypeProtocol
      case baseProtocol
    }
    
    public var kind: Kind {
      switch SILWitnessTableEntry_getKind(bridged) {
        case SILWitnessTableEntry_Method:                 return .method
        case SILWitnessTableEntry_AssociatedType:         return .associatedType
        case SILWitnessTableEntry_AssociatedTypeProtocol: return .associatedTypeProtocol
        case SILWitnessTableEntry_BaseProtocol:           return .baseProtocol
        default:
          fatalError("unknown witness table kind")
      }
    }
    
    public var methodFunction: Function? {
      assert(kind == .method)
      return SILWitnessTableEntry_getMethodFunction(bridged).function
    }

    public var description: String {
      let stdString = SILWitnessTableEntry_debugDescription(bridged)
      return String(_cxxString: stdString)
    }

    public var customMirror: Mirror { Mirror(self, children: []) }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridged: BridgedArrayRef
    
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return Int(bridged.numElements) }
    
    public subscript(_ index: Int) -> Entry {
      precondition(index >= 0 && index < endIndex)
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

  public var customMirror: Mirror { Mirror(self, children: []) }
}

public struct DefaultWitnessTable : CustomStringConvertible, CustomReflectable {
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

  public var customMirror: Mirror { Mirror(self, children: []) }
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
