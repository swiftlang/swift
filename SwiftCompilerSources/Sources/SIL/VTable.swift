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

import SILBridging

public struct VTable : CustomStringConvertible, NoReflectionChildren {
  let bridged: BridgedVTable

  public init(bridged: BridgedVTable) { self.bridged = bridged }

  public struct Entry : CustomStringConvertible, NoReflectionChildren {
    fileprivate let bridged: BridgedVTableEntry
    
    public var function: Function { bridged.getImplementation().function }

    public var description: String {
      return String(taking: bridged.getDebugDescription())
    }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let base: BridgedVTableEntry
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
