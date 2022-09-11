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

public struct VTable : CustomStringConvertible, CustomReflectable {
  let bridged: BridgedVTable

  public init(bridged: BridgedVTable) { self.bridged = bridged }

  public struct Entry : CustomStringConvertible, CustomReflectable {
    fileprivate let bridged: BridgedVTableEntry
    
    public var function: Function { SILVTableEntry_getFunction(bridged).function }

    public var description: String {
      let stdString = SILVTableEntry_debugDescription(bridged)
      return String(_cxxString: stdString)
    }

    public var customMirror: Mirror { Mirror(self, children: []) }
  }

  public struct EntryArray : BridgedRandomAccessCollection {
    fileprivate let bridgedArray: BridgedArrayRef
    
    public var startIndex: Int { return 0 }
    public var endIndex: Int { return Int(bridgedArray.numElements) }
    
    public subscript(_ index: Int) -> Entry {
      precondition(index >= 0 && index < endIndex)
      return Entry(bridged: BridgedVTableEntry(ptr: bridgedArray.data! + index &* BridgedVTableEntrySize))
    }
  }

  public var entries: EntryArray {
    EntryArray(bridgedArray: SILVTable_getEntries(bridged))
  }

  public var description: String {
    let stdString = SILVTable_debugDescription(bridged)
    return String(_cxxString: stdString)
  }

  public var customMirror: Mirror { Mirror(self, children: []) }
}
