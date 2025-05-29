//===----------------------------------------------------------------------===//
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

import Foundation
import LinuxSystemHeaders

public class SymbolCache {
  public typealias SymbolRange = (start: UInt64, end: UInt64)
  public typealias SymbolInfo = (start: UInt64, end: UInt64, module: String, name: String)
  public typealias SymbolLookup = [String: [String: SymbolRange]]

  public let symbolLookup: SymbolLookup
  let linkMap: LinkMap

  // an array of all symbols sorted by their start address
  lazy var sortedAddressLookup: [SymbolInfo] = {
    var addressLookup: [SymbolInfo] = []
    for (module, symbols) in self.symbolLookup {
      for (name, (start, end)) in symbols {
        addressLookup.append((start: start, end: end, module: module, name: name))
      }
    }
    addressLookup.sort { $0.start < $1.start }
    return addressLookup
  }()

  public init(for process: Process) throws {
    self.linkMap = try LinkMap(for: process)
    var symbolLookup: SymbolLookup = [:]
    for linkMapEntry in linkMap.entries {
      guard let elfFile = try? ElfFile(filePath: linkMapEntry.moduleName) else { continue }
      let symbolMap = try elfFile.loadSymbols(baseAddress: linkMapEntry.baseAddress)
      symbolLookup[linkMapEntry.moduleName] = symbolMap
    }
    self.symbolLookup = symbolLookup
  }

  // find the address range for the requested symbol
  public func address(of symbol: String) -> SymbolRange? {
    for (_, symbols) in symbolLookup { if let range = symbols[symbol] { return range } }
    return nil
  }

  // find and return symbol that contains the specified address
  public func symbol(for address: UInt64) -> SymbolInfo? {
    var lowerBound = 0
    var upperBound = self.sortedAddressLookup.count
    while lowerBound < upperBound {
      let currentIndex = (lowerBound + upperBound) / 2
      let entry = self.sortedAddressLookup[currentIndex]
      if entry.start > address {
        upperBound = currentIndex
      } else if entry.end <= address {
        lowerBound = currentIndex + 1
      } else {
        precondition(address >= entry.start)
        precondition(address < entry.end)
        return entry
      }
    }
    return nil
  }
}
