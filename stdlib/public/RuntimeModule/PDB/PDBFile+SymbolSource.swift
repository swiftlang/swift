//===--- PDBFile+SymbolSource.swift - PDB support for Swift ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Implements the SymbolSource protocol for PDB files.
//
//===----------------------------------------------------------------------===//

import Swift

@_spi(SymbolLocation)
extension PDBFile: SymbolSource {
  public func lookupSymbol(
    address: SymbolSource.Address
  ) -> SymbolSource.Symbol? {
    guard let symbolLookup = lookup(address: UInt32(address)) else {
      return nil
    }

    return SymbolSource.Symbol(name: symbolLookup.name,
                               offset: Int(symbolLookup.offset),
                               size: nil)
  }

  public func sourceLocation(
    for address: SymbolSource.Address
  ) -> SymbolSource.SourceLocation? {
    guard let symbolLookup = lookup(address: UInt32(address)) else {
      return nil
    }

    guard let location = symbolLookup.sourceLocation else {
      return nil
    }

    return SymbolSource.SourceLocation(
      path: location.filename,
      lineRange: location.lineRange,
      columnRange: location.columnRange
    )
  }

  public func inlineCallSites(
    at address: SymbolSource.Address
  ) -> Array<SymbolSource.CallSiteInfo> {
    return []
  }
}
