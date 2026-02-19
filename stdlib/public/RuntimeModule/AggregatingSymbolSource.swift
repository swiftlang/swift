//===--- AggregatingSymbolSource - Symbolication for Swift ----------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Defines a SymbolSource that can aggregate results from multiple
// SymbolSources.
//
//===----------------------------------------------------------------------===//

import Swift

@_spi(Symbolication)
public struct AggregatingSymbolSource: SymbolSource {
  public var sources: [any SymbolSource]

  public func lookupSymbol(address: Address) -> Symbol? {
    for source in sources {
      if let result = source.lookupSymbol(address: address) {
        return result
      }
    }
    return nil
  }

  public func sourceLocation(for address: Address) -> SourceLocation? {
    for source in sources {
      if let result = source.sourceLocation(for: address) {
        return result
      }
    }
    return nil
  }

  public func inlineCallSites(at address: Address) -> Array<CallSiteInfo> {
    for source in sources {
      let result = source.inlineCallSites(at: address)
      if result.count > 0 {
        return result
      }
    }
    return []
  }
}
