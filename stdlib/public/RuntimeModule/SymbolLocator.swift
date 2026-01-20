//===--- SymbolLocator.swift - Symbol file finding for Swift --------------===//
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
// Defines a protocol for a symbol locator that, when handed an image, can
// determine where the symbols for that image might be located.
//
//===----------------------------------------------------------------------===//

import Swift

internal import BacktracingImpl.Runtime

@_spi(SymbolLocation)
public struct SymbolLocationSymbol: Sendable, Hashable, Equatable {
  var name: String
  var offset: Int
  var size: Int?
}

@_spi(SymbolLocation)
public struct SymbolLocationCallSiteInfo: Sendable, Hashable, Equatable {
  var rawName: String?
  var name: String?
  var location: SymbolicatedBacktrace.SourceLocation
}

/// A `SymbolSource` is a place where we can fetch information about symbols.
/// It *may* be an image of some sort, or it could be some other kind of
/// thing (e.g. for Windows binaries, a PDB file).
@_spi(SymbolLocation)
public protocol SymbolSource {
  typealias Address = UInt64

  typealias Symbol = SymbolLocationSymbol
  typealias CallSiteInfo = SymbolLocationCallSiteInfo
  typealias SourceLocation = SymbolicatedBacktrace.SourceLocation

  func lookupSymbol(address: Address) -> Symbol?
  func sourceLocation(for address: Address) -> SourceLocation?
  func inlineCallSites(at address: Address) -> Array<CallSiteInfo>
}

@_spi(SymbolLocation)
public protocol SymbolLocator {

  typealias Image = SymbolLoader.Image
  typealias ImageFormat = SymbolLoader.ImageFormat

  func findSymbols(for image: any Image) -> (any SymbolSource)?
  func findSymbols(for image: any Image,
                   format: ImageFormat) -> (any SymbolSource)?

}

@_spi(Symbolication)
extension SymbolLocator {
  public func findSymbols(for image: any Image) -> (any SymbolSource)? {
    return findSymbols(for: image, format: .auto)
  }
}

@_spi(SymbolLocation)
extension Backtrace.Image: SymbolLocator.Image {
  public var uuid: [UInt8]? { uniqueID }
  public var age: UInt32? { nil }
}

@_spi(SymbolLocation)
public struct SymbolLoader {

  public protocol Image {
    var name: String? { get }
    var path: String? { get }
    var uuid: [UInt8]? { get }
    var age: UInt32? { get }
  }

  public enum ImageFormat {
    case auto
    case elf
    case peCoff
  }

  func loadSymbols(for image: any Image,
                   from path: String) -> (any SymbolSource)? {
    let pdbCache = PDBCache.threadLocal

    if let pdbFile = pdbCache.lookup(path: path) {
      if pdbFile.signature == image.uuid && pdbFile.age == image.age {
        return pdbFile
      }
    }

    let elfCache = ElfImageCache.threadLocal

    if let result = elfCache.lookup(path: path) {
      switch result {
        case let .elf32Image(elfImage):
          if let uuid = image.uuid, uuid != elfImage.uuid {
            return nil
          }
          return elfImage
        case let .elf64Image(elfImage):
          if let uuid = image.uuid, uuid != elfImage.uuid {
            return nil
          }
          return elfImage
      }
    }

    let peCache = PeImageCache.threadLocal

    if let coffImage = peCache.lookup(path: path) {
      if coffImage.getDwarfSection(.debugLine) != nil {
        return coffImage
      }
    }

    return nil
  }
}
