//===--- DefaultSymbolLocator.swift - Symbol file finding for Swift -------===//
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
// The default symbol locator used by the Runtime module.
//
//===----------------------------------------------------------------------===//

import Swift

internal import BacktracingImpl.Runtime

// We look for symbols at SWIFT_SYMBOL_PATH.  If that is not set,
// then on UNIX systems we look in /usr/lib/debug/, while on Windows
// we look in C:\Symbols..
#if os(Windows)
let defaultSymbolPath = "C:\\Symbols"
#else
let defaultSymbolPath = "/usr/lib/debug"
#endif

fileprivate let symbolPath: String = {
  return getEnv("SWIFT_SYMBOL_PATH") ?? defaultSymbolPath
}()

#if os(Windows)
fileprivate let sep = "\\"
fileprivate let elfSymbolPath = "\(symbolPath)\\ELF"
#else
fileprivate let sep = "/"
fileprivate let elfSymbolPath = "\(symbolPath)/.build-id"
#endif

@_spi(SymbolLocation)
public class DefaultSymbolLocator: SymbolLocator {

  public typealias Image = SymbolLocator.Image
  public typealias ImageFormat = SymbolLocator.ImageFormat

  public static let shared = DefaultSymbolLocator()

  enum CacheKey: Equatable, Hashable {
    case path(String)
    case uuid([UInt8])
    case uuidAndAge([UInt8], UInt32)
  }

  var cache: [CacheKey: any SymbolSource] = [:]

  private func findSymbolsForElf(image: any Image) -> (any SymbolSource)? {
    func loadElfImage(path: String, uuid: [UInt8]?) -> ElfImageCache.Result? {
      let elfCache = ElfImageCache.threadLocal

      guard let result = elfCache.lookup(path: path) else {
        return nil
      }

      switch result {
        case let .elf32Image(elfImage):
          if elfImage.uuid != uuid {
            return nil
          }

        case let .elf64Image(elfImage):
          if elfImage.uuid != uuid {
            return nil
          }
      }

      return result
    }

    func toSymbolSource(_ result: ElfImageCache.Result) -> (any SymbolSource)? {
      switch result {
        case let .elf32Image(elfImage):
          return elfImage
        case let .elf64Image(elfImage):
          return elfImage
      }
    }

    // Check the cache first
    if let uuid = image.uuid,
       let source = cache[.uuid(uuid)] {
      return source
    }

    if let path = image.path,
       let source = cache[.path(path)] {
      return source
    }

    // Now try finding it using the UUID
    if let uuid = image.uuid {
      let uuidString = hex(uuid)
      let uuidSuffix = uuidString.dropFirst(2)
      let uuidPrefix = uuidString.prefix(2)
      let path = "\(elfSymbolPath)\(sep)\(uuidPrefix)\(sep)\(uuidSuffix).debug"
      if let result = loadElfImage(path: path, uuid: image.uuid) {
        let source = toSymbolSource(result)
        cache[.uuid(uuid)] = source
        return source
      }
    }

    // We must be able to load the original image (as an ELF image)
    guard let imagePath = find(image: image),
          let result = loadElfImage(path: imagePath, uuid: image.uuid) else {
      return nil
    }

    if let realImagePath = realPath(imagePath) {
      let imageDir = dirname(realImagePath)
      let debugLink: String?
      let debugAltLink: String?

      switch result {
        case let .elf32Image(elfImage):
          debugLink = elfImage.getDebugLink()?.link
          debugAltLink = elfImage.getDebugAltLink()?.link

        case let .elf64Image(elfImage):
          debugLink = elfImage.getDebugLink()?.link
          debugAltLink = elfImage.getDebugAltLink()?.link
      }

      let tryLink = { (_ link: String) -> (any SymbolSource)? in
        let path1 = "\(imageDir)\(sep)\(link)"
        if let result = loadElfImage(path: path1, uuid: image.uuid) {
          return toSymbolSource(result)
        }

        let path2 = "\(imageDir)\(sep).debug\(sep)\(link)"
        if let result = loadElfImage(path: path2, uuid: image.uuid) {
          return toSymbolSource(result)
        }

        let path3 = "\(elfSymbolPath)\(sep)\(imageDir)\(sep)\(link)"
        if let result = loadElfImage(path: path3, uuid: image.uuid) {
          return toSymbolSource(result)
        }

        return nil
      }

      if let debugAltLink = debugAltLink, let source = tryLink(debugAltLink) {
        if let uuid = image.uuid {
          cache[.uuid(uuid)] = source
        } else if let path = image.path {
          cache[.path(path)] = source
        }
        return source
      }

      if let debugLink = debugLink, let source = tryLink(debugLink) {
        if let uuid = image.uuid {
          cache[.uuid(uuid)] = source
        } else if let path = image.path {
          cache[.path(path)] = source
        }
        return source
      }
    }

    let debugData: ImageSource?
    switch result {
      case let .elf32Image(elfImage):
        debugData = elfImage.getSection(".gnu_debugdata")

      case let .elf64Image(elfImage):
        debugData = elfImage.getSection(".gnu_debugdata")
    }

    if let debugData {
      do {
        let source = try ImageSource(lzmaCompressedImageSource: debugData)
        let symbolSource: any SymbolSource
        switch result {
          case .elf32Image(_):
            symbolSource = try Elf32Image(source: source)

          case .elf64Image(_):
            symbolSource = try Elf64Image(source: source)
        }

        if let uuid = image.uuid {
          cache[.uuid(uuid)] = symbolSource
        } else if let path = image.path {
          cache[.path(path)] = symbolSource
        }
        return symbolSource
      } catch let CompressedImageSourceError.libraryNotFound(library) {
        swift_reportWarning(0,
                            """
                              swift-runtime: warning: \(library) not found, \
                              unable to decode the .gnu_debugdata section in \
                              \(image.name ?? "<unnamed image>")
                              """)
      } catch {
      }
    }

    // Otherwise, return the image itself
    let source = toSymbolSource(result)
    if let uuid = image.uuid {
      cache[.uuid(uuid)] = source
    } else if let path = image.path {
      cache[.path(path)] = source
    }
    return source
  }

  private func findSymbolsForPeCoff(image: any Image) -> (any SymbolSource)? {
    var result: (any SymbolSource)? = nil

    func loadPeCoffImage(
      path: String, uuid: [UInt8]?, age: UInt32?
    ) -> PeCoffImage? {
      let peCache = PeImageCache.threadLocal

      guard let peImage = peCache.lookup(path: path) else {
        return nil
      }

      if let codeview = peImage.codeview,
         let uuid,
         let age,
         codeview.uuid != uuid
           || codeview.age != age {
        return nil
      }
      return peImage
    }

    let uuid: [UInt8]?
    let age: UInt32?

    // Break the UUID into a (uuid, age) pair.
    if let theUUID = image.uuid {
      if let theAge = image.age {
        uuid = theUUID
        age = theAge
      } else {
        // PE-COFF image IDs are a UUID with a DWORD appended
        if theUUID.count != 20 {
          return nil
        }

        uuid = Array(theUUID[0..<16])
        age = theUUID[16..<20].withUnsafeBytes {
          return $0.assumingMemoryBound(to: UInt32.self).baseAddress!.pointee
        }
      }
    } else {
      uuid = nil
      age = nil
    }

    // Check the cache first
    if let uuid, let age, let source = cache[.uuidAndAge(uuid, age)] {
      return source
    }

    if let path = image.path,
       let source = cache[.path(path)] {
      return source
    }

    // See if we can load the image
    let peImage: PeCoffImage?
    if let path = find(image: image) {
      peImage = loadPeCoffImage(path: path, uuid: uuid, age: age)
    } else {
      peImage = nil
    }

    // Try PDB first
    if let uuid, let age {
      let cache = PDBCache.threadLocal

      // First, try the path from the image itself
      if result == nil,
         let peImage, let codeview = peImage.codeview,
         let pdbFile = cache.lookup(path: codeview.pdbPath),
         uuid == pdbFile.signature
           && age == pdbFile.age {
        result = pdbFile
      }

      if result == nil,
         let sourcePath = image.path,
         let imagePath = realPath(sourcePath) {
        // Break apart the image name
        let (imageDir, imageName) = splitpath(imagePath)
        let pdbName: String
        let ext: Substring?
        if let dotNdx = imageName.lastIndex(of: ".") {
          let stem = imageName.prefix(upTo: dotNdx)
          ext = imageName.suffix(from: imageName.index(after: dotNdx))
          pdbName = "\(stem).pdb"
        } else {
          ext = nil
          pdbName = "\(imageName).pdb"
        }

        // Try the directory containing the image
        let cwd = imageDir
        if result == nil,
           let pdbFile = cache.lookup(path: "\(cwd)\(sep)\(pdbName)"),
           uuid == pdbFile.signature && age == pdbFile.age {
          result = pdbFile
        }

        // Also try <ext>/<filename>.pdb and symbols/<ext>/<filename>.pdb
        if let ext {
          let pdbPath1 = "\(cwd)\(sep)\(ext)\(sep)\(pdbName)"
          if result == nil,
             let pdbFile = PDBFile(path: pdbPath1),
             uuid == pdbFile.signature && age == pdbFile.age {
            result = pdbFile
          }

          // As well as symbols/ext/filename.pdb
          let pdbPath2 = "\(cwd)\(sep)symbols\(sep)\(ext)\(sep)\(pdbName)"
          if result == nil,
             let pdbFile = PDBFile(path: pdbPath2),
             uuid == pdbFile.signature && age == pdbFile.age {
            result = pdbFile
          }
        }

        // Finally, try the hard-coded symbol path
        let uuidElt = "\(hex(uuid))\(String(age, radix: 16))"
        let pdbPath = "\(symbolPath)\(sep)\(pdbName)\(sep)\(uuidElt)\(sep)\(pdbName)"
        if result == nil,
           let pdbFile = PDBFile(path: pdbPath),
           uuid == pdbFile.signature && age == pdbFile.age {
          result = pdbFile
        }
      }
    }

    // Next, if we loaded the image, see if it has a DWARF line table
    if peImage?.getDwarfSection(.debugLine) != nil {
      if let pdbSource = result {
        // If we have both, prioritise DWARF as it's richer
        result = AggregatingSymbolSource(sources: [peImage!, pdbSource])
      } else {
        result = peImage!
      }
    }

    if let result {
      if let uuid, let age {
        cache[.uuidAndAge(uuid, age)] = result
      } else if let path = image.path {
        cache[.path(path)] = result
      }
    }

    // Note: for PE-COFF, we assume that the COFF symbol table is empty,
    // which is usually the case, so we only return the PE-COFF image if
    // it has DWARF data.

    return result
  }

  public func findSymbols(for image: any Image,
                          format: ImageFormat) -> (any SymbolSource)? {
    if format == .auto || format == .elf {
      if let source = findSymbolsForElf(image: image) {
        return source
      }
    }

    if format == .auto || format == .peCoff {
      if let source = findSymbolsForPeCoff(image: image) {
        return source
      }
    }

    return nil
  }

  public func find(image: any Image) -> String? {
    let elfCache = ElfImageCache.threadLocal
    let peCache = PeImageCache.threadLocal

    guard let imagePath = image.path else {
      return nil
    }

    if let result = elfCache.lookup(path: imagePath) {
      switch result {
        case let .elf32Image(elfImage):
          if elfImage.uuid != image.uuid {
            return nil
          }

        case let .elf64Image(elfImage):
          if elfImage.uuid != image.uuid {
            return nil
          }
      }

      return imagePath
    }

    if let peImage = peCache.lookup(path: imagePath) {
      if let codeview = peImage.codeview,
         let uuid = image.uuid,
         let age = image.age,
         codeview.uuid != uuid
           || codeview.age != age {
        return nil
      }

      return imagePath
    }

    return nil
  }
}
