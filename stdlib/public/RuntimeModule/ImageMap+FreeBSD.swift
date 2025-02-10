//===--- ImageMap+FreeBSD.swift --------------------------------*- swift -*-===//
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
//  FreeBSD specifics for ImageMap capture.
//
//===----------------------------------------------------------------------===//

#if os(FreeBSD)

import Swift

internal import FreeBSD

internal import BacktracingImpl.ImageFormats.Elf
internal import BacktracingImpl.OS.FreeBSD

fileprivate func readOSRelease() -> [String:String]? {
  let result: String? = withUnsafeTemporaryAllocation(of: CChar.self, capacity: 256) { buffer in
    let mib = [CTL_KERN, KERN_OSRELEASE]
    var len: Int = 256
    return mib.withUnsafeBufferPointer { mibp in
      withUnsafeMutablePointer(to: &len) {
        let result = sysctl(mibp.baseAddress, 2, buffer.baseAddress!, $0, nil, 0)
        guard result != -1 else { return nil }
        return String(cString: buffer.baseAddress!)
      }
    }
  }

  return result == nil ? nil : ["VERSION": result!]
}

extension ImageMap {

  private static var platform = {
    "FreeBSD"
  }()

  private struct AddressRange {
    var low: Address = 0
    var high: Address = 0
  }

  @_specialize(exported: true, kind: full, where M == UnsafeLocalMemoryReader)
  @_specialize(exported: true, kind: full, where M == RemoteMemoryReader)
  @_specialize(exported: true, kind: full, where M == LocalMemoryReader)
  @_spi(Internal)
  public static func capture<M: MemoryReader>(
    using reader: M,
    forProcess pid: Int? = nil
  ) -> ImageMap {
    var images: [Image] = []

    let wordSize: WordSize

    #if arch(x86_64) || arch(arm64) || arch(arm64_32)
    wordSize = .sixtyFourBit
    #elseif arch(i386) || arch(arm)
    wordSize = .thirtyTwoBit
    #endif

    var vmmapCount: Int32 = 0
    let cEntries = kinfo_getvmmap(Int32(pid ?? -1), &vmmapCount)
    let entries = Array(UnsafeBufferPointer<kinfo_vmentry>(start: cEntries, count: Int(vmmapCount)))
    free(cEntries)

    var mappedFiles: [(String, AddressRange)] = []
    for entry in entries {
      if entry.kve_type == KVME_TYPE_VNODE {
        let range = AddressRange(low: entry.kve_start, high: entry.kve_end)
        let pathname: String = withUnsafePointer(to: entry.kve_path) { ptr in
          let charptr =  UnsafeRawPointer(ptr).assumingMemoryBound(to: CChar.self)
          return String(cString: charptr)
        }

        mappedFiles.append((pathname, range))
      }
    }

    // Look at each mapped file to see if it's an ELF image
    for (path, range) in mappedFiles {
      // Extract the filename from path
      let name: String
      if let slashIndex = path.lastIndex(of: "/") {
        name = String(path.suffix(from: path.index(after: slashIndex)))
      } else {
        name = String(path)
      }

      // Inspect the image and extract the UUID and end of text
      guard let (endOfText, uuid) = getElfImageInfo(
              at: M.Address(exactly: range.low)!,
              using: reader
            ) else {
        // Not an ELF image
        continue
      }

      let image = Image(name: String(name),
                        path: String(path),
                        uniqueID: uuid,
                        baseAddress: range.low,
                        endOfText: Address(endOfText))

      images.append(image)
    }

    images.sort(by: { $0.baseAddress < $1.baseAddress })

    return ImageMap(
      platform: ImageMap.platform,
      images: images,
      wordSize: wordSize
    )
  }

}

#endif // os(FreeBSD)
