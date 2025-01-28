//===--- ImageMap+Darwin.swift --------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Darwin specifics for ImageMap capture.
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)

import Swift

internal import Darwin
internal import BacktracingImpl.OS.Darwin

fileprivate func getSysCtlString(_ name: String) -> String? {
  return withUnsafeTemporaryAllocation(byteCount: 256, alignment: 16) {
    (buffer: UnsafeMutableRawBufferPointer) -> String? in

    var len = buffer.count
    let ret = sysctlbyname(name,
                           buffer.baseAddress, &len,
                           nil, 0)
    if ret != 0 {
      return nil
    }

    return String(validatingUTF8:
                    buffer.baseAddress!.assumingMemoryBound(to: CChar.self))
  }
}

extension ImageMap {

  private static let platform = {
    #if os(macOS)
    var platform = "macOS"
    #elseif os(iOS)
    var platform = "iOS"
    #elseif os(watchOS)
    var platform = "watchOS"
    #elseif os(tvOS)
    var platform = "tvOS"
    #elseif os(visionOS)
    var platform = "visionOS"
    #endif

    let osVersion = getSysCtlString("kern.osversion") ?? "<unknown>"
    let osProductVersion = getSysCtlString("kern.osproductversion") ?? "<unknown>"

    return "\(platform) \(osProductVersion) (\(osVersion))"
  }()

  private static func withDyldProcessInfo<T>(for task: task_t,
                                             fn: (OpaquePointer?) throws -> T)
    rethrows -> T {
    var kret = kern_return_t(KERN_SUCCESS)
    let dyldInfo = _dyld_process_info_create(task, 0, &kret)

    if kret != KERN_SUCCESS {
      fatalError("error: cannot create dyld process info")
    }

    defer {
      _dyld_process_info_release(dyldInfo)
    }

    return try fn(dyldInfo)
  }

  @_spi(Internal)
  public static func capture(for process: Any) -> ImageMap {
        var images: [Image] = []
    let task = process as! task_t

    withDyldProcessInfo(for: task) { dyldInfo in
      _dyld_process_info_for_each_image(dyldInfo) {
        (machHeaderAddress, uuid, path) in

        if let path = path, let uuid = uuid {
          let pathString = String(cString: path)
          let theUUID = Array(UnsafeBufferPointer(start: uuid,
                                                  count: MemoryLayout<uuid_t>.size))
          let name: String
          if let slashIndex = pathString.lastIndex(of: "/") {
            name = String(pathString.suffix(from:
                                              pathString.index(after:slashIndex)))
          } else {
            name = pathString
          }

          // Find the end of the __TEXT segment
          var endOfText = machHeaderAddress + 4096

          _dyld_process_info_for_each_segment(dyldInfo, machHeaderAddress) {
            address, size, name in

            if let name = String(validatingCString: name!), name == "__TEXT" {
              endOfText = address + size
            }
          }

          images.append(Image(name: name,
                              path: pathString,
                              uniqueID: theUUID,
                              baseAddress: machHeaderAddress,
                              endOfText: endOfText))
        }
      }
    }

    images.sort(by: { $0.baseAddress < $1.baseAddress })

    return ImageMap(
      platform: ImageMap.platform,
      images: images,
      wordSize: .sixtyFourBit
    )
  }

}

#endif // os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
