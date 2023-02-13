//===--- Backtrace.swift --------------------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines the `Backtrace` struct that represents a captured backtrace.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)

@_implementationOnly import Darwin.Mach
@_implementationOnly import _SwiftBacktracingShims

#endif

/// Holds a backtrace.
public struct Backtrace: CustomStringConvertible, Sendable {
  /// The type of an address.
  ///
  /// This will be `UInt32` or `UInt64` on current platforms.
  #if arch(x86_64) || arch(arm64)
  public typealias Address = UInt64
  #elseif arch(i386) || arch(arm)
  public typealias Address = UInt32
  #else
  #error("You need to fill this in for your architecture.")
  #endif

  /// The unwind algorithm to use.
  public enum UnwindAlgorithm {
    /// Choose the most appropriate for the platform.
    case auto

    /// Use the fastest viable method.
    ///
    /// Typically this means walking the frame pointers.
    case fast

    /// Use the most precise available method.
    ///
    /// On Darwin and on ELF platforms, this will use EH unwind
    /// information.  On Windows, it will use Win32 API functions.
    case precise
  }

  /// Represents an individual frame in a backtrace.
  public enum Frame: CustomStringConvertible, Sendable {
    /// An accurate program counter.
    ///
    /// This might come from a signal handler, or an exception or some
    /// other situation in which we have captured the actual program counter.
    case programCounter(Address)

    /// A return address.
    ///
    /// Corresponds to a normal function call.
    case returnAddress(Address)

    /// An async resume point.
    ///
    /// Corresponds to an `await` in an async task.
    case asyncResumePoint(Address)

    /// Indicates a discontinuity in the backtrace.
    ///
    /// This occurs when you set a limit and a minimum number of frames at
    /// the top.  For example, if you set a limit of 10 frames and a minimum
    /// of 4 top frames, but the backtrace generated 100 frames, you will see
    ///
    ///    0: frame 100 <----- bottom of call stack
    ///    1: frame 99
    ///    2: frame 98
    ///    3: frame 97
    ///    4: frame 96
    ///    5: ...       <----- omittedFrames(92)
    ///    6: frame 3
    ///    7: frame 2
    ///    8: frame 1
    ///    9: frame 0   <----- top of call stack
    ///
    /// Note that the limit *includes* the discontinuity.
    ///
    /// This is good for handling cases involving deep recursion.
    case omittedFrames(Int)

    /// Indicates a discontinuity of unknown length.
    case truncated

    /// The program counter, without any adjustment.
    public var originalProgramCounter: Address {
      switch self {
        case let .returnAddress(addr):
          return addr
        case let .programCounter(addr):
          return addr
        case let .asyncResumePoint(addr):
          return addr
        case .omittedFrames(_), .truncated:
          return 0
      }
    }

    /// The adjusted program counter to use for symbolication.
    public var adjustedProgramCounter: Address {
      switch self {
        case let .returnAddress(addr):
          return addr - 1
        case let .programCounter(addr):
          return addr
        case let .asyncResumePoint(addr):
          return addr
        case .omittedFrames(_), .truncated:
          return 0
      }
    }

    /// A textual description of this frame.
    public var description: String {
      switch self {
        case let .programCounter(addr):
          return "\(hex(addr))"
        case let .returnAddress(addr):
          return "\(hex(addr)) [ra]"
        case let .asyncResumePoint(addr):
          return "\(hex(addr)) [async]"
        case .omittedFrames(_), .truncated:
          return "..."
      }
    }
  }

  /// Represents an image loaded in the process's address space
  public struct Image: CustomStringConvertible, Sendable {
    /// The name of the image (e.g. libswiftCore.dylib).
    public var name: String

    /// The full path to the image (e.g. /usr/lib/swift/libswiftCore.dylib).
    public var path: String

    /// The build ID of the image, as a byte array (note that the exact number
    /// of bytes may vary, and that some images may not have a build ID).
    public var buildID: [UInt8]?

    /// The base address of the image.
    public var baseAddress: Backtrace.Address

    /// The end of the text segment in this image.
    public var endOfText: Backtrace.Address

    /// Provide a textual description of an Image.
    public var description: String {
      if let buildID = self.buildID {
        return "\(hex(baseAddress))-\(hex(endOfText)) \(hex(buildID)) \(name) \(path)"
      } else {
        return "\(hex(baseAddress))-\(hex(endOfText)) <no build ID> \(name) \(path)"
      }
    }
  }

  /// A list of captured frame information.
  public var frames: [Frame]

  /// A list of captured images.
  ///
  /// Some backtracing algorithms may require this information, in which case
  /// it will be filled in by the `capture()` method.  Other algorithms may
  /// not, in which case it will be empty and you can capture an image list
  /// separately yourself using `captureImages()`.
  public var images: [Image]?

  /// Holds information about the shared cache.
  public struct SharedCacheInfo: Sendable {
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    /// The UUID from the shared cache.
    public var uuid: [UInt8]

    /// The base address of the shared cache.
    public var baseAddress: Backtrace.Address

    /// Says whether there is in fact a shared cache.
    public var noCache: Bool
    #endif
  }

  /// Information about the shared cache.
  ///
  /// Holds information about the shared cache.  On Darwin only, this is
  /// required for symbolication.  On non-Darwin platforms it will always
  /// be `nil`.
  public var sharedCacheInfo: SharedCacheInfo?

  /// Capture a backtrace from the current program location.
  ///
  /// The `capture()` method itself will not be included in the backtrace;
  /// i.e. the first frame will be the one in which `capture()` was called,
  /// and its programCounter value will be the return address for the
  /// `capture()` method call.
  ///
  /// @param algorithm     Specifies which unwind mechanism to use.  If this
  ///                      is set to `.auto`, we will use the platform default.
  /// @param limit         The backtrace will include at most this number of
  ///                      frames; you can set this to `nil` to remove the
  ///                      limit completely if required.
  /// @param offset        Says how many frames to skip; this makes it easy to
  ///                      wrap this API without having to inline things and
  ///                      without including unnecessary frames in the backtrace.
  /// @param top           Sets the minimum number of frames to capture at the
  ///                      top of the stack.
  ///
  /// @returns A new `Backtrace` struct.
  @inline(never)
  public static func capture(algorithm: UnwindAlgorithm = .auto,
                             limit: Int? = 64,
                             offset: Int = 0,
                             top: Int = 16) throws -> Backtrace {
    // N.B. We use offset+1 here to skip this frame, rather than inlining
    //      this code into the client.
    return try HostContext.withCurrentContext { ctx in
      try capture(from: ctx,
                  using: UnsafeLocalMemoryReader(),
                  algorithm: algorithm,
                  limit: limit,
                  offset: offset + 1,
                  top: top)
    }
  }

  @_spi(Internal)
  public static func capture(from context: some Context,
                             using memoryReader: some MemoryReader,
                             algorithm: UnwindAlgorithm = .auto,
                             limit: Int? = 64,
                             offset: Int = 0,
                             top: Int = 16) throws -> Backtrace {
    switch algorithm {
      // All of them, for now, use the frame pointer unwinder.  In the long
      // run, we should be using DWARF EH frame data for .precise.
      case .auto, .fast, .precise:
        let unwinder =
          FramePointerUnwinder(context: context, memoryReader: memoryReader)
          .dropFirst(offset)

        if let limit = limit {
          if limit == 0 {
            return Backtrace(frames: [.truncated])
          }

          let realTop = top < limit ? top : limit - 1
          var iterator = unwinder.makeIterator()
          var frames: [Frame] = []

          // Capture frames normally until we hit limit
          while let frame = iterator.next() {
            if frames.count < limit {
              frames.append(frame)
              if frames.count == limit {
                break
              }
            }
          }

          if realTop == 0 {
            if let _ = iterator.next() {
              // More frames than we were asked for; replace the last
              // one with a discontinuity
              frames[limit - 1] = .truncated
            }

            return Backtrace(frames: frames)
          } else {

            // If we still have frames at this point, start tracking the
            // last `realTop` frames in a circular buffer.
            if let frame = iterator.next() {
              let topSection = limit - realTop
              var topFrames: [Frame] = []
              var topNdx = 0
              var omittedFrames = 0

              topFrames.reserveCapacity(realTop)
              topFrames.insert(contentsOf: frames.suffix(realTop - 1), at: 0)
              topFrames.append(frame)

              while let frame = iterator.next() {
                topFrames[topNdx] = frame
                topNdx += 1
                omittedFrames += 1
                if topNdx >= realTop {
                  topNdx = 0
                }
              }

              // Fix the backtrace to include a discontinuity followed by
              // the contents of the circular buffer.
              let firstPart = realTop - topNdx
              let secondPart = topNdx
              frames[topSection - 1] = .omittedFrames(omittedFrames)

              frames.replaceSubrange(topSection..<(topSection+firstPart),
                                     with: topFrames.suffix(firstPart))
              frames.replaceSubrange((topSection+firstPart)..<limit,
                                     with: topFrames.prefix(secondPart))
            }

            return Backtrace(frames: frames)
          }
        } else {
          return Backtrace(frames: Array(unwinder))
        }
    }
  }

  /// Capture a list of the images currently mapped into the calling
  /// process.
  ///
  /// @returns A list of `Image`s.
  public static func captureImages() -> [Image] {
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    return captureImages(for: mach_task_self_)
    #else
    return []
    #endif
  }

  #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  private static func withDyldProcessInfo<T>(for task: task_t,
                                             fn: (OpaquePointer?) throws -> T)
    rethrows -> T {
    var kret: kern_return_t = KERN_SUCCESS
    let dyldInfo = _dyld_process_info_create(task, 0, &kret)

    if kret != KERN_SUCCESS {
      fatalError("error: cannot create dyld process info")
    }

    defer {
      _dyld_process_info_release(dyldInfo)
    }

    return try fn(dyldInfo)
  }
  #endif

  @_spi(Internal)
  public static func captureImages(for process: Any) -> [Image] {
    var images: [Image] = []

    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    let task = process as! task_t

    withDyldProcessInfo(for: task){ dyldInfo in
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

          _dyld_process_info_for_each_segment(dyldInfo, machHeaderAddress){
            address, size, name in

            if let name = String(validatingUTF8: name!), name == "__TEXT" {
              endOfText = address + size
            }
          }

          images.append(Image(name: name,
                              path: pathString,
                              buildID: theUUID,
                              baseAddress: machHeaderAddress,
                              endOfText: endOfText))
        }
      }
    }
    #endif // os(macOS) || os(iOS) || os(watchOS)

    return images.sorted(by: { $0.baseAddress < $1.baseAddress })
  }

  /// Capture shared cache information.
  ///
  /// @returns A `SharedCacheInfo`.
  public static func captureSharedCacheInfo() -> SharedCacheInfo {
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    return captureSharedCacheInfo(for: mach_task_self_)
    #else
    return SharedCacheInfo()
    #endif
  }

  @_spi(Internal)
  public static func captureSharedCacheInfo(for t: Any) -> SharedCacheInfo {
    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    let task = t as! task_t
    return withDyldProcessInfo(for: task){ dyldInfo in
      var cacheInfo = dyld_process_cache_info()
      _dyld_process_info_get_cache(dyldInfo, &cacheInfo)
      let theUUID = withUnsafePointer(to: cacheInfo.cacheUUID){
        $0.withMemoryRebound(to: UInt8.self,
                             capacity: MemoryLayout<uuid_t>.size) {
          Array(UnsafeBufferPointer(start: $0,
                                    count: MemoryLayout<uuid_t>.size))
        }
      }
      return SharedCacheInfo(uuid: theUUID,
                             baseAddress: cacheInfo.cacheBaseAddress,
                             noCache: cacheInfo.noCache)
    }
    #else // !os(Darwin)
    return SharedCacheInfo()
    #endif
  }

  /// Return a symbolicated version of the backtrace.
  ///
  /// @param images Specifies the set of images to use for symbolication.
  ///               If `nil`, the function will look to see if the `Backtrace`
  ///               has already captured images.  If it has, those will be
  ///               used; otherwise we will capture images at this point.
  ///
  /// @param sharedCacheInfo  Provides information about the location and
  ///                         identity of the shared cache, if applicable.
  ///
  /// @param showInlineFrames If `true` and we know how on the platform we're
  ///                         running on, add virtual frames to show inline
  ///                         function calls.
  ///
  /// @returns A new `SymbolicatedBacktrace`.
  public func symbolicated(with images: [Image]? = nil,
                           sharedCacheInfo: SharedCacheInfo? = nil,
                           showInlineFrames: Bool = true)
    -> SymbolicatedBacktrace? {
    return SymbolicatedBacktrace.symbolicate(backtrace: self,
                                             images: images,
                                             sharedCacheInfo: sharedCacheInfo,
                                             showInlineFrames: showInlineFrames)
  }

  /// Provide a textual version of the backtrace.
  public var description: String {
    var lines: [String] = []

    var n = 0
    for frame in frames {
      lines.append("\(n)\t\(frame)")
      switch frame {
        case let .omittedFrames(count):
          n += count
        default:
          n += 1
      }
    }

    if let images = images {
      lines.append("")
      lines.append("Images:")
      lines.append("")
      for (n, image) in images.enumerated() {
        lines.append("\(n)\t\(image)")
      }
    }

    #if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
    if let sharedCacheInfo = sharedCacheInfo {
      lines.append("")
      lines.append("Shared Cache:")
      lines.append("")
      lines.append("  UUID: \(hex(sharedCacheInfo.uuid))")
      lines.append("  Base: \(hex(sharedCacheInfo.baseAddress))")
    }
    #endif

    return lines.joined(separator: "\n")
  }
}
