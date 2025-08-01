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
//  Defines the `SymbolicatedBacktrace` struct that represents a captured
//  backtrace with symbols.
//
//===----------------------------------------------------------------------===//

import Swift

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
internal import BacktracingImpl.OS.Darwin
#endif

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)
internal import Darwin
#elseif os(Windows)
internal import ucrt
#elseif canImport(Glibc)
internal import Glibc
#elseif canImport(Musl)
internal import Musl
#endif
internal import BacktracingImpl.Runtime

/// A symbolicated backtrace
public struct SymbolicatedBacktrace: CustomStringConvertible {
  /// The `Backtrace` from which this was constructed
  public var backtrace: Backtrace

  /// Represents a location in source code.
  ///
  /// The information in this structure comes from compiler-generated
  /// debug information and may not correspond to the current state of
  /// the filesystem --- it might even hold a path that only works
  /// from an entirely different machine.
  public struct SourceLocation: CustomStringConvertible, Sendable, Hashable {
    /// The path of the source file.
    public var path: String

    /// The line number.
    public var line: Int

    /// The column number.
    public var column: Int

    /// Provide a textual description.
    public var description: String {
      if column > 0 && line > 0 {
        return "\(path):\(line):\(column)"
      } else if line > 0 {
        return "\(path):\(line)"
      } else {
        return path
      }
    }
  }

  /// Represents an individual frame in the backtrace.
  public struct Frame: CustomStringConvertible {
    /// The captured frame from the `Backtrace`.
    public var captured: Backtrace.Frame

    /// The result of doing a symbol lookup for this frame.
    public var symbol: Symbol?

    /// If `true`, then this frame was inlined
    public var inlined: Bool = false

    /// `true` if this frame represents a Swift runtime failure.
    public var isSwiftRuntimeFailure: Bool {
      symbol?.isSwiftRuntimeFailure ?? false
    }

    /// `true` if this frame represents a Swift thunk function.
    public var isSwiftThunk: Bool {
      symbol?.isSwiftThunk ?? false
    }

    /// `true` if this frame is a system frame.
    public var isSystem: Bool {
      symbol?.isSystemFunction ?? false
    }

    /// A textual description of this frame.
    public var description: String {
      if let symbol = symbol {
        let isInlined = inlined ? " [inlined]" : ""
        let isThunk = isSwiftThunk ? " [thunk]" : ""
        return "\(captured.description)\(isInlined)\(isThunk) \(symbol)"
      } else {
        return captured.description
      }
    }
  }

  /// Represents a symbol we've located
  public class Symbol: CustomStringConvertible {
    /// The index of the image in which the symbol for this address is located.
    public var imageIndex: Int

    /// The name of the image in which the symbol for this address is located.
    public var imageName: String

    /// The raw symbol name, before demangling.
    public var rawName: String

    /// The demangled symbol name.
    public lazy var name: String = demangleRawName()

    /// The offset from the symbol.
    public var offset: Int

    /// The source location, if available.
    public var sourceLocation: SourceLocation?

    /// True if this symbol represents a Swift runtime failure.
    public var isSwiftRuntimeFailure: Bool {
      guard let sourceLocation = sourceLocation else {
        return false
      }

      let symName: Substring
      if rawName.hasPrefix("_") {
        symName = rawName.dropFirst()
      } else {
        symName = rawName.dropFirst(0)
      }

      return symName.hasPrefix("Swift runtime failure: ")
        && sourceLocation.line == 0
        && sourceLocation.column == 0
        && sourceLocation.path.hasSuffix("<compiler-generated>")
    }

    /// True if this symbol is a Swift thunk function.
    public var isSwiftThunk: Bool {
      return _swift_backtrace_isThunkFunction(rawName)
    }

    private func maybeUnderscore(_ sym: String) -> String {
      #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
      return "_" + sym
      #else
      return sym
      #endif
    }

    private func dylibName(_ dylib: String) -> String {
      #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
      return dylib + ".dylib"
      #else
      return dylib + ".so"
      #endif
    }

    /// True if this symbol represents a system function.
    ///
    /// For instance, the `start` function from `dyld` on macOS is a system
    /// function, and we don't need to display it under normal circumstances.
    public var isSystemFunction: Bool {
      #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
      if rawName == "start" && imageName == "dyld" {
        return true
      }
      #endif
      if rawName.hasSuffix("5$mainyyFZ")
           || rawName.hasSuffix("5$mainyyYaFZTQ0_")
           || rawName == maybeUnderscore("async_MainTQ0_") {
        return true
      }
      if rawName == maybeUnderscore("_ZL23completeTaskWithClosurePN5swift12AsyncContextEPNS_10SwiftErrorE") && imageName == dylibName("libswift_Concurrency") {
        return true
      }
      if let location = sourceLocation,
         ((location.line == 0 && location.column == 0)
            || location.path.hasSuffix("<compiler-generated>"))
           && !_swift_backtrace_isThunkFunction(rawName) {
        return true
      }
      return false
    }

    /// Construct a new Symbol.
    public init(imageIndex: Int, imageName: String,
                rawName: String, offset: Int, sourceLocation: SourceLocation?) {
      self.imageIndex = imageIndex
      self.imageName = imageName
      self.rawName = rawName
      self.offset = offset
      self.sourceLocation = sourceLocation
    }

    /// Demangle the raw name, if possible.
    private func demangleRawName() -> String {
      var length: size_t = 0
      if let demangled = _swift_backtrace_demangle(rawName, rawName.utf8.count,
                                                   nil, &length) {
        defer { free(demangled) }

        // length is the size of the buffer that was allocated, *not* the
        // length of the string.
        let stringLen = strlen(demangled)
        if stringLen > 0 {
          return demangled.withMemoryRebound(to: UInt8.self,
                                             capacity: stringLen) {
            let demangledBytes = UnsafeBufferPointer<UInt8>(start: $0,
                                                            count: stringLen)
            return String(decoding: demangledBytes, as: UTF8.self)
          }
        }
      }
      return rawName
    }

    /// A textual description of this symbol.
    public var description: String {
      let symPlusOffset: String

      if offset > 0 {
        symPlusOffset = "\(name) + \(offset)"
      } else if offset < 0 {
        symPlusOffset = "\(name) - \(-offset)"
      } else {
        symPlusOffset = name
      }

      let location: String
      if let sourceLocation = sourceLocation {
        location = " at \(sourceLocation)"
      } else {
        location = ""
      }

      return "[\(imageIndex)] \(imageName) \(symPlusOffset)\(location)"
    }
  }

  /// The architecture on which this backtrace was captured.
  public var architecture: String { return backtrace.architecture }

  /// A list of captured frame information.
  public private(set) var frames: [Frame]

  /// A list of images found in the process.
  public private(set) var images: ImageMap

  /// True if this backtrace is a Swift runtime failure.
  public var isSwiftRuntimeFailure: Bool {
    guard let frame = frames.first else { return false }
    return frame.isSwiftRuntimeFailure
  }

  /// If this backtrace is a Swift runtime failure, return the description.
  public var swiftRuntimeFailure: String? {
    guard let frame = frames.first else { return nil }
    if !frame.isSwiftRuntimeFailure { return nil }

    let symbolName = frame.symbol!.rawName
    if symbolName.hasPrefix("_") {
      return String(symbolName.dropFirst())
    }
    return symbolName
  }

  /// Construct a SymbolicatedBacktrace from a backtrace and a list of images.
  private init(backtrace: Backtrace, images: ImageMap, frames: [Frame]) {
    self.backtrace = backtrace
    self.images = images
    self.frames = frames
  }

  #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
  /// Convert a build ID to a CFUUIDBytes.
  private static func uuidBytesFromBuildID(_ buildID: [UInt8])
    -> CFUUIDBytes {
    return withUnsafeTemporaryAllocation(of: CFUUIDBytes.self,
                                         capacity: 1) { buf in
      buf.withMemoryRebound(to: UInt8.self) {
        _ = $0.initialize(from: buildID)
      }
      return buf[0]
    }
  }

  /// Create a symbolicator.
  private static func withSymbolicator<T>(images: ImageMap,
                                          useSymbolCache: Bool,
                                          fn: (CSSymbolicatorRef) throws -> T) rethrows -> T {
    let binaryImageList = images.map{ image in
      BinaryImageInformation(
        base: vm_address_t(image.baseAddress)!,
        extent: vm_address_t(image.endOfText)!,
        uuid: uuidBytesFromBuildID(image.uniqueID!),
        arch: HostContext.coreSymbolicationArchitecture,
        path: image.path ?? "",
        relocations: [
          BinaryRelocationInformation(
            base: vm_address_t(image.baseAddress)!,
            extent: vm_address_t(image.endOfText)!,
            name: "__TEXT"
          )
        ],
        flags: 0
      )
    }

    let symbolicator = CSSymbolicatorCreateWithBinaryImageList(
      binaryImageList,
      useSymbolCache ? 0 : kCSSymbolicatorDisallowDaemonCommunication,
      nil
    )

    defer { CSRelease(symbolicator) }

    return try fn(symbolicator)
  }

  /// Generate a frame from a symbol and source info pair
  private static func buildFrame(from capturedFrame: Backtrace.Frame,
                                 with owner: CSSymbolOwnerRef,
                                 isInline: Bool,
                                 symbol: CSSymbolRef,
                                 sourceInfo: CSSourceInfoRef?,
                                 images: ImageMap) -> Frame {
    if CSIsNull(symbol) {
      return Frame(captured: capturedFrame, symbol: nil)
    }

    let address = capturedFrame.originalProgramCounter
    let rawName = CSSymbolGetMangledName(symbol) ?? "<unknown>"
    let name = CSSymbolGetName(symbol) ?? rawName
    let range = CSSymbolGetRange(symbol)

    let location: SourceLocation?

    if let sourceInfo = sourceInfo, !CSIsNull(sourceInfo) {
      let path = CSSourceInfoGetPath(sourceInfo) ?? "<unknown>"
      let line = CSSourceInfoGetLineNumber(sourceInfo)
      let column = CSSourceInfoGetColumn(sourceInfo)

      location = SourceLocation(
        path: path,
        line: Int(line),
        column: Int(column)
      )
    } else {
      location = nil
    }

    let imageBase = CSSymbolOwnerGetBaseAddress(owner)
    var imageIndex = -1
    var imageName = ""
    for (ndx, image) in images.enumerated() {
      if vm_address_t(image.baseAddress) == imageBase {
        imageIndex = ndx
        imageName = image.name ?? "<unknown>"
        break
      }
    }

    let theSymbol = Symbol(imageIndex: imageIndex,
                           imageName: imageName,
                           rawName: rawName,
                           offset: Int(UInt64(address)! - UInt64(range.location)),
                           sourceLocation: location)
    theSymbol.name = name

    return Frame(captured: capturedFrame, symbol: theSymbol, inlined: isInline)
  }
  #endif

  /// Actually symbolicate.
  internal static func symbolicate(backtrace: Backtrace,
                                   images: ImageMap?,
                                   options: Backtrace.SymbolicationOptions)
    -> SymbolicatedBacktrace? {

    let theImages: ImageMap
    if let images = images {
      theImages = images
    } else if let images = backtrace.images {
      theImages = images
    } else {
      theImages = ImageMap.capture()
    }

    var frames: [Frame] = []

    #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
    withSymbolicator(images: theImages,
                     useSymbolCache: options.contains(.useSymbolCache)) {
      symbolicator in
      for frame in backtrace.frames {
        switch frame {
          case .omittedFrames(_), .truncated:
            frames.append(Frame(captured: frame, symbol: nil))
          default:
            let address = vm_address_t(frame.adjustedProgramCounter)!
            let owner
              = CSSymbolicatorGetSymbolOwnerWithAddressAtTime(symbolicator,
                                                              address,
                                                              kCSBeginningOfTime)

            if CSIsNull(owner) {
              frames.append(Frame(captured: frame, symbol: nil))
            } else if options.contains(.showInlineFrames) {
              // These present in *reverse* order (i.e. the real one first,
              // then the inlined frames from callee to caller).
              let pos = frames.count
              var first = true

              _ = CSSymbolOwnerForEachStackFrameAtAddress(owner, address) {
                symbol, sourceInfo in

                frames.insert(buildFrame(from: frame,
                                         with: owner,
                                         isInline: !first,
                                         symbol: symbol,
                                         sourceInfo: sourceInfo,
                                         images: theImages),
                              at: pos)

                first = false
              }
            } else if options.contains(.showSourceLocations) {
              let symbol = CSSymbolOwnerGetSymbolWithAddress(owner, address)
              let sourceInfo = CSSymbolOwnerGetSourceInfoWithAddress(owner,
                                                                     address)

              frames.append(buildFrame(from: frame,
                                       with: owner,
                                       isInline: false,
                                       symbol: symbol,
                                       sourceInfo: sourceInfo,
                                       images: theImages))
            } else {
              let symbol = CSSymbolOwnerGetSymbolWithAddress(owner, address)

              frames.append(buildFrame(from: frame,
                                       with: owner,
                                       isInline: false,
                                       symbol: symbol,
                                       sourceInfo: nil,
                                       images: theImages))
            }
        }
      }
    }
    #elseif os(Linux)
    let cache = ElfImageCache.threadLocal

    // This could be more efficient; at the moment we execute the line
    // number programs once per frame, whereas we could just run them once
    // for all the addresses we're interested in.

    for frame in backtrace.frames {
      let address = frame.adjustedProgramCounter
      if let imageNdx = theImages.indexOfImage(at: address) {
        let name = theImages[imageNdx].name ?? "<unknown>"
        var symbol: Symbol = Symbol(imageIndex: imageNdx,
                                    imageName: name,
                                    rawName: "<unknown>",
                                    offset: 0,
                                    sourceLocation: nil)

        func lookupSymbol<ElfImage: ElfSymbolLookupProtocol>(
          image: ElfImage?,
          at imageNdx: Int,
          named name: String,
          address imageAddr: ImageSource.Address
        ) -> Symbol? {
          let address = ElfImage.Traits.Address(imageAddr)

          guard let image = image else {
            return nil
          }
          guard let theSymbol = image.lookupSymbol(address: address) else {
            return nil
          }

          var location: SourceLocation?

          if options.contains(.showSourceLocations)
               || options.contains(.showInlineFrames) {
            location = try? image.sourceLocation(for: address)
          } else {
            location = nil
          }

          if options.contains(.showInlineFrames) {
            for inline in image.inlineCallSites(at: address) {
              let fakeSymbol = Symbol(imageIndex: imageNdx,
                                      imageName: name,
                                      rawName: inline.rawName ?? "<unknown>",
                                      offset: 0,
                                      sourceLocation: location)
              frames.append(Frame(captured: frame,
                                  symbol: fakeSymbol,
                                  inlined: true))

              location = SourceLocation(path: inline.filename,
                                        line: inline.line,
                                        column: inline.column)
            }
          }

          return Symbol(imageIndex: imageNdx,
                        imageName: name,
                        rawName: theSymbol.name,
                        offset: theSymbol.offset,
                        sourceLocation: location)
        }

        if let hit = cache.lookup(path: theImages[imageNdx].path) {
          switch hit {
            case let .elf32Image(image):
              let relativeAddress = ImageSource.Address(
                address - theImages[imageNdx].baseAddress
              ) + image.imageBase
              if let theSymbol = lookupSymbol(image: image,
                                              at: imageNdx,
                                              named: name,
                                              address: relativeAddress) {
                symbol = theSymbol
              }
            case let .elf64Image(image):
              let relativeAddress = ImageSource.Address(
                address - theImages[imageNdx].baseAddress
              ) + image.imageBase
              if let theSymbol = lookupSymbol(image: image,
                                              at: imageNdx,
                                              named: name,
                                              address: relativeAddress) {
                symbol = theSymbol
              }
          }
        }

        frames.append(Frame(captured: frame, symbol: symbol))
        continue
      }

      frames.append(Frame(captured: frame, symbol: nil))
    }
    #else
    frames = backtrace.frames.map{ Frame(captured: $0, symbol: nil) }
    #endif

    return SymbolicatedBacktrace(backtrace: backtrace,
                                 images: theImages,
                                 frames: frames)
  }

  /// Provide a textual version of the backtrace.
  public var description: String {
    var lines: [String] = []

    var n = 0
    for frame in frames {
      lines.append("\(n)\t\(frame.description)")
      switch frame.captured {
        case let .omittedFrames(count):
          n += count
        default:
          n += 1
      }
    }

    lines.append("")
    lines.append("Images:")
    lines.append("")
    for (n, image) in images.enumerated() {
      lines.append("\(n)\t\(image.description)")
    }

    return lines.joined(separator: "\n")
  }
}
