//===--- BacktraceJSONFormatter.swift -------------------------*- swift -*-===//
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
//  Provides functionality to format JSON backtraces.
//
//===----------------------------------------------------------------------===//

import Swift

@_spi(Formatting)
public struct BacktraceJSONFormatterOptions: OptionSet {
  public let rawValue: Int

  public init(rawValue: Int) {
    self.rawValue = rawValue
  }

  public static let allRegisters = BacktraceJSONFormatterOptions(rawValue: 1<<0)
  public static let demangle = BacktraceJSONFormatterOptions(rawValue: 1<<1)
  public static let sanitize = BacktraceJSONFormatterOptions(rawValue: 1<<2)
  public static let mentionedImages = BacktraceJSONFormatterOptions(rawValue: 1<<3)
  public static let allThreads = BacktraceJSONFormatterOptions(rawValue: 1<<4)
  public static let images = BacktraceJSONFormatterOptions(rawValue: 1<<5)
}

internal extension BacktraceJSONFormatterOptions {
  // this is not the form we want but will do for now
  // we are going to turn this into an OptionSet or similar
  var showAllRegisters: Bool { contains(.allRegisters) }
  var shouldDemangle: Bool { contains(.demangle) }
  var shouldSanitize: Bool { contains(.sanitize) }
  var showMentionedImages: Bool { contains(.mentionedImages) }
  var showAllThreads: Bool { contains(.allThreads) }
  var showImages: Bool { contains(.images) }
}

@_spi(Formatting)
public protocol BacktraceJSONWriter {
  func write(_ string: String, flush: Bool)
  func writeln(_ string: String, flush: Bool)
}

@_spi(Formatting)
@available(Backtracing 6.2, *)
public struct BacktraceJSONFormatter<
Address: FixedWidthInteger,
Writer: BacktraceJSONWriter> {

  var writer: Writer
  var options: BacktraceJSONFormatterOptions

  typealias Log = CrashLog<Address>

  var crashLog: Log
  var imageMap: ImageMap?

  var mentionedImages: Set<Int> = []

  @_spi(Formatting)
  public init(
    crashLog: CrashLog<Address>,
    writer: Writer,
    options: BacktraceJSONFormatterOptions)
  {
    self.crashLog = crashLog
    self.writer = writer
    self.options = options
    self.imageMap = crashLog.imageMap()
  }
}

@available(Backtracing 6.2, *)
internal extension BacktraceJSONFormatter {
  func write(_ string: String, flush: Bool) {
    writer.write(string, flush: flush)
  }
  func writeln(_ string: String, flush: Bool) {
    writer.writeln(string, flush: flush)
  }

  func getDescription() -> String? {
    crashLog.description
  }

  func getArchitecture() -> String? {
    crashLog.architecture
  }

  func getPlatform() -> String? {
    crashLog.platform
  }

  func getFaultAddress() -> String? {
    crashLog.faultAddress
  }
}

@_spi(Formatting)
@available(Backtracing 6.2, *)
public extension BacktraceJSONFormatter {
  mutating func writeCrashLog(now: String) {
    writePreamble(now: now)
    writeThreads()
    writeCapturedMemory()
    writeImages()
    writeFooter()
  }
}

@_spi(Formatting)
@available(Backtracing 6.2, *)
public extension BacktraceJSONFormatter {
  func writePreamble(now: String) {
    guard let description = getDescription(),
    let faultAddress = getFaultAddress(),
    let platform = getPlatform(),
    let architecture = getArchitecture() else { return }

    write("""
        { \
        "timestamp": "\(now)", \
        "kind": "crashReport", \
        "description": "\(escapeJSON(description))", \
        "faultAddress": "\(faultAddress)", \
        "platform": "\(escapeJSON(platform))", \
        "architecture": "\(escapeJSON(architecture))"
        """,
    flush: false)
  }

  // this updates the mentionedImages and omittedImages properties
  // it's done here rather than in capture for efficiency, as at that
  // point we are not sure if we are limiting to mentioned images or not
  mutating func writeThreads() {

    write(#", "threads": [ "#, flush: false)

    let threads = crashLog.threads.filter { options.showAllThreads || $0.crashed }

    var first = true
    for thread in threads {
      if first {
        first = false
      } else {
        write(", ", flush: false)
      }

      writeThread(thread: thread)
    }

    write(" ]", flush: false)

    if !options.showAllThreads && crashLog.threads.count > 1 {
      write(", \"omittedThreads\": \(crashLog.threads.count - 1)", flush: false)
    }

    // if omittedImages is nil, try to calculate it.
    if let images = crashLog.images,
      options.showMentionedImages,
      self.crashLog.omittedImages == nil {

      self.crashLog.omittedImages = images.count - mentionedImages.count

    }
  }

  func writeCapturedMemory() {
    // suppress writing captured memory if we should sanitize
    if !options.shouldSanitize, let capturedMemory = crashLog.capturedMemory {
      write(#", "capturedMemory": {"#, flush: false)
      var first = true
      for (address, bytes) in capturedMemory.sorted(by: <) {
        if first {
          first = false
        } else {
          write(",", flush: false)
        }
        write(" \"\(address)\": \"\(bytes)\"", flush: false)
      }
      write(" }", flush: false)
    }
  }

  // note: this updates the crash log with the ommitted image count
  func writeImages() {
    if options.showImages, let images = crashLog.images {
      var imagesToWrite = images

      if options.showMentionedImages {
        let mentioned =
          images.enumerated()
            .filter { mentionedImages.contains($0.0) }
            .map { $0.1 }

        imagesToWrite = mentioned
        
        write(", \"omittedImages\": \(self.crashLog.omittedImages ?? 0)",
        flush: false)
      }

      write(", \"images\": [ ", flush: false)
      var first = true
      for image in imagesToWrite {
        writeImage(image, first: first)
        if first {
          first = false
        }
      }
      write(" ] ", flush: false)
    }
  }

  func writeFooter() {
    write(", \"backtraceTime\": \(crashLog.backtraceTime) ", flush: false)

    write("}", flush: false)
  }
}

@available(Backtracing 6.2, *)
internal extension BacktraceJSONFormatter {
  func writeThreadRegisters(thread: Log.Thread) {
    guard let registers = thread.registers else { return }

    var first = true
    let registerOrder = HostContext.registerDumpOrder
    for registerName in registerOrder {
      if let value = registers[registerName] {
        if first {
          first = false
        } else {
          write(", ", flush: false)
        }

        write("\"\(registerName)\": \"\(value)\"", flush: false)
      }
    }
  }

  func imageByAddress(_ address: Address) -> Int? {
    guard let address = Backtrace.Address(address) else { return nil }
    return imageMap?.indexOfImage(at: address)
  }

  // note: this updates the mentioned images
  mutating func writeThread(
    thread: Log.Thread) {

      write("{ ", flush: false)

      if let name = thread.name, !name.isEmpty {
        write("\"name\": \"\(escapeJSON(name))\", ", flush: false)
      }

      let isCrashingThread = thread.crashed

      if isCrashingThread {
        write(#""crashed": true, "#, flush: false)
      }

      if options.showAllRegisters || isCrashingThread {
        write(#""registers": {"#, flush: false)
        writeThreadRegisters(thread: thread)
        write(" }, ", flush: false)
      }

      write(#""frames": ["#, flush: false)
      var first = true
      for frame in thread.frames {
        if first {
          first = false
        } else {
          write(",", flush: false)
        }

        write(" { ", flush: false)

        write(frame.jsonBody, flush: false)

        if frame.inlined {
          write(#", "inlined": true"#, flush: false)
        }
        if frame.isSwiftRuntimeFailure {
          write(#", "runtimeFailure": true"#, flush: false)
        }
        if frame.isSwiftThunk {
          write(#", "thunk": true"#, flush: false)
        }
        if frame.isSystem {
          write(#", "system": true"#, flush: false)
        }

        if let symbol = frame.symbol {
          write("""
                  , "symbol": "\(escapeJSON(symbol))"\
                  , "offset": \(frame.offset ?? 0)
                  """, flush: false)

          if options.shouldDemangle, let demangledName = frame.demangledName {
            let formattedOffset: String
            if (frame.offset ?? 0) > 0 {
              formattedOffset = " + \(frame.offset!)"
            } else if (frame.offset ?? 0) < 0 {
              formattedOffset = " - \(frame.offset!)"
            } else {
              formattedOffset = ""
            }

            write("""
                    , "description": \"\(escapeJSON(demangledName))\(formattedOffset)\"
                    """, flush: false)
          }

          if let image = frame.image {
            write(", \"image\": \"\(image)\"", flush: false)
          }

          if var sourceLocation = frame.sourceLocation {
            if options.shouldSanitize {
              sourceLocation.file = sanitizePath(sourceLocation.file)
            }
            write(#", "sourceLocation": { "#, flush: false)

            write("""
                    "file": "\(escapeJSON(sourceLocation.file))", \
                    "line": \(sourceLocation.line), \
                    "column": \(sourceLocation.column)
                    """, flush: false)

            write(" }", flush: false)
          }
        }
        write(" }", flush: false)
      }
      
      write(" ] ", flush: false)

      write("}", flush: false)

      if options.showMentionedImages {
        for frame in thread.frames {
          if let imageName = frame.image,
            let imageIndex = crashLog.images?
            .firstIndex(where: { $0.name == imageName }) {
            mentionedImages.insert(imageIndex)
          } else if let addressString = frame.address,
              let address = Log.addressFromString(addressString),
              let imageIndex = imageByAddress(address) {
            mentionedImages.insert(imageIndex)
          }
        }
      }    
  }

  func writeImage(_ image: Log.Image, first: Bool) {
    if !first {
      write(", ", flush: false)
    }

    write("{ ", flush: false)

    if let name = image.name {
      write("\"name\": \"\(escapeJSON(name))\", ", flush: false)
    }

    if let buildId = image.buildId {
      write("\"buildId\": \"\(buildId)\", ", flush: false)
    }

    if var path = image.path {
      if options.shouldSanitize {
        path = sanitizePath(path)
      }
      write("\"path\": \"\(path)\", ", flush: false)
    }

    write("""
            "baseAddress": "\(image.baseAddress)", \
            "endOfText": "\(image.endOfText)"
            """, flush: false)

    write(" }", flush: false)
  }
}
