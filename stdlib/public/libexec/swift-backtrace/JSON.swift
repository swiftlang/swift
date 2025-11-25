//===----------------------------------------------------------------------===//
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

#if canImport(Darwin)
import Darwin
#elseif canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#elseif canImport(CRT)
import CRT
#endif

@_spi(Contexts) import Runtime
@_spi(Registers) import Runtime
@_spi(Formatting) import Runtime
@_spi(Internal) import Runtime
@_spi(MemoryReaders) import Runtime
@_spi(Utils) import Runtime

extension TargetThread: BacktraceJSONFormatterThread {
  func getBacktrace() -> BacktraceJSONFormatterBacktrace {
    switch backtrace {
      case .raw(let bt):
        return .raw(bt)
      case .symbolicated(let sbt):
        return .symbolicated(sbt)
    }
  }
}

extension SwiftBacktrace: BacktraceJSONFormatter {
  static func getDescription() -> String? {
    guard let target = target else {
      print("swift-backtrace: unable to get target",
            to: &standardError)
      return nil
    }

    let crashingThread = target.threads[target.crashingThreadNdx]

    let description: String

    if case let .symbolicated(symbolicated) = crashingThread.backtrace,
       let failure = symbolicated.swiftRuntimeFailure {
      description = failure
    } else {
      description = target.signalDescription
    }

    return description
  }

  static func getArchitecture() -> String? {
    guard let target else { return nil }

    let crashingThread = target.threads[target.crashingThreadNdx]

    let architecture: String
    switch crashingThread.backtrace {
      case let .raw(backtrace):
        architecture = backtrace.architecture
      case let .symbolicated(backtrace):
        architecture = backtrace.architecture
    }

    return architecture
  }

  static func getPlatform() -> String? {
    guard let target else { return nil }

    return target.images.platform
  }

  static func getFaultAddress() -> String? {
    guard let target else { return nil }

    return hex(target.faultAddress)
  }

  static func getShouldShowAllThreads() -> Bool {
    args.threads!
  }

  static func getShouldShowAllRegisters() -> Bool {
    args.registers! == .all
  }

  static func getShouldDemangle() -> Bool {
    args.demangle
  }

  static func getShouldSanitize() -> Bool {
    args.sanitize ?? false
  }

  static func getShowMentionedImages() -> Bool {
    args.showImages! == .mentioned
  }

  static func crashingThreadIndex() -> Int? {
    target?.crashingThreadNdx
  }

  static func allThreads() -> [TargetThread] {
    target?.threads ?? []
  }

  static func writeThreadRegisters(thread: TargetThread, capturedBytes: inout [UInt64:Array<UInt8>]) {
    if let context = thread.context {
      writeRegisterDump(context) { addressValue in
        if let bytes = try? target?.reader.fetch(
            from: RemoteMemoryReader.Address(addressValue),
            count: 16,
            as: UInt8.self) {
          capturedBytes[UInt64(truncatingIfNeeded: addressValue)] = bytes
        }
      }
    }
  }

  static func getImages() -> ImageMap? {
    target?.images
  }
}

extension SwiftBacktrace {
  static func outputJSONCrashLog() {
    guard let target = target else {
      print("swift-backtrace: unable to get target",
            to: &standardError)
      return
    }

    var mentionedImages = Set<Int>()
    var capturedBytes: [UInt64:Array<UInt8>] = [:]

    writePreamble(now: formatISO8601(now))

    writeThreads(mentionedImages: &mentionedImages, capturedBytes: &capturedBytes)

    if !capturedBytes.isEmpty && !getShouldSanitize() {
      write(#", "capturedMemory": {"#)
      var first = true
      for (address, bytes) in capturedBytes {
        let formattedBytes = bytes
          .map{ hex($0, withPrefix: false) }
          .joined(separator: "")
        if first {
          first = false
        } else {
          write(",")
        }
        write(" \"\(hex(address))\": \"\(formattedBytes)\"")
      }
      write(" }")
    }

    func outputJSONImage(_ image: Backtrace.Image, first: Bool) {
      if !first {
        write(", ")
      }

      write("{ ")

      if let name = image.name {
        write("\"name\": \"\(escapeJSON(name))\", ")
      }

      if let bytes = image.uniqueID {
        let buildID = hex(bytes)
        write("\"buildId\": \"\(buildID)\", ")
      }

      if var path = image.path {
        if args.sanitize ?? false {
          path = sanitizePath(path)
        }
        write("\"path\": \"\(path)\", ")
      }

      write("""
              "baseAddress": "\(image.baseAddress)", \
              "endOfText": "\(image.endOfText)"
              """)

      write(" }")
    }

    switch args.showImages! {
      case .none:
        break
      case .mentioned:
        let images = mentionedImages.sorted().map{ target.images[$0] }
        let omitted = target.images.count - images.count
        write(", \"omittedImages\": \(omitted), \"images\": [ ")
        var first = true
        for image in images {
          outputJSONImage(image, first: first)
          if first {
            first = false
          }
        }
        write(" ] ")
      case .all:
        write(#", "images": [ "#)
        var first = true
        for image in target.images {
          outputJSONImage(image, first: first)
          if first {
            first = false
          }
        }
        write(" ] ")
    }

    let secs = Double(backtraceDuration.tv_sec)
      + 1.0e-9 * Double(backtraceDuration.tv_nsec)

    write(", \"backtraceTime\": \(secs) ")

    writeln("}")
  }

}
