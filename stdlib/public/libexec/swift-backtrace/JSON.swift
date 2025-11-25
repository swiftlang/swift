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

public enum JSONOutputThreadBacktrace {
  case raw(Backtrace)
  case symbolicated(SymbolicatedBacktrace)
}

public protocol JSONOutputThread {
  associatedtype ThreadID: Equatable

  var id: ThreadID { get }
  var name: String { get }
  func getBacktrace() -> JSONOutputThreadBacktrace
}

extension TargetThread: JSONOutputThread {
  func getBacktrace() -> JSONOutputThreadBacktrace {
    switch backtrace {
      case .raw(let bt):
        return .raw(bt)
      case .symbolicated(let sbt):
        return .symbolicated(sbt)
    }
  }
}

public protocol JSONOutput {
  associatedtype ThreadType: JSONOutputThread

  static func write(_ string: String, flush: Bool) 
  static func writeln(_ string: String, flush: Bool)

  static func getDescription() -> String?
  static func getArchitecture() -> String?
  static func getPlatform() -> String?
  static func getFaultAddress() -> String?
  static func getShouldShowAllThreads() -> Bool
  static func getShouldShowAllRegisters() -> Bool
  static func getShouldDemangle() -> Bool
  static func getShouldSanitize() -> Bool
  static func getShowMentionedImages() -> Bool

  static func allThreads() -> [ThreadType]
  static func crashingThreadIndex() -> Int?
  static func writeThreadRegisters(thread: ThreadType, capturedBytes: inout [UInt64:Array<UInt8>])
  static func getImages() -> ImageMap?
}

extension SwiftBacktrace: JSONOutput {
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
      outputJSONRegisterDump(context) { addressValue in
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

extension JSONOutput {
    static func outputJSONRegister<T: FixedWidthInteger>(
      name: String, value: T, first: Bool = false,
      captureMemory: ((T) -> Void)
    ) {
      if !first {
        write(", ", flush: false)
      }
      write("\"\(name)\": \"\(hex(value))\"", flush: false)

      captureMemory(value)
    }

    static func outputJSONRegister<C: Context>(
      name: String, context: C, register: C.Register, first: Bool = false,
      captureMemory: ((C.GPRValue) -> Void)
    ) {
      let value = context.getRegister(register)!
      outputJSONRegister(name: name, value: value, first: first, captureMemory: captureMemory)
    }

    static func outputJSONGPRs<C: Context, Rs: Sequence>(_ context: C, range: Rs,
      captureMemory: ((C.GPRValue) -> Void))
      where Rs.Element == C.Register
    {
      var first = true
      for reg in range {
        outputJSONRegister(name: "\(reg)", context: context, register: reg,
                           first: first, captureMemory: captureMemory)
        if first {
          first = false
        }
      }
    }

    static func outputJSONRegisterDump(_ context: X86_64Context, captureMemory: ((X86_64Context.GPRValue) -> Void)) {
      outputJSONGPRs(context, range: .rax ... .r15, captureMemory: captureMemory)
      outputJSONRegister(name: "rip", value: context.programCounter, captureMemory: captureMemory)
      outputJSONRegister(name: "rflags", context: context, register: .rflags, captureMemory: captureMemory)
      outputJSONRegister(name: "cs", context: context, register: .cs, captureMemory: captureMemory)
      outputJSONRegister(name: "fs", context: context, register: .fs, captureMemory: captureMemory)
      outputJSONRegister(name: "gs", context: context, register: .gs, captureMemory: captureMemory)
    }

    static func outputJSONRegisterDump(_ context: I386Context, captureMemory: ((I386Context.GPRValue) -> Void)) {
      outputJSONGPRs(context, range: .eax ... .edi, captureMemory: captureMemory)
      outputJSONRegister(name: "eip", value: context.programCounter, captureMemory: captureMemory)
      outputJSONRegister(name: "eflags", context: context, register: .eflags, captureMemory: captureMemory)
      outputJSONRegister(name: "es", context: context, register: .es, captureMemory: captureMemory)
      outputJSONRegister(name: "cs", context: context, register: .cs, captureMemory: captureMemory)
      outputJSONRegister(name: "ss", context: context, register: .ss, captureMemory: captureMemory)
      outputJSONRegister(name: "ds", context: context, register: .ds, captureMemory: captureMemory)
      outputJSONRegister(name: "fs", context: context, register: .fs, captureMemory: captureMemory)
      outputJSONRegister(name: "gs", context: context, register: .gs, captureMemory: captureMemory)
    }

    static func outputJSONRegisterDump(_ context: ARM64Context, captureMemory: ((ARM64Context.GPRValue) -> Void)) {
      outputJSONGPRs(context, range: .x0 ..< .x29, captureMemory: captureMemory)
      outputJSONRegister(name: "fp", context: context, register: .x29, captureMemory: captureMemory)
      outputJSONRegister(name: "lr", context: context, register: .x30, captureMemory: captureMemory)
      outputJSONRegister(name: "sp", context: context, register: .sp, captureMemory: captureMemory)
      outputJSONRegister(name: "pc", context: context, register: .pc, captureMemory: captureMemory)
    }

    static func outputJSONRegisterDump(_ context: ARMContext, captureMemory: ((ARMContext.GPRValue) -> Void)) {
      outputJSONGPRs(context, range: .r0 ... .r10, captureMemory: captureMemory)
      outputJSONRegister(name: "fp", context: context, register: .r11, captureMemory: captureMemory)
      outputJSONRegister(name: "ip", context: context, register: .r12, captureMemory: captureMemory)
      outputJSONRegister(name: "sp", context: context, register: .r13, captureMemory: captureMemory)
      outputJSONRegister(name: "lr", context: context, register: .r14, captureMemory: captureMemory)
      outputJSONRegister(name: "pc", context: context, register: .r15, captureMemory: captureMemory)
    }
}

public extension JSONOutput {
  static func crashingThreadID() -> ThreadType.ThreadID? {
    guard let crashingThreadIndex = crashingThreadIndex() else { return nil }
    let crashingThread = allThreads()[crashingThreadIndex]
    return crashingThread.id
  }

  static func writePreamble(now: timespec) {
    guard let description = getDescription(), let faultAddress = getFaultAddress(),
    let platform = getPlatform(), let architecture = getArchitecture() else { return }

    write("""
        { \
        "timestamp": "\(formatISO8601(now))", \
        "kind": "crashReport", \
        "description": "\(escapeJSON(description))", \
        "faultAddress": "\(faultAddress)", \
        "platform": "\(escapeJSON(platform))", \
        "architecture": "\(escapeJSON(architecture))"
        """,
    flush: false)
  }

  static func writeThreads(
    mentionedImages: inout Set<Int>,
    capturedBytes: inout [UInt64:Array<UInt8>]) {

    write(#", "threads": [ "#, flush: false)
    if getShouldShowAllThreads() {
      let crashingThreadId = crashingThreadID()
      var first = true
      for (ndx, thread) in allThreads().enumerated() {
        if first {
          first = false
        } else {
          write(", ", flush: false)
        }

        writeThread(index: ndx,
          thread: thread,
          isCrashingThread: thread.id == crashingThreadId,
          mentionedImages: &mentionedImages,
          capturedBytes: &capturedBytes)
      }
    } else {
      if let crashingThreadIndex = crashingThreadIndex() {
        let crashingThread = allThreads()[crashingThreadIndex]
        writeThread(index: crashingThreadIndex,
                    thread: crashingThread,
                    isCrashingThread: true,
                    mentionedImages: &mentionedImages,
                    capturedBytes: &capturedBytes)
      }
    }

    write(" ]", flush: false)

    if !getShouldShowAllThreads() && allThreads().count > 1 {
      write(", \"omittedThreads\": \(allThreads().count - 1)", flush: false)
    }
  }

  static func writeThread(
    index: Int,
    thread: ThreadType,
    isCrashingThread: Bool,
    mentionedImages: inout Set<Int>,
    capturedBytes: inout [UInt64:Array<UInt8>]) {

      write("{ ", flush: false)

      if !thread.name.isEmpty {
        write("\"name\": \"\(escapeJSON(thread.name))\", ", flush: false)
      }
      if isCrashingThread {
        write(#""crashed": true, "#, flush: false)
      }
      if getShouldShowAllRegisters() || isCrashingThread {
        write(#""registers": {"#, flush: false)
        writeThreadRegisters(thread: thread, capturedBytes: &capturedBytes)
        write(" }, ", flush: false)
      }

      write(#""frames": ["#, flush: false)
      var first = true
      switch thread.getBacktrace() {
        case let .raw(backtrace):
          for frame in backtrace.frames {
            if first {
              first = false
            } else {
              write(",", flush: false)
            }

            write(" { \(frame.jsonBody) }", flush: false)
          }
        case let .symbolicated(backtrace):
          for frame in backtrace.frames {
            if first {
              first = false
            } else {
              write(",", flush: false)
            }

            write(" { ", flush: false)

            write(frame.captured.jsonBody, flush: false)

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
                      , "symbol": "\(escapeJSON(symbol.rawName))"\
                      , "offset": \(symbol.offset)
                      """, flush: false)

              if getShouldDemangle() {
                let formattedOffset: String
                if symbol.offset > 0 {
                  formattedOffset = " + \(symbol.offset)"
                } else if symbol.offset < 0 {
                  formattedOffset = " - \(symbol.offset)"
                } else {
                  formattedOffset = ""
                }

                write("""
                        , "description": \"\(escapeJSON(symbol.name))\(formattedOffset)\"
                        """, flush: false)
              }

              if symbol.imageIndex >= 0 {
                write(", \"image\": \"\(symbol.imageName)\"", flush: false)
              }

              if var sourceLocation = symbol.sourceLocation {
                if getShouldSanitize() {
                  sourceLocation.path = sanitizePath(sourceLocation.path)
                }
                write(#", "sourceLocation": { "#, flush: false)

                write("""
                        "file": "\(escapeJSON(sourceLocation.path))", \
                        "line": \(sourceLocation.line), \
                        "column": \(sourceLocation.column)
                        """, flush: false)

                write(" }", flush: false)
              }
            }
            write(" }", flush: false)
          }
      }
      write(" ] ", flush: false)

      write("}", flush: false)

      if getShowMentionedImages() {
        switch thread.getBacktrace() {
          case let .raw(backtrace):
            for frame in backtrace.frames {
              let address = frame.adjustedProgramCounter
              if let images = getImages(), let imageNdx = images.firstIndex(
                   where: { address >= $0.baseAddress
                              && address < $0.endOfText }
                 ) {
                mentionedImages.insert(imageNdx)
              }
            }
          case let .symbolicated(backtrace):
            for frame in backtrace.frames {
              if let symbol = frame.symbol, symbol.imageIndex >= 0 {
                mentionedImages.insert(symbol.imageIndex)
              }
            }
        }
      }    
  }  
}

extension SwiftBacktrace {

  static func outputJSONCrashLog() {
    guard let target = target else {
      print("swift-backtrace: unable to get target",
            to: &standardError)
      return
    }

    // let crashingThread = target.threads[target.crashingThreadNdx]
    // var mentionedImages = Set<Int>()
    // var capturedBytes: [UInt64:Array<UInt8>] = [:]



    var mentionedImages = Set<Int>()
    var capturedBytes: [UInt64:Array<UInt8>] = [:]

    writePreamble(now: now)

    writeThreads(mentionedImages: &mentionedImages, capturedBytes: &capturedBytes)

    if !capturedBytes.isEmpty && !(args.sanitize ?? false) {
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
