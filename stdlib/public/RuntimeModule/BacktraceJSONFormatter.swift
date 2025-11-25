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
public enum BacktraceJSONFormatterBacktrace {
  case raw(Backtrace)
  case symbolicated(SymbolicatedBacktrace)
}

@_spi(Formatting)
public protocol BacktraceJSONFormatterThread {
  associatedtype ThreadID: Equatable

  var id: ThreadID { get }
  var name: String { get }
  func getBacktrace() -> BacktraceJSONFormatterBacktrace
}

@_spi(Formatting)
public protocol BacktraceJSONFormatter {
  associatedtype ThreadType: BacktraceJSONFormatterThread

  static func write(_ string: String, flush: Bool) 
  static func writeln(_ string: String, flush: Bool)

  static func getDescription() -> String?
  static func getArchitecture() -> String?
  static func getPlatform() -> String?
  static func getFaultAddress() -> String?
  static func allThreads() -> [ThreadType]
  static func crashingThreadIndex() -> Int?
  static func writeThreadRegisters(thread: ThreadType, capturedBytes: inout [UInt64:Array<UInt8>])
  static func getImages() -> ImageMap?

  // settings for the output format
  static func getShouldShowAllThreads() -> Bool
  static func getShouldShowAllRegisters() -> Bool
  static func getShouldDemangle() -> Bool
  static func getShouldSanitize() -> Bool
  static func getShowMentionedImages() -> Bool
}

@_spi(Formatting)
public extension BacktraceJSONFormatter {
    static func writeRegister<T: FixedWidthInteger>(
      name: String, value: T, first: Bool = false,
      captureMemory: ((T) -> Void)
    ) {
      if !first {
        write(", ", flush: false)
      }
      write("\"\(name)\": \"\(hex(value))\"", flush: false)

      captureMemory(value)
    }

    static func writeRegister<C: Context>(
      name: String, context: C, register: C.Register, first: Bool = false,
      captureMemory: ((C.GPRValue) -> Void)
    ) {
      let value = context.getRegister(register)!
      writeRegister(name: name, value: value, first: first, captureMemory: captureMemory)
    }

    static func writeGPRs<C: Context, Rs: Sequence>(_ context: C, range: Rs,
      captureMemory: ((C.GPRValue) -> Void))
      where Rs.Element == C.Register
    {
      var first = true
      for reg in range {
        writeRegister(name: "\(reg)", context: context, register: reg,
                           first: first, captureMemory: captureMemory)
        if first {
          first = false
        }
      }
    }

    static func writeRegisterDump(_ context: X86_64Context, captureMemory: ((X86_64Context.GPRValue) -> Void)) {
      writeGPRs(context, range: .rax ... .r15, captureMemory: captureMemory)
      writeRegister(name: "rip", value: context.programCounter, captureMemory: captureMemory)
      writeRegister(name: "rflags", context: context, register: .rflags, captureMemory: captureMemory)
      writeRegister(name: "cs", context: context, register: .cs, captureMemory: captureMemory)
      writeRegister(name: "fs", context: context, register: .fs, captureMemory: captureMemory)
      writeRegister(name: "gs", context: context, register: .gs, captureMemory: captureMemory)
    }

    static func writeRegisterDump(_ context: I386Context, captureMemory: ((I386Context.GPRValue) -> Void)) {
      writeGPRs(context, range: .eax ... .edi, captureMemory: captureMemory)
      writeRegister(name: "eip", value: context.programCounter, captureMemory: captureMemory)
      writeRegister(name: "eflags", context: context, register: .eflags, captureMemory: captureMemory)
      writeRegister(name: "es", context: context, register: .es, captureMemory: captureMemory)
      writeRegister(name: "cs", context: context, register: .cs, captureMemory: captureMemory)
      writeRegister(name: "ss", context: context, register: .ss, captureMemory: captureMemory)
      writeRegister(name: "ds", context: context, register: .ds, captureMemory: captureMemory)
      writeRegister(name: "fs", context: context, register: .fs, captureMemory: captureMemory)
      writeRegister(name: "gs", context: context, register: .gs, captureMemory: captureMemory)
    }

    static func writeRegisterDump(_ context: ARM64Context, captureMemory: ((ARM64Context.GPRValue) -> Void)) {
      writeGPRs(context, range: .x0 ..< .x29, captureMemory: captureMemory)
      writeRegister(name: "fp", context: context, register: .x29, captureMemory: captureMemory)
      writeRegister(name: "lr", context: context, register: .x30, captureMemory: captureMemory)
      writeRegister(name: "sp", context: context, register: .sp, captureMemory: captureMemory)
      writeRegister(name: "pc", context: context, register: .pc, captureMemory: captureMemory)
    }

    static func writeRegisterDump(_ context: ARMContext, captureMemory: ((ARMContext.GPRValue) -> Void)) {
      writeGPRs(context, range: .r0 ... .r10, captureMemory: captureMemory)
      writeRegister(name: "fp", context: context, register: .r11, captureMemory: captureMemory)
      writeRegister(name: "ip", context: context, register: .r12, captureMemory: captureMemory)
      writeRegister(name: "sp", context: context, register: .r13, captureMemory: captureMemory)
      writeRegister(name: "lr", context: context, register: .r14, captureMemory: captureMemory)
      writeRegister(name: "pc", context: context, register: .r15, captureMemory: captureMemory)
    }
}

@_spi(Formatting)
public extension BacktraceJSONFormatter {
  static func crashingThreadID() -> ThreadType.ThreadID? {
    guard let crashingThreadIndex = crashingThreadIndex() else { return nil }
    let crashingThread = allThreads()[crashingThreadIndex]
    return crashingThread.id
  }

  static func writePreamble(now: String) {
    guard let description = getDescription(), let faultAddress = getFaultAddress(),
    let platform = getPlatform(), let architecture = getArchitecture() else { return }

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