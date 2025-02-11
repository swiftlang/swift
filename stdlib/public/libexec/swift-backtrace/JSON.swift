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

extension SwiftBacktrace {

  static func outputJSONCrashLog() {
    guard let target = target else {
      print("swift-backtrace: unable to get target",
            to: &standardError)
      return
    }

    let crashingThread = target.threads[target.crashingThreadNdx]

    let description: String

    if case let .symbolicated(symbolicated) = crashingThread.backtrace,
       let failure = symbolicated.swiftRuntimeFailure {
      description = failure
    } else {
      description = target.signalDescription
    }

    let architecture: String
    switch crashingThread.backtrace {
      case let .raw(backtrace):
        architecture = backtrace.architecture
      case let .symbolicated(backtrace):
        architecture = backtrace.architecture
    }

    write("""
            { \
            "timestamp": "\(formatISO8601(now))", \
            "kind": "crashReport", \
            "description": "\(escapeJSON(description))", \
            "faultAddress": "\(hex(target.faultAddress))", \
            "platform": "\(escapeJSON(target.images.platform))", \
            "architecture": "\(escapeJSON(architecture))"
            """)

    var mentionedImages = Set<Int>()
    var capturedBytes: [UInt64:Array<UInt8>] = [:]

    func outputJSONRegister<T: FixedWidthInteger>(
      name: String, value: T, first: Bool = false
    ) {
      if !first {
        write(", ")
      }
      write("\"\(name)\": \"\(hex(value))\"")

      if let bytes = try? target.reader.fetch(
           from: RemoteMemoryReader.Address(value),
           count: 16,
           as: UInt8.self) {
        capturedBytes[UInt64(truncatingIfNeeded: value)] = bytes
      }
    }

    func outputJSONRegister<C: Context>(
      name: String, context: C, register: C.Register, first: Bool = false
    ) {
      let value = context.getRegister(register)!
      outputJSONRegister(name: name, value: value, first: first)
    }

    func outputJSONGPRs<C: Context, Rs: Sequence>(_ context: C, range: Rs)
      where Rs.Element == C.Register
    {
      var first = true
      for reg in range {
        outputJSONRegister(name: "\(reg)", context: context, register: reg,
                           first: first)
        if first {
          first = false
        }
      }
    }

    func outputJSONRegisterDump(_ context: X86_64Context) {
      outputJSONGPRs(context, range: .rax ... .r15)
      outputJSONRegister(name: "rip", value: context.programCounter)
      outputJSONRegister(name: "rflags", context: context, register: .rflags)
      outputJSONRegister(name: "cs", context: context, register: .cs)
      outputJSONRegister(name: "fs", context: context, register: .fs)
      outputJSONRegister(name: "gs", context: context, register: .gs)
    }

    func outputJSONRegisterDump(_ context: I386Context) {
      outputJSONGPRs(context, range: .eax ... .edi)
      outputJSONRegister(name: "eip", value: context.programCounter)
      outputJSONRegister(name: "eflags", context: context, register: .eflags)
      outputJSONRegister(name: "es", context: context, register: .es)
      outputJSONRegister(name: "cs", context: context, register: .cs)
      outputJSONRegister(name: "ss", context: context, register: .ss)
      outputJSONRegister(name: "ds", context: context, register: .ds)
      outputJSONRegister(name: "fs", context: context, register: .fs)
      outputJSONRegister(name: "gs", context: context, register: .gs)
    }

    func outputJSONRegisterDump(_ context: ARM64Context) {
      outputJSONGPRs(context, range: .x0 ..< .x29)
      outputJSONRegister(name: "fp", context: context, register: .x29)
      outputJSONRegister(name: "lr", context: context, register: .x30)
      outputJSONRegister(name: "sp", context: context, register: .sp)
      outputJSONRegister(name: "pc", context: context, register: .pc)
    }

    func outputJSONRegisterDump(_ context: ARMContext) {
      outputJSONGPRs(context, range: .r0 ... .r10)
      outputJSONRegister(name: "fp", context: context, register: .r11)
      outputJSONRegister(name: "ip", context: context, register: .r12)
      outputJSONRegister(name: "sp", context: context, register: .r13)
      outputJSONRegister(name: "lr", context: context, register: .r14)
      outputJSONRegister(name: "pc", context: context, register: .r15)
    }

    func outputJSONThread(ndx: Int, thread: TargetThread) {
      write("{ ")

      if !thread.name.isEmpty {
        write("\"name\": \"\(escapeJSON(thread.name))\", ")
      }
      if thread.id == target.crashingThread {
        write(#""crashed": true, "#)
      }
      if args.registers! == .all || thread.id == target.crashingThread {
        if let context = thread.context {
          write(#""registers": {"#)
          outputJSONRegisterDump(context)
          write(" }, ")
        }
      }

      write(#""frames": ["#)
      var first = true
      switch thread.backtrace {
        case let .raw(backtrace):
          for frame in backtrace.frames {
            if first {
              first = false
            } else {
              write(",")
            }

            write(" { \(frame.jsonBody) }")
          }
        case let .symbolicated(backtrace):
          for frame in backtrace.frames {
            if first {
              first = false
            } else {
              write(",")
            }

            write(" { ")

            write(frame.captured.jsonBody)

            if frame.inlined {
              write(#", "inlined": true"#)
            }
            if frame.isSwiftRuntimeFailure {
              write(#", "runtimeFailure": true"#)
            }
            if frame.isSwiftThunk {
              write(#", "thunk": true"#)
            }
            if frame.isSystem {
              write(#", "system": true"#)
            }

            if let symbol = frame.symbol {
              write("""
                      , "symbol": "\(escapeJSON(symbol.rawName))"\
                      , "offset": \(symbol.offset)
                      """)

              if args.demangle {
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
                        """)
              }

              if symbol.imageIndex >= 0 {
                write(", \"image\": \"\(symbol.imageName)\"")
              }

              if var sourceLocation = symbol.sourceLocation {
                if args.sanitize ?? false {
                  sourceLocation.path = sanitizePath(sourceLocation.path)
                }
                write(#", "sourceLocation": { "#)

                write("""
                        "file": "\(escapeJSON(sourceLocation.path))", \
                        "line": \(sourceLocation.line), \
                        "column": \(sourceLocation.column)
                        """)

                write(" }")
              }
            }
            write(" }")
          }
      }
      write(" ] ")

      write("}")

      if args.showImages! == .mentioned {
        switch thread.backtrace {
          case let .raw(backtrace):
            for frame in backtrace.frames {
              let address = frame.adjustedProgramCounter
              if let imageNdx = target.images.firstIndex(
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

    write(#", "threads": [ "#)
    if args.threads! {
      var first = true
      for (ndx, thread) in target.threads.enumerated() {
        if first {
          first = false
        } else {
          write(", ")
        }
        outputJSONThread(ndx: ndx, thread: thread)
      }
    } else {
      outputJSONThread(ndx: target.crashingThreadNdx,
                       thread: crashingThread)
    }
    write(" ]")

    if !args.threads! && target.threads.count > 1 {
      write(", \"omittedThreads\": \(target.threads.count - 1)")
    }

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
