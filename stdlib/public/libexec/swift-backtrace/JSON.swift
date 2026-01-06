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
@_spi(CrashLog) import Runtime

extension TargetThread {
  func getBacktrace<Address:FixedWidthInteger>() ->
  [CrashLog<Address>.Frame] {
    switch backtrace {
      case .raw(let bt):
        bt.frames.compactMap { CrashLog<Address>.Frame(fromFrame: $0) }
      case .symbolicated(let sbt):
        sbt.frames.compactMap { CrashLog<Address>.Frame(fromFrame: $0) }
    }
  }
}

extension CrashLog.Thread {
  init(backtraceThread: TargetThread, isCrashingThread: Bool) {
    self.init(
      name: backtraceThread.name,
      crashed: isCrashingThread,
      registers: [:],
      frames: backtraceThread.getBacktrace())
  }
}

extension SwiftBacktrace {
  static func captureCrashLog(
    imageMap: ImageMap,
    backtraceDuration: timespec) -> CrashLog<HostContext.Address>? {

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

    let architecture: String
    switch crashingThread.backtrace {
      case let .raw(backtrace):
        architecture = backtrace.architecture
      case let .symbolicated(backtrace):
        architecture = backtrace.architecture
    }

    let images =
      imageMap.images.map {
        CrashLog<HostContext.Address>.Image(fromImageMapImage: $0)
      }

    let backtraceTime = Double(backtraceDuration.tv_sec)
        + 1.0e-9 * Double(backtraceDuration.tv_nsec)

    let crashLogCapture =
      CrashLogCapture<HostContext.GPRValue>(memoryReader: target.reader)
    
    let threads = target.threads.map {
      var thread = CrashLog<HostContext.Address>.Thread(
        backtraceThread: $0,
        isCrashingThread: $0.id == target.crashingThread)

      if let context = $0.context {
        crashLogCapture.captureRegisterDump(context, into: &thread)
      }

      return thread
    }

    return CrashLog<HostContext.Address>(
      timestamp: formatISO8601(now),
      kind: "crashReport",
      description: description,
      faultAddress: hex(target.faultAddress),
      platform: target.images.platform,
      architecture: architecture,
      threads: threads,
      capturedMemory: crashLogCapture.capturedMemory,
      omittedImages: 0, // this will be calculated during write out
      images: images,
      backtraceTime: backtraceTime )
  }
}

struct SwiftBacktraceWriter: BacktraceJSONWriter {
  func write(_ string: String, flush: Bool) {
    SwiftBacktrace.write(string, flush: flush)
  }

  func writeln(_ string: String, flush: Bool) {
    SwiftBacktrace.writeln(string, flush: flush)
  }
}
