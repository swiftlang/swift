//===--- TargetLinux.swift - Represents a process we are inspecting -------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines `Target`, which represents the process we are inspecting.
//  This is the Linux version.
//
//===----------------------------------------------------------------------===//

#if os(Linux)

#if canImport(Glibc)
import Glibc
#elseif canImport(Musl)
import Musl
#endif

import Runtime
@_spi(Internal) import Runtime
@_spi(Contexts) import Runtime
@_spi(MemoryReaders) import Runtime
@_spi(Utils) import Runtime

enum SomeBacktrace {
  case raw(Backtrace)
  case symbolicated(SymbolicatedBacktrace)
}

struct TargetThread {
  typealias ThreadID = pid_t

  var id: ThreadID
  var context: HostContext?
  var name: String
  var backtrace: SomeBacktrace
}

class Target {
  typealias Address = UInt64

  var pid: pid_t
  var name: String
  var signal: UInt64
  var faultAddress: Address
  var crashingThread: TargetThread.ThreadID

  var images: ImageMap

  var threads: [TargetThread] = []
  var crashingThreadNdx: Int = -1

  var signalName: String {
    switch signal {
      case UInt64(SIGQUIT): return "SIGQUIT"
      case UInt64(SIGABRT): return "SIGABRT"
      case UInt64(SIGBUS):  return "SIGBUS"
      case UInt64(SIGFPE): return "SIGFPE"
      case UInt64(SIGILL): return "SIGILL"
      case UInt64(SIGSEGV): return "SIGSEGV"
      case UInt64(SIGTRAP): return "SIGTRAP"
      default: return "\(signal)"
    }
  }

  var signalDescription: String {
    switch signal {
      case UInt64(SIGQUIT): return "Terminated"
      case UInt64(SIGABRT): return "Aborted"
      case UInt64(SIGBUS): return "Bus error"
      case UInt64(SIGFPE): return "Floating point exception"
      case UInt64(SIGILL): return "Illegal instruction"
      case UInt64(SIGSEGV): return "Bad pointer dereference"
      case UInt64(SIGTRAP): return "System trap"
      default:
        return "Signal \(signal)"
    }
  }

  var reader: MemserverMemoryReader

  // Get the name of a process
  private static func getProcessName(pid: pid_t) -> String {
    let path = "/proc/\(pid)/comm"
    guard let name = readString(from: path) else {
      return ""
    }
    return String(stripWhitespace(name))
  }

  /// Get the name of a thread
  private func getThreadName(tid: Int64) -> String {
    let path = "/proc/\(pid)/task/\(tid)/comm"
    guard let name = readString(from: path) else {
      return ""
    }
    let trimmed = String(stripWhitespace(name))

    // Allow the main thread to use the process' name, but other
    // threads will have an empty name unless they've set the name
    // explicitly
    if trimmed == self.name && pid != tid {
      return ""
    }
    return trimmed
  }

  init(crashInfoAddr: UInt64, limit: Int?, top: Int, cache: Bool,
       symbolicate: SwiftBacktrace.Symbolication) {
    // fd #4 is reserved for the memory server
    let memserverFd: CInt = 4

    pid = getppid()
    reader = MemserverMemoryReader(fd: memserverFd)
    name = Self.getProcessName(pid: pid)

    let crashInfo: CrashInfo
    do {
      crashInfo = try reader.fetch(from: crashInfoAddr, as: CrashInfo.self)
    } catch {
      print("swift-backtrace: unable to fetch crash info.")
      exit(1)
    }

    crashingThread = TargetThread.ThreadID(crashInfo.crashing_thread)
    signal = crashInfo.signal
    faultAddress = crashInfo.fault_address

    images = ImageMap.capture(using: reader, forProcess: Int(pid))

    do {
      try fetchThreads(threadListHead: Address(crashInfo.thread_list),
                       limit: limit, top: top, cache: cache,
                       symbolicate: symbolicate)
    } catch {
      print("swift-backtrace: failed to fetch thread information: \(error)")
      exit(1)
    }
  }

  /// Fetch information about all of the process's threads; the crash_info
  /// structure contains a linked list of thread ucontexts, which may not
  /// include every thread.  In particular, if a thread was stuck in an
  /// uninterruptible wait, we won't have a ucontext for it.
  func fetchThreads(
    threadListHead: Address,
    limit: Int?, top: Int, cache: Bool,
    symbolicate: SwiftBacktrace.Symbolication
  ) throws {
    var next = threadListHead

    while next != 0 {
      let t = try reader.fetch(from: next, as: thread.self)

      next = t.next

      guard let ucontext
              = try? reader.fetch(from: t.uctx, as: ucontext_t.self) else {
	// This can happen if a thread is in an uninterruptible wait
        continue
      }

      let context = HostContext.fromHostMContext(ucontext.uc_mcontext)
      let backtrace = try Backtrace.capture(from: context,
                                            using: reader,
                                            images: images,
                                            algorithm: .auto,
                                            limit: limit,
                                            offset: 0,
                                            top: top)

      let shouldSymbolicate: Bool
      var options: Backtrace.SymbolicationOptions
      switch symbolicate {
        case .off:
          shouldSymbolicate = false
          options = []
        case .fast:
          shouldSymbolicate = true
          options = [ .showSourceLocations ]
        case .full:
          shouldSymbolicate = true
          options = [ .showInlineFrames, .showSourceLocations ]
      }

      if cache {
        options.insert(.useSymbolCache)
      }

      if shouldSymbolicate {
        guard let symbolicated = backtrace.symbolicated(
                with: images,
                options: options
              ) else {
          print("unable to symbolicate backtrace for thread \(t.tid)")
          exit(1)
        }

        threads.append(TargetThread(id: TargetThread.ThreadID(t.tid),
                                    context: context,
                                    name: getThreadName(tid: t.tid),
                                    backtrace: .symbolicated(symbolicated)))
      } else {
        threads.append(TargetThread(id: TargetThread.ThreadID(t.tid),
                                    context: context,
                                    name: getThreadName(tid: t.tid),
                                    backtrace: .raw(backtrace)))
      }
    }

    // Sort the threads by thread ID; the main thread always sorts
    // lower than any other.
    threads.sort {
      return $0.id == pid || ($1.id != pid && $0.id < $1.id)
    }

    // Find the crashing thread index
    if let ndx = threads.firstIndex(where: { $0.id == crashingThread }) {
      crashingThreadNdx = ndx
    } else {
      print("unable to find the crashing thread")
      exit(1)
    }
  }

  func redoBacktraces(
    limit: Int?, top: Int, cache: Bool,
    symbolicate: SwiftBacktrace.Symbolication
  ) {
    for (ndx, thread) in threads.enumerated() {
      guard let context = thread.context else {
        continue
      }

      guard let backtrace = try? Backtrace.capture(from: context,
                                                   using: reader,
                                                   images: images,
                                                   algorithm: .auto,
                                                   limit: limit,
                                                   offset: 0,
                                                   top: top) else {
        print("unable to capture backtrace from context for thread \(ndx)")
        continue
      }

      let shouldSymbolicate: Bool
      var options: Backtrace.SymbolicationOptions
      switch symbolicate {
        case .off:
          shouldSymbolicate = false
          options = []
        case .fast:
          shouldSymbolicate = true
          options = [ .showSourceLocations ]
        case .full:
          shouldSymbolicate = true
          options = [ .showInlineFrames, .showSourceLocations ]
      }

      if cache {
        options.insert(.useSymbolCache)
      }

      if shouldSymbolicate {
        guard let symbolicated = backtrace.symbolicated(
                with: images,
                options: options
              ) else {
          print("unable to symbolicate backtrace from context for thread \(ndx)")
          continue
        }

        threads[ndx].backtrace = .symbolicated(symbolicated)
      } else {
        threads[ndx].backtrace = .raw(backtrace)
      }
    }
  }

  func withDebugger(_ body: () -> ()) throws {
    print("""
            From another shell, please run

            lldb --attach-pid \(pid) -o c
            """)
    body()
  }
}

#endif // os(Linux)
