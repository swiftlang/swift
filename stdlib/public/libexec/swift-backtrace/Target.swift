//===--- Target.swift - Represents a process we are inspecting ------------===//
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
//  There are a lot of system specifics in this file!
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)

import Darwin
import Darwin.Mach

import _Backtracing
@_spi(Internal) import _Backtracing
@_spi(Contexts) import _Backtracing
@_spi(MemoryReaders) import _Backtracing

import _SwiftBacktracingShims

#if arch(x86_64)
typealias MContext = darwin_x86_64_mcontext
#elseif arch(arm64) || arch(arm64_32)
typealias MContext = darwin_arm64_mcontext
#else
#error("You need to define MContext for this architecture")
#endif

extension thread_extended_info {
  var pth_swiftName: String {
    withUnsafePointer(to: pth_name) { ptr in
      let len = strnlen(ptr, Int(MAXTHREADNAMESIZE))
      return ptr.withMemoryRebound(to: UInt8.self,
                                   capacity: Int(MAXTHREADNAMESIZE)) {
        u8ptr in
        return String(decoding: UnsafeBufferPointer(start: u8ptr, count: Int(len)),
                      as: UTF8.self)
      }
    }
  }
}

struct TargetThread {
  typealias ThreadID = UInt64

  var id: ThreadID
  var context: HostContext?
  var name: String
  var backtrace: SymbolicatedBacktrace
}

class Target {
  typealias Address = UInt64

  var pid: pid_t
  var name: String
  var signal: UInt64
  var faultAddress: Address
  var crashingThread: TargetThread.ThreadID

  var task: mach_port_t
  var images: [Backtrace.Image] = []
  var sharedCacheInfo: Backtrace.SharedCacheInfo

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

  var reader: RemoteMemoryReader

  var mcontext: MContext

  static func getParentTask() -> mach_port_t? {
    var ports: mach_port_array_t? = nil
    var portCount: mach_msg_type_number_t = 0

    // For some reason, we can't pass a task read port this way, but we
    // *can* pass the control port.  So do that and then ask for a read port
    // before immediately dropping the control port from this process.

    let kr = mach_ports_lookup(mach_task_self_, &ports, &portCount)
    if kr != KERN_SUCCESS {
      return nil
    }

    if let ports = ports, portCount != 0 {
      var taskPort: mach_port_t = 0
      let kr = task_get_special_port(ports[0], TASK_READ_PORT, &taskPort)
      if kr != KERN_SUCCESS {
        mach_port_deallocate(mach_task_self_, ports[0])
        return nil
      }
      mach_port_deallocate(mach_task_self_, ports[0])
      return taskPort
    } else {
      return nil
    }
  }

  static func getProcessName(pid: pid_t) -> String {
    let buffer = UnsafeMutableBufferPointer<UInt8>.allocate(capacity: 4096)
    defer {
      buffer.deallocate()
    }
    let ret = proc_name(pid, buffer.baseAddress, UInt32(buffer.count))
    if ret <= 0 {
      return "<unknown>"
    } else {
      return String(decoding: buffer[0..<Int(ret)], as: UTF8.self)
    }
  }

  init(crashInfoAddr: UInt64, limit: Int?, top: Int) {
    pid = getppid()
    if let parentTask = Self.getParentTask() {
      task = parentTask
    } else {
      print("swift-backtrace: couldn't fetch parent task")
      exit(1)
    }

    reader = RemoteMemoryReader(task: task)

    name = Self.getProcessName(pid: pid)

    let crashInfo: CrashInfo
    do {
      crashInfo = try reader.fetch(from: crashInfoAddr, as: CrashInfo.self)
    } catch {
      print("swift-backtrace: unable to fetch crash info.")
      exit(1)
    }

    crashingThread = crashInfo.crashing_thread
    signal = crashInfo.signal
    faultAddress = crashInfo.fault_address

    guard let mctx: MContext = try? reader.fetch(from: crashInfo.mctx,
                                                 as: MContext.self) else {
      print("swift-backtrace: unable to fetch mcontext.")
      exit(1)
    }

    mcontext = mctx

    images = Backtrace.captureImages(for: task)
    sharedCacheInfo = Backtrace.captureSharedCacheInfo(for: task)

    fetchThreads(limit: limit, top: top)
  }

  func fetchThreads(limit: Int?, top: Int) {
    var threadPorts: thread_act_array_t? = nil
    var threadCount: mach_msg_type_number_t = 0
    let kr = task_threads(task,
                          &threadPorts,
                          &threadCount)

    if kr != KERN_SUCCESS {
      print("swift-backtrace: failed to enumerate threads - \(kr)")
      exit(1)
    }

    guard let ports = threadPorts else {
      print("swift-backtrace: thread array is nil")
      exit(1)
    }

    for ndx in 0..<threadCount {
      var threadIdInfo: thread_identifier_info_data_t? = nil
      var kr = mach_thread_info(ports[Int(ndx)], THREAD_IDENTIFIER_INFO,
                                &threadIdInfo)
      if kr != KERN_SUCCESS {
        print("swift-backtrace: unable to get thread info for thread \(ndx) - \(kr)")
        exit(1)
      }

      guard let info = threadIdInfo else {
        print("swift-backtrace: thread info is nil")
        exit(1)
      }

      var extInfo = thread_extended_info_data_t()

      let threadName: String

      kr = mach_thread_info(ports[Int(ndx)],
                            THREAD_EXTENDED_INFO,
                            &extInfo)
      if kr == KERN_SUCCESS {
        threadName = extInfo.pth_swiftName
      } else {
        print("unable to fetch ext info \(kr)")
        threadName = ""
      }

      let ctx: HostContext

      if info.thread_id == crashingThread {
        ctx = HostContext.fromHostMContext(mcontext)
        crashingThreadNdx = Int(ndx)
      } else {
        guard let threadCtx = HostContext.fromHostThread(ports[Int(ndx)]) else {
          // This can happen legitimately, e.g. when looking at a Rosetta 2
          // process, where there are ARM64 threads AS WELL AS the x86_64 ones.
          continue
        }
        ctx = threadCtx
      }

      guard let backtrace = try? Backtrace.capture(from: ctx,
                                                   using: reader,
                                                   limit: limit,
                                                   top: top) else {
        print("unable to capture backtrace from context for thread \(ndx)")
        exit(1)
      }

      guard let symbolicated = backtrace.symbolicated(with: images,
                                                      sharedCacheInfo: sharedCacheInfo) else {
        print("unable to symbolicate backtrace from context for thread \(ndx)")
        exit(1)
      }

      threads.append(TargetThread(id: info.thread_id,
                                  context: ctx,
                                  name: threadName,
                                  backtrace: symbolicated))

      mach_port_deallocate(mach_task_self_, ports[Int(ndx)])
    }
  }

  public func redoBacktraces(limit: Int?, top: Int) {
    for (ndx, thread) in threads.enumerated() {
      guard let context = thread.context else {
        continue
      }

      guard let backtrace = try? Backtrace.capture(from: context,
                                                   using: reader,
                                                   limit: limit,
                                                   top: top) else {
        print("unable to capture backtrace from context for thread \(ndx)")
        continue
      }

      guard let symbolicated = backtrace.symbolicated(with: images,
                                                      sharedCacheInfo: sharedCacheInfo) else {
        print("unable to symbolicate backtrace from context for thread \(ndx)")
        continue
      }

      threads[ndx].backtrace = symbolicated
    }
  }

  public func withDebugger(_ body: () -> ()) throws {
    #if os(macOS)
    return try withTemporaryDirectory(pattern: "/tmp/backtrace.XXXXXXXX") {
      tmpdir in

      let cmdfile = "\(tmpdir)/lldb.command"
      guard let fp = fopen(cmdfile, "wt") else {
        throw PosixError(errno: errno)
      }
      if fputs("""
                 #!/bin/bash
                 clear
                 echo "Once LLDB has attached, return to the other window and press any key"
                 echo ""
                 xcrun lldb --attach-pid \(pid) -o c
                 """, fp) == EOF {
        throw PosixError(errno: errno)
      }
      if fclose(fp) != 0 {
        throw PosixError(errno: errno)
      }
      if chmod(cmdfile, S_IXUSR|S_IRUSR) != 0 {
        throw PosixError(errno: errno)
      }

      try spawn("/usr/bin/open", args: ["open", cmdfile])

      body()
    }
    #else
    print("""
            From another shell, please run

            lldb --attach-pid \(target.pid) -o c
            """)
    body()
    #endif
  }
}

private func mach_thread_info<T>(_ thread: thread_t,
                                 _ flavor: CInt,
                                 _ result: inout T) -> kern_return_t {
  var count: mach_msg_type_number_t
    = mach_msg_type_number_t(MemoryLayout<T>.stride
                               / MemoryLayout<natural_t>.stride)

  return withUnsafeMutablePointer(to: &result) { ptr in
    ptr.withMemoryRebound(to: natural_t.self, capacity: Int(count)) { intPtr in
      return thread_info(thread,
                         thread_flavor_t(flavor),
                         intPtr,
                         &count)
    }
  }
}

#endif // os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
