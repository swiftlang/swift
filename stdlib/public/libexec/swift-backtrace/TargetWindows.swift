//===--- TargetWindows.swift - Represents a process we are inspecting -----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022-2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Defines `Target`, which represents the process we are inspecting.
//  This is the Windows version.
//
//===----------------------------------------------------------------------===//

#if os(Windows)

import WinSDK
import CRT

import Runtime
@_spi(Internal) import Runtime
@_spi(Contexts) import Runtime
@_spi(MemoryReaders) import Runtime

internal import BacktracingImpl.OS.Windows

enum SomeBacktrace {
  case raw(Backtrace)
  case symbolicated(SymbolicatedBacktrace)
}

struct TargetThread {
  typealias ThreadID = DWORD

  var id: ThreadID
  var context: HostContext?
  var name: String
  var backtrace: SomeBacktrace
}

fileprivate func GetProcessImageFileName(_ hProcess: HANDLE) -> String? {
  return withUnsafeTemporaryAllocation(of: WCHAR.self, capacity: 2048) {
    buffer in

    let dwRet = K32GetProcessImageFileNameW(hProcess, buffer.baseAddress,
                                            DWORD(buffer.count))
    if dwRet == 0 {
      return nil
    }

    let slice = buffer[0..<Int(dwRet)]

    return String(decoding: slice, as: UTF16.self)
  }
}

fileprivate func GetThreadDescription(_ hThread: HANDLE) -> String? {
  var description: PWSTR? = nil

  let result = GetThreadDescription(hThread, &description)
  if result != 0 {
    return nil
  }

  return String.decodeCString(description!, as: UTF16.self)?.result
}

fileprivate func getProcessName(_ hProcess: HANDLE) -> String? {
  // According to Microsoft's documentation, using GetModuleBaseName()
  // for this is unreliable, and we should use GetProcessImageFileName()
  // instead, then process the returned path.
  //
  // See https://learn.microsoft.com/en-gb/windows/win32/api/psapi/nf-psapi-getmodulebasenamew

  guard let filename = GetProcessImageFileName(hProcess) else {
    return nil
  }

  if let ndx = filename.lastIndex(of: "\\"), ndx < filename.endIndex {
    return String(filename.suffix(from: filename.index(after: ndx)))
  } else {
    return filename
  }
}

class Target {
  typealias Address = UInt64

  var hProcess: HANDLE = INVALID_HANDLE_VALUE
  var hThreads: [HANDLE] = []

  var pid: DWORD
  var name: String
  var crashingThread: TargetThread.ThreadID
  var exceptionCode: DWORD
  var faultAddress: Address
  var exceptionInfo: Address

  var images: ImageMap

  var threads: [TargetThread] = []
  var crashingThreadNdx: Int = -1

  var signalDescription: String {
    switch exceptionCode {
      case EXCEPTION_ACCESS_VIOLATION: return "Access violation"
      case EXCEPTION_ARRAY_BOUNDS_EXCEEDED: return "Array bounds exceeded"
      case EXCEPTION_BREAKPOINT: return "Breakpoint"
      case EXCEPTION_DATATYPE_MISALIGNMENT: return "Data type misalignment"
      case EXCEPTION_FLT_DENORMAL_OPERAND: return "Denormal floating point"
      case EXCEPTION_FLT_INEXACT_RESULT: return "Inexact floating point result"
      case EXCEPTION_FLT_INVALID_OPERATION:
        return "Invalid floating point operation"
      case EXCEPTION_FLT_OVERFLOW: return "Floating point overflow"
      case EXCEPTION_FLT_STACK_CHECK: return "Floating point stack check failed"
      case EXCEPTION_FLT_UNDERFLOW: return "Floating point underflow"
      case EXCEPTION_GUARD_PAGE: return "Guard page"
      case EXCEPTION_ILLEGAL_INSTRUCTION: return "Illegal instruction"
      case EXCEPTION_IN_PAGE_ERROR: return "Page fault"
      case EXCEPTION_INT_DIVIDE_BY_ZERO: return "Integer divide by zero"
      case EXCEPTION_INT_OVERFLOW: return "Integer overflow"
      case EXCEPTION_INVALID_DISPOSITION: return "Invalid disposition"
      case EXCEPTION_INVALID_HANDLE: return "Invalid handle"
      case EXCEPTION_NONCONTINUABLE_EXCEPTION: return "Noncontinuable exception"
      case EXCEPTION_PRIV_INSTRUCTION: return "Privileged instruction"
      case EXCEPTION_SINGLE_STEP: return "Single step"
      case EXCEPTION_STACK_OVERFLOW: return "Stack overflow"
      default:
        return "Exception \(hex(exceptionCode))"
    }
  }

  var reader: RemoteMemoryReader

  init(pid: DWORD,
       crashInfoAddr: UInt64, limit: Int?, top: Int, cache: Bool,
       symbolicate: SwiftBacktrace.Symbolication) {
    self.pid = pid

    if !DebugActiveProcess(pid) {
      let error = GetLastError()
      print("swift-backtrace: unable to debug process: \(hex(error)).",
            to: &standardError)
      exit(1)
    }

    // Make sure that if we crash, the original process continues
    DebugSetProcessKillOnExit(false)

    var event = DEBUG_EVENT()

    // Process the debug events that tell us about the process
    while WaitForDebugEvent(&event, 0) {
      switch Int32(event.dwDebugEventCode) {
        case CREATE_PROCESS_DEBUG_EVENT:
          hProcess = event.u.CreateProcessInfo.hProcess
          hThreads.append(event.u.CreateProcessInfo.hThread)

        case CREATE_THREAD_DEBUG_EVENT:
          hThreads.append(event.u.CreateThread.hThread)
          SuspendThread(event.u.CreateThread.hThread)

        default:
          break
      }
    }

    if hProcess == INVALID_HANDLE_VALUE {
      print("swift-backtrace: didn't get a process handle.", to: &standardError)
      exit(1)
    }

    if hThreads.count < 1 {
      print("swift-backtrace: no threads.", to: &standardError)
      exit(1)
    }

    reader = RemoteMemoryReader(hProcess: UInt(bitPattern: hProcess))

    if let theName = getProcessName(hProcess) {
      name = theName
    } else {
      print("swift-backtrace: unable to fetch process name.", to: &standardError)
      exit(1)
    }

    let crashInfo: CrashInfo
    do {
      crashInfo = try reader.fetch(from: crashInfoAddr, as: CrashInfo.self)
    } catch {
      print("swift-backtrace: unable to fetch crash info.", to: &standardError)
      exit(1)
    }

    crashingThread = DWORD(truncatingIfNeeded: crashInfo.crashing_thread)
    exceptionCode = DWORD(crashInfo.signal)
    faultAddress = Address(truncatingIfNeeded: crashInfo.fault_address)
    exceptionInfo = Address(truncatingIfNeeded: crashInfo.exception_info)

    images = ImageMap.capture(for: UInt(bitPattern: hProcess))

    fetchThreads(limit: limit, top: top, cache: cache, symbolicate: symbolicate)
  }

  func fetchThreads(
    limit: Int?, top: Int, cache: Bool,
    symbolicate: SwiftBacktrace.Symbolication
  ) {
    for hThread in hThreads {
      let id = GetThreadId(hThread)
      let name = GetThreadDescription(hThread) ?? ""

      // Grab the NT context and convert to our type
      var ctx = CONTEXT()

      let context = withUnsafeMutablePointer(to: &ctx) {
        (lpctx: UnsafeMutablePointer<CONTEXT>) -> HostContext? in
        if id == crashingThread {
          do {
            let ppctx = exceptionInfo + Address(MemoryLayout<UInt>.size)
            let pctx = try reader.fetch(from: ppctx, as: UInt.self)
            lpctx.pointee = try reader.fetch(from: Address(pctx),
                                             as: CONTEXT.self)
          } catch {
            return nil
          }
        } else if !GetThreadContext(hThread, lpctx) {
          return nil
        }

        return HostContext(ntContext: UnsafeRawPointer(lpctx))
      }

      guard let context else {
        let err = GetLastError()
        print("swift-backtrace: failed to get thread context: \(hex(err))",
              to: &standardError)
        continue
      }

      guard let backtrace = try? Backtrace.capture(from: context,
                                                   using: reader,
                                                   images: images,
                                                   algorithm: .auto,
                                                   limit: limit,
                                                   offset: 0,
                                                   top: top) else {
        print("swift-backtrace: unable to capture backtrace from context for thread \(id)",
              to: &standardError)
        exit(1)
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

      if id == crashingThread {
        crashingThreadNdx = threads.count
      }

      if shouldSymbolicate {
        guard let symbolicated = backtrace.symbolicated(
                with: images,
                options: options
              ) else {
          print("unable to symbolicate backtrace from context for thread \(id)",
                to: &standardError)
          exit(1)
        }

        threads.append(TargetThread(id: id,
                                    context: context,
                                    name: name,
                                    backtrace: .symbolicated(symbolicated)))
      } else {
        threads.append(TargetThread(id: id,
                                    context: context,
                                    name: name,
                                    backtrace: .raw(backtrace)))
      }
    }
  }

  func redoBacktraces(
    limit: Int?, top: Int,
    cache: Bool,
    symbolicate: SwiftBacktrace.Symbolication
  ) {
    for (ndx, thread) in threads.enumerated() {
      guard let backtrace = try? Backtrace.capture(from: thread.context!,
                                                   using: reader,
                                                   images: images,
                                                   algorithm: .auto,
                                                   limit: limit,
                                                   offset: 0,
                                                   top: top) else {
        print("swift-backtrace: unable to capture backtrace from context for thread \(thread.id)",
              to: &standardError)
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
          print("swift-backtrace: unable to symbolicate backtrace from context for thread \(thread.id)",
                to: &standardError)
          continue
        }

        threads[ndx].backtrace = .symbolicated(symbolicated)
      } else {
        threads[ndx].backtrace = .raw(backtrace)
      }
    }
  }

  func withDebugger(_ body: () -> ()) throws {
    // ###TODO
    body()
  }
}

#endif // os(Windows)

