//===--- Win32Unwinder.swift ----------------------------------*- swift -*-===//
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
//  Unwind the stack by using the StackWalk64 API.
//
//===----------------------------------------------------------------------===//

#if os(Windows)

import Swift
internal import WinSDK

// We use this protocol to work around the problem of not being able to
// pass a function as a C function if it has captured generic parameters.
protocol Win32UnwinderSupport {
  func readProcessMemory(
    qwBaseAddress: DWORD64,
    lpBuffer: UnsafeMutableRawPointer?,
    nSize: DWORD,
    lpNumberOfBytesRead: UnsafeMutablePointer<DWORD>?
  ) -> WindowsBool

  func getModuleBase(address: DWORD64) -> DWORD64

  func getFunctionTableEntry(addrBase: DWORD64) -> UnsafeMutableRawPointer?
}

@_spi(Unwinders)
public struct Win32Unwinder<C: Win32Context, M: MemoryReader>
  : Sequence, IteratorProtocol
{
  public typealias Context = C
  public typealias MemoryReader = M
  public typealias Address = C.Address

  var context: Context
  var ntContext: UnsafeMutableRawPointer
  var machineType: UInt32
  var stackFrame: STACKFRAME64

  var first: Bool
  var done: Bool

  var images: ImageMap

  var reader: MemoryReader

  public init(
    context: Context,
    ntContext: UnsafeMutableRawPointer,
    images: ImageMap,
    memoryReader: MemoryReader) {
    self.first = true
    self.done = false
    self.images = images
    self.context = context
    self.ntContext = ntContext
    self.machineType = context.win32MachineType
    self.reader = memoryReader

    self.stackFrame = STACKFRAME64()
    self.stackFrame.AddrPC = ADDRESS64(Offset: DWORD64(context.programCounter),
                                       Segment: 0, Mode: AddrModeFlat)
    self.stackFrame.AddrFrame = ADDRESS64(Offset: DWORD64(context.framePointer),
                                          Segment: 0, Mode: AddrModeFlat)
    self.stackFrame.AddrStack = ADDRESS64(Offset: DWORD64(context.stackPointer),
                                          Segment: 0, Mode: AddrModeFlat)
  }

  public mutating func next() -> RichFrame<Address>? {
    if done {
      return nil
    }

    // ###TODO: Async frame support

    // We would like to avoid using the symbol helper API as it isn't thread
    // safe.  Since we already have most of the required code to implement the
    // functionality ourselves, do that instead.
    func readProcessMemoryThunk(
      hProcess: HANDLE?,
      qwBaseAddress: DWORD64,
      lpBuffer: UnsafeMutableRawPointer?,
      nSize: DWORD,
      lpNumberOfBytesRead: UnsafeMutablePointer<DWORD>?
    ) -> WindowsBool {
      guard let hProcess else {
        return false
      }
      let walker = hProcess.assumingMemoryBound(
        to: (any Win32UnwinderSupport).self
      )

      #if DEBUG_WIN32_UNWINDER
      print("Read memory at \(hex(qwBaseAddress)) of size \(nSize)")
      #endif

      let result = walker.pointee.readProcessMemory(
        qwBaseAddress: qwBaseAddress,
        lpBuffer: lpBuffer,
        nSize: nSize,
        lpNumberOfBytesRead: lpNumberOfBytesRead
      )

      #if DEBUG_WIN32_UNWINDER
      print("-> Result \(result)")
      #endif

      return result
    }

    func getModuleBaseThunk(hProcess: HANDLE?, address: DWORD64) -> DWORD64 {
      guard let hProcess else {
        return 0
      }
      let walker = hProcess.assumingMemoryBound(
        to: (any Win32UnwinderSupport).self
      )

      let result = walker.pointee.getModuleBase(address: address)

      #if DEBUG_WIN32_UNWINDER
      print("Module for \(hex(address)) is based at \(hex(result))")
      #endif

      return result
    }

    func getFunctionTableEntryThunk(
      hProcess: HANDLE?, addrBase: DWORD64
    ) -> UnsafeMutableRawPointer? {
      guard let hProcess else {
        return nil
      }
      let walker = hProcess.assumingMemoryBound(
        to: (any Win32UnwinderSupport).self
      )

      let ptr = walker.pointee.getFunctionTableEntry(addrBase: addrBase)

      #if DEBUG_WIN32_UNWINDER
      if let somePtr = ptr {
        print("Function table for \(hex(addrBase)) is at \(somePtr)")
      } else {
        print("Function table for \(hex(addrBase)) not found")
      }
      #endif

      return ptr
    }

    let unwinder: any Win32UnwinderSupport = self
    if !(withUnsafePointer(to: unwinder) { ptr in
           StackWalk64(DWORD(truncatingIfNeeded: machineType),
                       HANDLE(mutating: ptr),
                       HANDLE(mutating: ptr),
                       &stackFrame,
                       ntContext,
                       readProcessMemoryThunk,
                       getFunctionTableEntryThunk,
                       getModuleBaseThunk,
                       nil)
         }) {
      #if DEBUG_WIN32_UNWINDER
      print("StackWalk64 returned false")
      #endif
      done = true
      return nil
    }

    if first {
      first = false
      #if DEBUG_WIN32_UNWINDER
      print("Initial PC returned \(hex(Address(stackFrame.AddrPC.Offset)))")
      #endif
      return .programCounter(Address(stackFrame.AddrPC.Offset))
    }

    #if DEBUG_WIN32_UNWINDER
    print("StackWalk64 returned \(hex(Address(stackFrame.AddrPC.Offset)))")
    #endif

    return .returnAddress(Address(stackFrame.AddrPC.Offset))
  }
}

extension Win32Unwinder: Win32UnwinderSupport {
  func readProcessMemory(
    qwBaseAddress: DWORD64,
    lpBuffer: UnsafeMutableRawPointer?,
    nSize: DWORD,
    lpNumberOfBytesRead: UnsafeMutablePointer<DWORD>?
  ) -> WindowsBool {
    guard let lpBuffer else {
      return false
    }

    let buffer = UnsafeMutableRawBufferPointer(start: lpBuffer,
                                               count: Int(nSize))
    do {
      try self.reader.fetch(from: M.Address(qwBaseAddress),
                            into: buffer)
      if let lpNumberOfBytesRead {
        lpNumberOfBytesRead.pointee = DWORD(buffer.count)
      }
      return true
    } catch {
      if let lpNumberOfBytesRead {
        lpNumberOfBytesRead.pointee = 0
      }
      return false
    }
  }

  func getModuleBase(address: DWORD64) -> DWORD64 {
    if let ndx = self.images.indexOfImage(
         at: Backtrace.Address(address)!
       ) {
      return DWORD64(self.images[ndx].baseAddress)!
    }

    return 0
  }

  func getFunctionTableEntry(addrBase: DWORD64) -> UnsafeMutableRawPointer? {
    guard let ndx = self.images.indexOfImage(
            at: Backtrace.Address(addrBase)!
          ) else {
      return nil
    }

    let addrOffset = addrBase - DWORD64(self.images[ndx].baseAddress)!

    switch self.images.exceptionTable(at: ndx) {
    case .arm64(let wrapper):
      // Search the IMAGE_ARM64_RUNTIME_FUNCTION_ENTRY table
      let table = wrapper.table

      var min = 0, max = table.count
      while min < max {
        let mid = min + (max - min) / 2

        if table[mid].BeginAddress > addrOffset {
          max = mid
          continue
        }

        if mid == table.count - 1 || table[mid + 1].BeginAddress > addrOffset {
          let ptr = UnsafeRawPointer(table.baseAddress! + mid)!
          return UnsafeMutableRawPointer(mutating: ptr)
        }

        min = mid
      }
      return nil
    case .amd64(let wrapper):
      // Search the _IMAGE_RUNTIME_FUNCTION_ENTRY table
      let table = wrapper.table

      var min = 0, max = table.count
      while min < max {
        let mid = min + (max - min) / 2

        if table[mid].BeginAddress > addrOffset {
          max = mid
          continue
        }

        if mid == table.count - 1 || table[mid + 1].BeginAddress > addrOffset {
          let ptr = UnsafeRawPointer(table.baseAddress! + mid)!
          return UnsafeMutableRawPointer(mutating: ptr)
        }

        min = mid
      }
      return nil
    case .i386(let wrapper):
      // Search the FPO_DATA table
      let table = wrapper.table

      var min = 0, max = table.count
      while min < max {
        let mid = min + (max - min) / 2

        let entry = table.baseAddress! + mid
        let ulOffStart = entry.pointee.ulOffStart

        if ulOffStart > addrOffset {
          max = mid
          continue
        }

        let found: Bool

        if mid == table.count - 1 {
          found = true
        } else {
          let nextEntry = entry + 1
          let ulOffStart = nextEntry.pointee.ulOffStart

          found = ulOffStart > addrOffset
        }

        if found {
          let ptr = UnsafeRawPointer(table.baseAddress! + mid)!
          return UnsafeMutableRawPointer(mutating: ptr)
        }

        min = mid
      }
      return nil
    default:
      return nil
    }
  }
}

#endif // os(Windows)
