//===--- FramePointerUnwinder.swift ---------------------------*- swift -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  Unwind the stack by chasing the frame pointer.
//
//===----------------------------------------------------------------------===//

import Swift

// @available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_getCurrent")
func _getCurrentAsyncTask() -> UnsafeRawPointer?

@_spi(Unwinders)
public struct FramePointerUnwinder<C: Context, M: MemoryReader>: Sequence, IteratorProtocol {
  public typealias Context = C
  public typealias MemoryReader = M
  public typealias Address = MemoryReader.Address

  var pc: Address
  var fp: Address
  var asyncContext: Address
  var first: Bool
  var isAsync: Bool

  var reader: MemoryReader

  public init(context: Context, memoryReader: MemoryReader) {
    pc = Address(context.programCounter)
    fp = Address(context.framePointer)
    first = true
    isAsync = false
    asyncContext = 0
    reader = memoryReader
  }

  private func isAsyncFrame(_ storedFp: Address) -> Bool {
    #if (os(macOS) || os(iOS) || os(watchOS)) && (arch(arm64) || arch(arm64_32) || arch(x86_64))
    // On Darwin, we borrow a bit of the frame pointer to indicate async
    // stack frames
    return (storedFp & (1 << 60)) != 0 && _getCurrentAsyncTask() != nil
    #else
    return false
    #endif
  }

  private func stripPtrAuth(_ address: Address) -> Address {
    return Address(Context.stripPtrAuth(address: Context.Address(address)))
  }

  private mutating func fetchAsyncContext() -> Bool {
    let strippedFp = stripPtrAuth(fp)

    do {
      asyncContext = try reader.fetch(from: Address(strippedFp - 8),
                                      as: Address.self)
      return true
    } catch {
      return false
    }
  }

  public mutating func next() -> Backtrace.Frame? {
    if first {
      first = false
      pc = stripPtrAuth(pc)
      return .programCounter(Backtrace.Address(pc))
    }

    if !isAsync {
      // Try to read the next fp/pc pair
      var next: Address = 0
      let strippedFp = stripPtrAuth(fp)

      if strippedFp == 0
           || !Context.isAlignedForStack(framePointer:
                                           Context.Address(strippedFp)) {
        return nil
      }

      do {
        pc = stripPtrAuth(try reader.fetch(from:
                 strippedFp + Address(MemoryLayout<Address>.size),
                 as: Address.self))
        next = try reader.fetch(from: Address(strippedFp), as: Address.self)
      } catch {
        return nil
      }

      if next <= fp {
        return nil
      }

      if !isAsyncFrame(next) {
        fp = next
        return .returnAddress(Backtrace.Address(pc))
      }

      isAsync = true
      if !fetchAsyncContext() {
        return nil
      }
    }

    // If we get here, we're in async mode

    var next: Address = 0
    let strippedCtx = stripPtrAuth(asyncContext)

    if strippedCtx == 0 {
      return nil
    }

    #if arch(arm64_32)

    // On arm64_32, the two pointers at the start of the context are 32-bit,
    // although the stack layout is identical to vanilla arm64
    do {
      var next32 = try reader.fetch(from: strippedCtx, as: UInt32.self)
      var pc32 = try reader.fetch(from: strippedCtx + 4, as: UInt32.self)

      next = Address(next32)
      pc = stripPtrAuth(Address(pc32))
    } catch {
      return nil
    }
    #else

    // Otherwise it's two 64-bit words
    do {
      next = try reader.fetch(from: strippedCtx, as: Address.self)
      pc = stripPtrAuth(try reader.fetch(from: strippedCtx + 8, as: Address.self))
    } catch {
      return nil
    }

    #endif

    asyncContext = next

    return .asyncResumePoint(Backtrace.Address(pc))
  }
}
