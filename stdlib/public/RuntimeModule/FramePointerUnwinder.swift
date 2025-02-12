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

@_spi(Unwinders)
public struct FramePointerUnwinder<C: Context, M: MemoryReader>: Sequence, IteratorProtocol {
  public typealias Context = C
  public typealias MemoryReader = M
  public typealias Address = Context.Address

  var pc: Address
  var fp: Address
  var asyncContext: Address
  var first: Bool
  var isAsync: Bool
  var done: Bool

  #if os(Linux)
  var images: ImageMap?
  #endif

  var reader: MemoryReader

  @_specialize(exported: true, kind: full, where C == HostContext, M == UnsafeLocalMemoryReader)
  #if os(macOS) || os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == RemoteMemoryReader)
  #endif
  #if os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == MemserverMemoryReader)
  #endif
  public init(context: Context,
              images: ImageMap?,
              memoryReader: MemoryReader) {

    pc = Address(context.programCounter)
    fp = Address(context.framePointer)
    first = true
    isAsync = false
    done = false
    asyncContext = 0
    reader = memoryReader

    // On Linux, the unwinder needs images in order to spot async frames
    #if os(Linux)
    self.images = images
    #endif
  }

  private func isAsyncSymbol(_ mangledName: String) -> Bool {
    let mangledUTF8 = mangledName.utf8
    if mangledUTF8.last == UInt8(ascii: "_") {
      let withoutUnderscore = mangledUTF8.dropLast(1)
      if let beforeIndexNdx = withoutUnderscore.lastIndex(
           where: { $0 < UInt8(ascii: "0") || $0 > UInt8(ascii: "9") }
         ) {
        let beforeIndex = withoutUnderscore[...beforeIndexNdx]
        let suffix = beforeIndex.suffix(2)
        let awaitResume = "TY".utf8
        let suspendResume = "TQ".utf8
        return suffix.elementsEqual(awaitResume) ||
          suffix.elementsEqual(suspendResume)
      }
    }
    return false
  }

  @_specialize(exported: true, kind: full, where C == HostContext, M == UnsafeLocalMemoryReader)
  #if os(macOS) || os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == RemoteMemoryReader)
  #endif
  #if os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == MemserverMemoryReader)
  #endif
  private mutating func isAsyncPC(_ pc: Address) -> Bool {
    // On Linux, we need to examine the PC to see if this is an async frame
    #if os(Linux)
    let address = MemoryReader.Address(pc)

    if let images = images,
       let imageNdx = images.indexOfImage(at: Backtrace.Address(address)) {
      let base = MemoryReader.Address(images[imageNdx].baseAddress)!
      let relativeAddress = address - base
      let cache = ElfImageCache.threadLocal

      if let hit = cache.lookup(path: images[imageNdx].path) {
        switch hit {
          case let .elf32Image(image):
            if let theSymbol = image.lookupSymbol(
                 address: Elf32Image.Traits.Address(relativeAddress)
               ) {
              return isAsyncSymbol(theSymbol.name)
            }
          case let .elf64Image(image):
            if let theSymbol = image.lookupSymbol(
                 address: Elf64Image.Traits.Address(relativeAddress)) {
              return isAsyncSymbol(theSymbol.name)
            }
        }
      }
    }
    #endif

    return false
  }

  @_specialize(exported: true, kind: full, where C == HostContext, M == UnsafeLocalMemoryReader)
  #if os(macOS) || os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == RemoteMemoryReader)
  #endif
  #if os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == MemserverMemoryReader)
  #endif
  private func isAsyncFrame(_ storedFp: Address) -> Bool {
    #if (os(macOS) || os(iOS) || os(watchOS)) && (arch(arm64) || arch(arm64_32) || arch(x86_64))
    // On Darwin, we borrow a bit of the frame pointer to indicate async
    // stack frames
    return (storedFp & (1 << 60)) != 0
    #else
    return false
    #endif
  }

  @_specialize(exported: true, kind: full, where C == HostContext, M == UnsafeLocalMemoryReader)
  #if os(macOS) || os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == RemoteMemoryReader)
  #endif
  #if os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == MemserverMemoryReader)
  #endif
  private func stripPtrAuth(_ address: Address) -> Address {
    return Context.stripPtrAuth(address: address)
  }

  @_specialize(exported: true, kind: full, where C == HostContext, M == UnsafeLocalMemoryReader)
  #if os(macOS) || os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == RemoteMemoryReader)
  #endif
  #if os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == MemserverMemoryReader)
  #endif
  private mutating func fetchAsyncContext() -> Bool {
    let strippedFp = stripPtrAuth(fp)

    do {
      asyncContext = try reader.fetch(from: MemoryReader.Address(strippedFp - 8),
                                      as: Address.self)
      return true
    } catch {
      return false
    }
  }

  @_specialize(exported: true, kind: full, where C == HostContext, M == UnsafeLocalMemoryReader)
  #if os(macOS) || os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == RemoteMemoryReader)
  #endif
  #if os(Linux)
  @_specialize(exported: true, kind: full, where C == HostContext, M == MemserverMemoryReader)
  #endif
  public mutating func next() -> RichFrame<Address>? {
    if done {
      return nil
    }

    if first {
      first = false
      pc = stripPtrAuth(pc)
      return .programCounter(pc)
    }

    if !isAsync {
      if !isAsyncPC(pc) {
        // Try to read the next fp/pc pair
        var next: Address = 0
        let strippedFp = stripPtrAuth(fp)

        if strippedFp == 0
             || !Context.isAlignedForStack(framePointer:strippedFp) {
          done = true
          return nil
        }

        do {
          pc = stripPtrAuth(try reader.fetch(
                              from:MemoryReader.Address(
                                strippedFp
                                  + Address(MemoryLayout<Address>.size)
                              ),
                              as: Address.self))
          next = try reader.fetch(from: MemoryReader.Address(strippedFp),
                                  as: Address.self)
        } catch {
          done = true
          return nil
        }

        if next <= fp || pc == 0 {
          done = true
          return nil
        }

        if !isAsyncFrame(next) {
          fp = next
          return .returnAddress(pc)
        }
      }

      isAsync = true
      if !fetchAsyncContext() {
        done = true
        return nil
      }
    }

    // If we get here, we're in async mode

    var next: Address = 0
    let strippedCtx = stripPtrAuth(asyncContext)

    if strippedCtx == 0 {
      done = true
      return nil
    }

    #if arch(arm64_32)

    // On arm64_32, the two pointers at the start of the context are 32-bit,
    // although the stack layout is identical to vanilla arm64
    do {
      var next32 = try reader.fetch(from: MemoryReader.Address(strippedCtx),
                                    as: UInt32.self)
      var pc32 = try reader.fetch(from: MemoryReader.Address(strippedCtx + 4),
                                  as: UInt32.self)

      next = Address(next32)
      pc = stripPtrAuth(Address(pc32))
    } catch {
      done = true
      return nil
    }
    #else

    // Otherwise it's two 64-bit words
    do {
      next = try reader.fetch(from: MemoryReader.Address(strippedCtx),
                              as: Address.self)
      pc = stripPtrAuth(try reader.fetch(from: MemoryReader.Address(strippedCtx + 8),
                                         as: Address.self))
    } catch {
      done = true
      return nil
    }

    #endif

    asyncContext = next
    return .asyncResumePoint(pc)
  }
}
