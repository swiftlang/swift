//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 5.6, *)
struct HeterogeneousBuffer: Sequence {
  typealias Element = UnsafeMutableRawPointer

  let types: [Any.Type]
  let buffer: UnsafeMutableRawPointer

  static func allocate(forTypes types: [Any.Type]) -> HeterogeneousBuffer {
    let buffer = UnsafeMutableRawPointer.allocate(
        byteCount: HeterogeneousBuffer.getBufferSize(types: types), alignment: 16)
    return HeterogeneousBuffer(types: types, buffer: buffer)
  }

  static func allocate(withValues values: [Any]) -> HeterogeneousBuffer {
    let types = values.map { type(of: $0) }
    let buffer = UnsafeMutableRawPointer.allocate(
      byteCount: HeterogeneousBuffer.getBufferSize(types: types), alignment: 16)

    var offset = 0
    func initialize<T>(_ value: T) {
      // Move to the start of this element in the buffer
      offset = MemoryLayout<T>.nextAlignedOffset(offset)

      // Store this value at that position.
      buffer.storeBytes(of: value, toByteOffset: offset, as: T.self)

      // Move past this value.
      offset += MemoryLayout<T>.size
    }

    for value in values {
      _openExistential(value, do: initialize)
    }

    return HeterogeneousBuffer(types: types, buffer: buffer)
  }

  /// Iterator exposing writable unsafe pointers into the appropriate offsets
  /// of the underlying `buffer`.
  ///
  /// Use this to initialize the buffer after allocating it "for types".
  func makeIterator() -> UnsafeHeterogeneousBufferIterator {
    UnsafeHeterogeneousBufferIterator(self)
  }
  struct UnsafeHeterogeneousBufferIterator: IteratorProtocol {
    typealias Element = UnsafeMutableRawPointer

    let hbuf: HeterogeneousBuffer

    var i: Int = 0
    var offset: Int = 0

    init(_ hbuf: HeterogeneousBuffer) {
      self.hbuf = hbuf
    }

    mutating func next() -> UnsafeMutableRawPointer? {
      guard i < hbuf.types.count else {
        return nil
      }

      func alignedOffsetAndSize<T>(_: T.Type) -> (offset: Int, size: Int) {
        return (MemoryLayout<T>.nextAlignedOffset(offset), MemoryLayout<T>.size)
      }

      // Retrieve aligned offset and element size info
      let info = _openExistential(hbuf.types[i], do: alignedOffsetAndSize)
      // Adjust current offset
      offset = info.offset

      defer {
        // Advance offset by the size of the current element
        offset += info.size
        i += 1
      }

      return hbuf.buffer.advanced(by: offset)
    }
  }

  func deinitialize() {
    var offset = 0
    func destroy<T>(_ type: T.Type) {
      // Move to the start of this element in the buffer
      offset = MemoryLayout<T>.nextAlignedOffset(offset)

      // Destroy the value at this position.
      buffer.advanced(by: offset).assumingMemoryBound(to: type).deinitialize(count: 1)

      // Move past this value.
      offset += MemoryLayout<T>.size
    }

    for type in types {
      _openExistential(type, do: destroy)
    }
  }

  func deallocate() {
    buffer.deallocate()
  }
}

extension MemoryLayout {
  /// Find the next offset that's appropriately aligned
  /// for this type.
  static func nextAlignedOffset(_ offset: Int) -> Int {
    ((offset + alignment - 1) & ~(alignment - 1))
  }
}

@available(SwiftStdlib 5.6, *)
protocol HeterogeneousVisitor {
  associatedtype Result

  func visit<T>(_ value: T) -> Result
}

@available(SwiftStdlib 5.6, *)
extension HeterogeneousBuffer {
  func map<Visitor: HeterogeneousVisitor>(visitor: Visitor) -> [Visitor.Result] {
    var results: [Visitor.Result] = []

    var offset = 0
    func append<T>(_ type: T.Type) {
      // Move to the start of this element in the buffer
      offset = MemoryLayout<T>.nextAlignedOffset(offset)

      // Read a value from this position.
      let value = buffer.load(fromByteOffset: offset, as: T.self)
      results.append(visitor.visit(value))

      // Move past this value.
      offset += MemoryLayout<T>.size
    }

    for type in types {
      _openExistential(type, do: append)
    }

    return results
  }

  private struct AnyVisitor: HeterogeneousVisitor {
    func visit<T>(_ value: T) -> Any { value }
  }

  var existentialValues: [Any] { map(visitor: AnyVisitor()) }
}

@available(SwiftStdlib 5.6, *)
extension HeterogeneousBuffer {
  private static func getBufferSize(
    types: [Any.Type]
  ) -> Int {
    var offset = 0

    func allocateSpace<T>(_ type: T.Type) -> Int {
      return MemoryLayout<T>.nextAlignedOffset(offset) + MemoryLayout<T>.size
    }

    for type in types {
      offset = _openExistential(type, do: allocateSpace)
    }

    return offset
  }
}
