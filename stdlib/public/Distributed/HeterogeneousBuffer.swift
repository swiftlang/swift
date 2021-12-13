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

extension MemoryLayout {
  /// Find the next offset that's appropriately aligned
  /// for this type.
  static func nextAlignedOffset(_ offset: Int) -> Int {
    ((offset + alignment - 1) & ~(alignment - 1))
  }
}

@available(SwiftStdlib 5.6, *)
struct HeterogeneousBuffer {
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

//func myCompute(_ i: Int8, _ s: String, _ d: Double) {
//  print("myCompute: i = \(i), s = \(s), d = \(d)")
//}
//
//func myComputeThunk(buffer: UnsafeMutableRawPointer) {
//  var offset = 0
//
//  offset = MemoryLayout<Int8>.nextAlignedOffset(offset)
//  let i = buffer.load(fromByteOffset: offset, as: Int8.self)
//  offset += MemoryLayout<Int>.size
//
//  offset = MemoryLayout<String>.nextAlignedOffset(offset)
//  let s = buffer.load(fromByteOffset: offset, as: String.self)
//  offset += MemoryLayout<String>.size
//
//  offset = MemoryLayout<Double>.nextAlignedOffset(offset)
//  let d = buffer.load(fromByteOffset: offset, as: Double.self)
//  offset += MemoryLayout<Double>.size
//
//  myCompute(i, s, d)
//}
//
//let values: [Any] = [1 as Int8, "hello", 3.14159]
//print("Original array of existential values: \(values)")
//let buffer = HeterogeneousBuffer.allocate(values: values)
//print("Mapped back into an array of existential values: \(buffer.existentialValues)")
//myComputeThunk(buffer: buffer.buffer)
//buffer.deinitialize()
//buffer.deallocate()
//
//// [1, "hello", 3.14159]
//// [1, "hello", 3.14159]
//// i = 1, s = hello, d = 3.14159
