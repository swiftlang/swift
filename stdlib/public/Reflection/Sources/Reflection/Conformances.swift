//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2023 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

#if os(macOS) || os(iOS) || os(tvOS) || os(watchOS)

import Swift
import _Runtime

@available(SwiftStdlib 5.9, *)
@_silgen_name("_swift_reflection_withConformanceCache")
func _withConformanceCache(
  _ proto: ProtocolDescriptor,
  _ context: UnsafeMutableRawPointer,
  _ callback: @convention(c) (
    /* Array of types */ UnsafePointer<UnsafeRawPointer>,
    /* Number of types */ Int,
    /* Context we just passed */ UnsafeMutableRawPointer
  ) -> ()
)

@available(SwiftStdlib 5.9, *)
@_spi(Reflection)
public func _typesThatConform(to type: Any.Type) -> [Any.Type]? {
  let meta = Metadata(type)

  guard meta.kind == .existential else {
    return nil
  }

  let existential = meta.existential

  let protos = existential.protocols

  guard protos.count == 1 else {
    return nil
  }

  let proto = protos[0]

  var result: [Any.Type] = []

  withUnsafeMutablePointer(to: &result) {
    _withConformanceCache(proto, UnsafeMutableRawPointer($0)) {
      let buffer = UnsafeBufferPointer<Any.Type>(
        start: UnsafePointer<Any.Type>($0._rawValue),
        count: $1
      )

      let arrayPtr = $2.assumingMemoryBound(to: [Any.Type].self)

      arrayPtr.pointee = Array(buffer)
    }
  }

  return result
}

#endif
