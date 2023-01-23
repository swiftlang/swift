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

import Swift
import _Runtime

/// A namespace used for working with runtime attributes.
@available(SwiftStdlib 5.9, *)
@frozen
public enum Attribute {
  /// Get all the instances of a runtime attribute wherever it's attached to.
  ///
  /// Example:
  ///
  ///     @runtimeMetadata
  ///     struct Field {
  ///       let name: String
  ///
  ///       init<T, U>(attachedTo: KeyPath<T, U>, _ name: String) {
  ///         self.name = name
  ///       }
  ///     }
  ///
  ///     struct Dog {
  ///       @Field("dog_breed")
  ///       let breed: String
  ///     }
  ///
  ///     let fields = Attribute.allInstances(of: Field.self)
  ///
  ///     for field in fields {
  ///       print(field.name) // "dog_breed"
  ///     }
  ///
  /// - Parameters:
  ///   - type: The type of the attribute that is attached to various sources.
  /// - Returns: A sequence of attribute instances of `type` in no particular
  ///   order.
  @available(SwiftStdlib 5.9, *)
  public static func allInstances<T>(of type: T.Type) -> AttributeInstances<T> {
    let meta = Metadata(T.self)

    guard meta.kind == .struct ||
          meta.kind == .enum ||
          meta.kind == .class else {
      return AttributeInstances([])
    }

    return ImageInspection.withAttributeCache {
      let attrDescriptor = meta.type.descriptor.base

      guard let fnPtrs = $0[attrDescriptor] else {
        return AttributeInstances([])
      }

      return AttributeInstances(fnPtrs)
    }
  }
}

/// A sequence wrapper over some runtime attribute instances.
///
/// Instances of `AttributeInstances` are created with the
/// `Attribute.allInstances(of:)` function.
@available(SwiftStdlib 5.9, *)
@frozen
public struct AttributeInstances<T> {
  @usableFromInline
  let fnPtrs: [UnsafeRawPointer]

  @usableFromInline
  var index = 0

  @available(SwiftStdlib 5.9, *)
  init(_ fnPtrs: [UnsafeRawPointer]) {
    self.fnPtrs = fnPtrs
  }
}

@available(SwiftStdlib 5.9, *)
extension AttributeInstances: IteratorProtocol {
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public mutating func next() -> T? {
    while index < fnPtrs.endIndex {
      let fnPtr = fnPtrs[index]
      index += 1

      typealias AttributeFn = @convention(thin) () -> T?

      let fn = unsafeBitCast(fnPtr, to: AttributeFn.self)

      guard let attribute = fn() else {
        continue
      }

      return attribute
    }

    return nil
  }
}

@available(SwiftStdlib 5.9, *)
extension AttributeInstances: Sequence {}
