//===----------------------------------------------------------------------===//
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

//===----------------------------------------------------------------------===//
// Bool AtomicValue conformance
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension Bool: AtomicValue {
  @available(SwiftStdlib 5.10, *)
  public typealias AtomicRepresentation = UInt8.AtomicRepresentation

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func encodeAtomicRepresentation(
    _ value: borrowing Bool
  ) -> AtomicRepresentation {
    UInt8.encodeAtomicRepresentation(
      UInt8(Builtin.zext_Int1_Int8(value._value))
    )
  }

  @available(SwiftStdlib 5.10, *)
  @_alwaysEmitIntoClient
  @_transparent
  public static func decodeAtomicRepresentation(
    _ representation: consuming AtomicRepresentation
  ) -> Bool {
    Bool(Builtin.trunc_Int8_Int1(
      UInt8.decodeAtomicRepresentation(representation)._value)
    )
  }
}

//===----------------------------------------------------------------------===//
// Bool load then atomic operations
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.10, *)
extension Atomic where Value == Bool {
  /// Perform an atomic logical AND operation and return the old and new value,
  /// applying the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple with the old value before the operation a the new value
  ///   after the operation.
  @available(SwiftStdlib 5.10, *)
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func logicalAnd(
    with operand: Bool,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Bool, newValue: Bool) {
    // If we could reinterpret self as `Atomic<UInt8>` then we could just call
    // bitwiseAnd...

    let original = switch ordering {
    case .relaxed:
      Builtin.atomicrmw_and_monotonic_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .acquiring:
      Builtin.atomicrmw_and_acquire_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .releasing:
      Builtin.atomicrmw_and_release_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .acquiringAndReleasing:
      Builtin.atomicrmw_and_acqrel_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .sequentiallyConsistent:
      Builtin.atomicrmw_and_seqcst_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    default:
      Builtin.unreachable()
    }

    let old = Bool.decodeAtomicRepresentation(_AtomicStorage8(original))

    return (oldValue: old, newValue: old && operand)
  }

  /// Perform an atomic logical OR operation and return the old and new value,
  /// applying the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple with the old value before the operation a the new value
  ///   after the operation.
  @available(SwiftStdlib 5.10, *)
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func logicalOr(
    with operand: Bool,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Bool, newValue: Bool) {
    let original = switch ordering {
    case .relaxed:
      Builtin.atomicrmw_or_monotonic_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .acquiring:
      Builtin.atomicrmw_or_acquire_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .releasing:
      Builtin.atomicrmw_or_release_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .acquiringAndReleasing:
      Builtin.atomicrmw_or_acqrel_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .sequentiallyConsistent:
      Builtin.atomicrmw_or_seqcst_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    default:
      Builtin.unreachable()
    }

    let old = Bool.decodeAtomicRepresentation(_AtomicStorage8(original))

    return (oldValue: old, newValue: old || operand)
  }

  /// Perform an atomic logical XOR operation and return the old and new value,
  /// applying the specified memory ordering.
  ///
  /// - Parameter operand: A boolean value.
  /// - Parameter ordering: The memory ordering to apply on this operation.
  /// - Returns: A tuple with the old value before the operation a the new value
  ///   after the operation.
  @available(SwiftStdlib 5.10, *)
  @discardableResult
  @_semantics("atomics.requires_constant_orderings")
  @_alwaysEmitIntoClient
  @_transparent
  public func logicalXor(
    with operand: Bool,
    ordering: AtomicUpdateOrdering
  ) -> (oldValue: Bool, newValue: Bool) {
    let original = switch ordering {
    case .relaxed:
      Builtin.atomicrmw_xor_monotonic_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .acquiring:
      Builtin.atomicrmw_xor_acquire_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .releasing:
      Builtin.atomicrmw_xor_release_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .acquiringAndReleasing:
      Builtin.atomicrmw_xor_acqrel_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    case .sequentiallyConsistent:
      Builtin.atomicrmw_xor_seqcst_Int8(
        rawAddress,
        Bool.encodeAtomicRepresentation(operand).storage
      )

    default:
      Builtin.unreachable()
    }

    let old = Bool.decodeAtomicRepresentation(_AtomicStorage8(original))

    return (oldValue: old, newValue: old != operand)
  }
}
