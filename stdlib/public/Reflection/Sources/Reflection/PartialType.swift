//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import Swift
import _Runtime

@available(SwiftStdlib 5.9, *)
@frozen
public struct PartialType {
  @usableFromInline
  let descriptor: TypeDescriptor
  
  @inlinable
  init(_ metadata: TypeMetadata) {
    self.descriptor = metadata.descriptor
  }
}

@available(SwiftStdlib 5.9, *)
extension PartialType {
  @inlinable
  public var isGeneric: Bool {
    descriptor.base.flags.isGeneric
  }
  
  @inlinable
  public var name: String {
    descriptor.name
  }
}

@available(SwiftStdlib 5.9, *)
extension PartialType {
  /// Creates a fully realized and uniqued `Type` instance using this
  /// `PartialType` as the base type.
  ///
  /// This variant takes no arguments which assumes the type being referenced by
  /// this `PartialType` is completely non-generic. In the following example,
  /// `Nested` is technically generic, but in reality there's only a single
  /// spelling of this which is `Generic<Int>.Nested`. Considering this,
  /// `PartialType` does not consider this type to be generic thus you can call
  /// the no argument variant of `create(with:)` to create a type of `Nested`.
  ///
  ///     struct Generic<T> {}
  ///
  ///     extension Generic where T == Int {
  ///       struct Nested {}
  ///     }
  ///
  ///     let nestedTy = Type(Generic<Int>.Nested.self)
  ///     print(nestedTy.partial?.create()) // Optional(Generic<Int>.Nested)
  ///
  /// - Returns: The created `Type` instance or nil if the operation failed.
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public func create() -> Type? {
    _create(with: UnsafeBufferPointer(start: nil, count: 0))
  }

  /// Creates a fully realized and uniqued `Type` instance using this
  /// `PartialType` as the base type.
  ///
  /// Takes a list of types to use when making the full `Type` instance. It
  /// expects the exact same number of generic arguments that is needed for
  /// creation. Note that all of the provided types must fulfill the
  /// requirements of the type's generic signature. In the following example,
  /// `Dictionary` requires that its `Key` argument must conform to `Hashable`.
  /// If the provided `Key` argument in the list of types does not conform to
  /// `Hashable`, this operation will fail and return a `nil`.
  ///
  ///     struct Dictionary<Key: Hashable, Value> {}
  ///
  ///     let dictTy = Type([String: String].self)
  ///     // Optional(Dictionary<String, Int>)
  ///     print(dictTy.partial?.create(with: Type(String.self), Type(Int.self)))
  ///
  /// The order in which the provided types are assigned start from the
  /// outermost generic context and works its way inward.
  ///
  ///     struct ABC<A, B: Equatable> {
  ///       struct XYZ<C: Hashable, D> {}
  ///     }
  ///
  /// Effectively `XYZ` has 4 generic arguments needed to fully realize it.
  /// Given the following list of types: `[Int, String, Double, Array<Float>]`,
  /// we start with the outermost type, `ABC`, and assign the following generic
  /// arguments: `<A = Int, B = String>` and lookup the `Equatable` conformance
  /// for `String`. Now for `XYZ`, we finish the generic assignment with the
  /// following: `<C = Double, D = Array<Float>>` and lookup `Double`'s
  /// `Hashable`.
  ///
  /// - Parameters:
  ///   - types: A variadic list of `Type` instances used when making this
  ///     `PartialType`'s realized `Type`.
  /// - Returns: The created `Type` instance or nil if the operation failed.
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public func create(with types: Type...) -> Type? {
    create(with: types)
  }

  /// Creates a fully realized and uniqued `Type` instance using this
  /// `PartialType` as the base type.
  ///
  /// Takes a list of types to use when making the full `Type` instance. It
  /// expects the exact same number of generic arguments that is needed for
  /// creation. Note that all of the provided types must fulfill the
  /// requirements of the type's generic signature. In the following example,
  /// `Dictionary` requires that its `Key` argument must conform to `Hashable`.
  /// If the provided `Key` argument in the list of types does not conform to
  /// `Hashable`, this operation will fail and return a `nil`.
  ///
  ///     struct Dictionary<Key: Hashable, Value> {}
  ///
  ///     let dictTy = Type([String: String].self)
  ///     // Optional(Dictionary<String, Int>)
  ///     print(dictTy.partial?.create(
  ///       with: [Type(String.self), Type(Int.self)]
  ///     ))
  ///
  /// The order in which the provided types are assigned start from the
  /// outermost generic context and works its way inward.
  ///
  ///     struct ABC<A, B: Equatable> {
  ///       struct XYZ<C: Hashable, D> {}
  ///     }
  ///
  /// Effectively `XYZ` has 4 generic arguments needed to fully realize it.
  /// Given the following list of types: `[Int, String, Double, Array<Float>]`,
  /// we start with the outermost type, `ABC`, and assign the following generic
  /// arguments: `<A = Int, B = String>` and lookup the `Equatable` conformance
  /// for `String`. Now for `XYZ`, we finish the generic assignment with the
  /// following: `<C = Double, D = Array<Float>>` and lookup `Double`'s
  /// `Hashable`.
  ///
  /// - Parameters:
  ///   - types: An array of `Type` instances used when making this
  ///     `PartialType`'s realized `Type`.
  /// - Returns: The created `Type` instance or nil if the operation failed.
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public func create(with args: [Type]) -> Type? {
    args.withUnsafeBufferPointer {
      let buffer = UnsafeBufferPointer<UnsafeRawPointer>(
        start: UnsafePointer<UnsafeRawPointer>(
          $0.baseAddress.unsafelyUnwrapped._rawValue
        ),
        count: $0.count
      )

      return _create(with: buffer)
    }
  }

  /// Creates a fully realized and uniqued `Type` instance using this
  /// `PartialType` as the base type.
  ///
  /// Takes a list of types to use when making the full `Type` instance. It
  /// expects the exact same number of generic arguments that is needed for
  /// creation. Note that all of the provided types must fulfill the
  /// requirements of the type's generic signature. In the following example,
  /// `Dictionary` requires that its `Key` argument must conform to `Hashable`.
  /// If the provided `Key` argument in the list of types does not conform to
  /// `Hashable`, this operation will fail and return a `nil`.
  ///
  ///     struct Dictionary<Key: Hashable, Value> {}
  ///
  ///     let dictTy = Type([String: String].self)
  ///     // Optional(Dictionary<String, Int>)
  ///     print(dictTy.partial?.create(with: String.self, Int.self))
  ///
  /// The order in which the provided types are assigned start from the
  /// outermost generic context and works its way inward.
  ///
  ///     struct ABC<A, B: Equatable> {
  ///       struct XYZ<C: Hashable, D> {}
  ///     }
  ///
  /// Effectively `XYZ` has 4 generic arguments needed to fully realize it.
  /// Given the following list of types: `[Int, String, Double, Array<Float>]`,
  /// we start with the outermost type, `ABC`, and assign the following generic
  /// arguments: `<A = Int, B = String>` and lookup the `Equatable` conformance
  /// for `String`. Now for `XYZ`, we finish the generic assignment with the
  /// following: `<C = Double, D = Array<Float>>` and lookup `Double`'s
  /// `Hashable`.
  ///
  /// - Parameters:
  ///   - types: A variadic list of `Any.Type` instances used when making this
  ///     `PartialType`'s realized `Type`.
  /// - Returns: The created `Type` instance or nil if the operation failed.
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public func create(with args: any Any.Type...) -> Type? {
    create(with: args)
  }

  /// Creates a fully realized and uniqued `Type` instance using this
  /// `PartialType` as the base type.
  ///
  /// Takes a list of types to use when making the full `Type` instance. It
  /// expects the exact same number of generic arguments that is needed for
  /// creation. Note that all of the provided types must fulfill the
  /// requirements of the type's generic signature. In the following example,
  /// `Dictionary` requires that its `Key` argument must conform to `Hashable`.
  /// If the provided `Key` argument in the list of types does not conform to
  /// `Hashable`, this operation will fail and return a `nil`.
  ///
  ///     struct Dictionary<Key: Hashable, Value> {}
  ///
  ///     let dictTy = Type([String: String].self)
  ///     // Optional(Dictionary<String, Int>)
  ///     print(dictTy.partial?.create(with: [String.self, Int.self]))
  ///
  /// The order in which the provided types are assigned start from the
  /// outermost generic context and works its way inward.
  ///
  ///     struct ABC<A, B: Equatable> {
  ///       struct XYZ<C: Hashable, D> {}
  ///     }
  ///
  /// Effectively `XYZ` has 4 generic arguments needed to fully realize it.
  /// Given the following list of types: `[Int, String, Double, Array<Float>]`,
  /// we start with the outermost type, `ABC`, and assign the following generic
  /// arguments: `<A = Int, B = String>` and lookup the `Equatable` conformance
  /// for `String`. Now for `XYZ`, we finish the generic assignment with the
  /// following: `<C = Double, D = Array<Float>>` and lookup `Double`'s
  /// `Hashable`.
  ///
  /// - Parameters:
  ///   - types: An array of `Any.Type` instances used when making this
  ///     `PartialType`'s realized `Type`.
  /// - Returns: The created `Type` instance or nil if the operation failed.
  @available(SwiftStdlib 5.9, *)
  @inlinable
  public func create(with args: [any Any.Type]) -> Type? {
    args.withUnsafeBufferPointer {
      let buffer = UnsafeBufferPointer<UnsafeRawPointer>(
        start: UnsafePointer<UnsafeRawPointer>(
          $0.baseAddress.unsafelyUnwrapped._rawValue
        ),
        count: $0.count
      )

      return _create(with: buffer)
    }
  }

  @available(SwiftStdlib 5.9, *)
  @usableFromInline
  internal func _create(
    with args: UnsafeBufferPointer<UnsafeRawPointer>
  ) -> Type? {
    // If a descriptor doesn't have a generic signature, it itself is not
    // generic. Thus, we have the 0 argument case, so just call the accessor
    // with no arguments if we were passed no arguments.
    guard let genericSig = descriptor.genericSignature else {
      guard args.count == 0 else {
        return nil
      }

      return Type(descriptor.accessor(.complete))
    }

    // Otherwise, this type is generic.

    // Gather the number of key parameters defined by the actual parameters in a
    // generic signature.
    var numberOfParameterKeyArguments = 0

    for parameter in genericSig.parameters {
      if parameter.hasKeyArgument {
        numberOfParameterKeyArguments += 1
      }
    }

    // Make sure the number of arguments we were given is equal to the number of
    // parameter key arguments in our generic signature.
    guard numberOfParameterKeyArguments == args.count else {
      return nil
    }

    // If we don't have any parameter key arguments, then we're done and can
    // call the accessor with nothing.
    guard numberOfParameterKeyArguments > 0 else {
      return Type(descriptor.accessor(.complete))
    }

    // If we don't have requirements to deal with, just do the simple thing and
    // call the accessor with just our metadata arguments. Otherwise, we need to
    // ensure our arguments conform to all their respective protocols, layouts,
    // or same type requirements.
    guard genericSig.requirements.count > 0 else {
      return createNoRequirements(with: args)
    }

    return createRequirements(with: args, genericSig)
  }

  @available(SwiftStdlib 5.9, *)
  internal func createNoRequirements(
    with args: UnsafeBufferPointer<UnsafeRawPointer>
  ) -> Type? {
    switch args.count {
    case 1:
      return Type(descriptor.accessor(.complete, args[0]))
    case 2:
      return Type(descriptor.accessor(.complete, args[0], args[1]))
    case 3:
      return Type(descriptor.accessor(.complete, args[0], args[1], args[2]))
    default:
      return Type(descriptor.accessor(.complete, args))
    }
  }

  @available(SwiftStdlib 5.9, *)
  internal func createRequirements(
    with args: UnsafeBufferPointer<UnsafeRawPointer>,
    _ genericSig: GenericSignature
  ) -> Type? {
    var keyArguments = Array(args)
    let argPtr = UnsafeRawPointer(args.baseAddress.unsafelyUnwrapped)

    for req in genericSig.requirements {
      switch req.flags.kind {
      // There are 3 kinds of same type requirements:
      //
      //   1. Concrete type like 'T == String'
      //   2. Associated type same type constraints. E.g. 'T.Element == X'
      //   3. Same type constraints to other generic parameters. E.g. 'T == U'
      //
      // The first case should have been handled before we got here because
      // those are purely syntactical parameters at that point. However, we must
      // still check the 2nd case. The 3rd case is also somewhat syntactical,
      // but we still have a key argument in that case which is why it makes
      // its way down here.
      case .sameType:
        if !req.checkSameType(in: descriptor.base, with: argPtr) {
          return nil
        }

      // Protocol conformance requirement like 'T: P'.
      case .protocol:
        // Ensure that the passed parameters conform to their respectful
        // protocols and pass the witness table 
        guard let witnessTable = req.checkProtocolConformance(
          in: descriptor.base,
          with: argPtr
        ) else {
          return nil
        }

        // If this requirement doesn't introduce a key argument, don't append
        // the found witness table to our arguments. Not sure if this is
        // possible, but let's be defensive here.
        guard req.flags.hasKeyArgument else {
          continue
        }

        keyArguments.append(witnessTable.ptr)

      // Check that the appropriate argument is a subclass of the required base
      // class.
      case .baseClass:
        if !req.checkBaseClass(in: descriptor.base, with: argPtr) {
          return nil
        }

      // The only currently legal layout constraint is 'AnyObject'. Ensure that
      // whatever argument is supposed to bind to this constraint is in fact
      // an object.
      case .layout:
        if !req.checkLayout(in: descriptor.base, with: argPtr) {
          return nil
        }

      // Same conformance requirements are currently not emitted, so it's safe
      // to skip this kind.
      case .sameConformance:
        continue

      // If we have a requirement that we don't know about, just be defensive
      // and return nil.
      default:
        return nil
      }
    }

    switch keyArguments.count {
    case 1:
      return Type(descriptor.accessor(.complete, keyArguments[0]))
    case 2:
      return Type(descriptor.accessor(
        .complete,
        keyArguments[0],
        keyArguments[1]
      ))
    case 3:
      return Type(descriptor.accessor(
        .complete,
        keyArguments[0],
        keyArguments[1],
        keyArguments[2]
      ))
    default:
      return Type(descriptor.accessor(.complete, keyArguments))
    }
  }
}

//===----------------------------------------------------------------------===//
// Stdlib conformances
//===----------------------------------------------------------------------===//

@available(SwiftStdlib 5.9, *)
extension PartialType: Equatable {
  @inlinable
  public static func ==(_ lhs: PartialType, _ rhs: PartialType) -> Bool {
    lhs.descriptor == rhs.descriptor
  }
}

@available(SwiftStdlib 5.9, *)
extension PartialType: Hashable {
  @inlinable
  public func hash(into hasher: inout Hasher) {
    hasher.combine(descriptor)
  }
}
