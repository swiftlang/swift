//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A type that represents a globally-unique actor that can be used to isolate
/// various declarations anywhere in the program.
///
/// A type that conforms to the `GlobalActor` protocol and is marked with the
/// the `@globalActor` attribute can be used as a custom attribute. Such types
/// are called global actor types, and can be applied to any declaration to
/// specify that such types are isolated to that global actor type. When using
/// such a declaration from another actor (or from nonisolated code),
/// synchronization is performed through the \c shared actor instance to ensure
/// mutually-exclusive access to the declaration.
@available(SwiftStdlib 5.5, *)
public protocol GlobalActor {
  /// The type of the shared actor instance that will be used to provide
  /// mutually-exclusive access to declarations annotated with the given global
  /// actor type.
  associatedtype ActorType: Actor

  /// The shared actor instance that will be used to provide mutually-exclusive
  /// access to declarations annotated with the given global actor type.
  ///
  /// The value of this property must always evaluate to the same actor
  /// instance.
  static var shared: ActorType { get }

  /// The shared executor instance that will be used to provide
  /// mutually-exclusive access for the global actor.
  ///
  /// The value of this property must be equivalent to `shared.unownedExecutor`.
  static var sharedUnownedExecutor: UnownedSerialExecutor { get }
}

@available(SwiftStdlib 5.5, *)
extension GlobalActor {
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    shared.unownedExecutor
  }
}

