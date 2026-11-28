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
/// A type that conforms to the `GlobalActor` protocol and is marked with
/// the `@globalActor` attribute can be used as a custom attribute. Such types
/// are called global actor types, and can be applied to any declaration to
/// specify that such types are isolated to that global actor type. When using
/// such a declaration from another actor (or from nonisolated code),
/// synchronization is performed through the shared actor instance to ensure
/// mutually-exclusive access to the declaration.
///
/// ## Custom Actor Executors
/// A global actor uses a custom executor if it needs to customize its execution
/// semantics, for example, by making sure all of its invocations are run on a
/// specific thread or dispatch queue.
///
/// This is done the same way as with normal non-global actors, by declaring a
/// ``Actor/unownedExecutor`` nonisolated property in the ``ActorType``
/// underlying this global actor.
///
/// It is *not* necessary to override the ``sharedUnownedExecutor`` static
/// property of the global actor, as its default implementation already
/// delegates to the `shared.unownedExecutor`, which is the most reasonable
/// and correct implementation of this protocol requirement.
///
/// You can find out more about custom executors, by referring to the
/// ``SerialExecutor`` protocol's documentation.
///
/// - SeeAlso: ``SerialExecutor``
///
/// For information about the language-level concurrency model that `GlobalActor` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency#Global-Actors
/// [tspl]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/
@available(SwiftStdlib 5.1, *)
public protocol GlobalActor: SendableMetatype {
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

  /// Shorthand for referring to the `shared.unownedExecutor` of this global actor.
  ///
  /// When declaring a global actor with a custom executor, prefer to implement
  /// the underlying actor's ``Actor/unownedExecutor`` property, and leave this
  /// `sharedUnownedExecutor` default implementation in-place as it will simply
  /// delegate to the `shared.unownedExecutor`.
  ///
  /// The value of this property must be equivalent to `shared.unownedExecutor`,
  /// as it may be used by the Swift concurrency runtime or explicit user code with
  /// that assumption in mind.
  ///
  /// Returning different executors for different invocations of this computed
  /// property is also illegal, as it could lead to inconsistent synchronization
  /// of the underlying actor.
  ///
  /// - SeeAlso: ``SerialExecutor``
  static var sharedUnownedExecutor: UnownedSerialExecutor { get }
}

@available(SwiftStdlib 5.1, *)
extension GlobalActor {
  public static var sharedUnownedExecutor: UnownedSerialExecutor {
    unsafe shared.unownedExecutor
  }
}

#if $AllowIsolatedGlobalActorAttribute
@available(SwiftStdlib 5.1, *)
extension GlobalActor {
  /// Assume that the current task is executing on this global actor's
  /// serial executor, or stop program execution.
  ///
  /// This method allows to *assume and verify* that the currently
  /// executing synchronous function is actually executing on the serial
  /// executor of this global actor.
  ///
  /// If that is the case, the operation is invoked with an `isolated` version
  /// of the global actor, allowing synchronous access to its isolated state.
  ///
  /// If the current context is not running on the global actor's serial
  /// executor, or if the actor is a reference to a remote actor, this method
  /// will crash with a fatal error (similar to ``preconditionIsolated()``).
  ///
  /// This method can only be used from synchronous functions, as asynchronous
  /// functions should instead perform a normal method call to the actor, which
  /// will hop task execution to the target actor if necessary.
  ///
  /// - Note: This check is performed against the global actor's serial
  ///   executor, meaning that if another actor uses the same serial executor
  ///   then the check will succeed.
  ///
  /// - Parameters:
  ///   - operation: the operation that will be executed if the current context
  ///                is executing on the global actor's serial executor.
  ///   - file: The file name to print if the assertion fails. The default is
  ///           where this method was called.
  ///   - line: The line number to print if the assertion fails The default is
  ///           where this method was called.
  /// - Returns: the return value of the `operation`
  /// - Throws: the error thrown by the operation, if it threw
  @export(implementation)
  @_unavailableFromAsync(message: "await the call to the global actor isolated closure directly")
  public static func assumeIsolated<T: Sendable & ~Copyable, E: Error>(
      _ operation: @isolated(Self) () throws(E) -> T,
      file: StaticString = #fileID, line: UInt = #line
  ) throws(E) -> T {
    /// This is guaranteed to be fatal if the check fails,
    /// as this is our "safe" version of this API.
    let executor: Builtin.Executor = unsafe Self.sharedUnownedExecutor.executor
    guard _taskIsCurrentExecutor(executor) else {
      // TODO: offer information which executor we actually got
      #if !$Embedded
      fatalError("Incorrect actor executor assumption; Expected same executor as \(self).", file: file, line: line)
      #else
      Builtin.int_trap()
      #endif
    }


#if $BuiltinApplyIsolatedUnchecked
    // Apply the closure directly, ignoring its isolation: no escaping
    // conversion, so no closure context allocation and no escape check
    return try Builtin.applyGlobalActorIsolatedUnchecked(operation)
#else
    typealias YesActor = @isolated(Self) () throws(E) -> T
    typealias NoActor = () throws(E) -> T

    // To do the unsafe cast, we have to pretend it's @escaping
    // Use a builtin cast rather than unsafeBitCast, which would require runtime
    // metadata for the typed throws function type (only available since
    // Swift 6.0 runtimes)
    return try withoutActuallyEscaping(operation) {
      (_ fn: @escaping YesActor) throws(E) -> T in
      let rawFn: NoActor = Builtin.reinterpretCast(fn)
      return try rawFn()
    }
#endif
  }
}
#endif

