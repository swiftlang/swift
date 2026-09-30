//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020-2022 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// Common marker protocol providing a shared "base" for both (local) `Actor`
/// and (potentially remote) `DistributedActor` types.
///
/// The `AnyActor` marker protocol generalizes over all actor types, including
/// distributed ones. In practice, this protocol can be used to restrict
/// protocols, or generic parameters to only be usable with actors, which
/// provides the guarantee that calls may be safely made on instances of given
/// type without worrying about the thread-safety of it -- as they are
/// guaranteed to follow the actor-style isolation semantics.
///
/// While both local and distributed actors are conceptually "actors", there are
/// some important isolation model differences between the two, which make it
/// impossible for one to refine the other.
@available(SwiftStdlib 5.1, *)
@available(*, deprecated, message: "Use 'any Actor' with 'DistributedActor.asLocalActor' instead")
@available(swift, obsoleted: 6.0, message: "Use 'any Actor' with 'DistributedActor.asLocalActor' instead")
public typealias AnyActor = AnyObject & Sendable

/// Common protocol to which all actors conform.
///
/// The `Actor` protocol generalizes over all `actor` types.
/// Define a new actor with the `actor` keyword.
/// Only types declared with the `actor` keyword conform to the `Actor` protocol.
///
/// ### Actors and SerialExecutors
/// By default, actors execute tasks on a shared global concurrency thread pool.
/// This pool is shared by all default actors and tasks, unless an actor or task
/// specified a more specific executor requirement.
///
/// It is possible to configure an actor to use a specific ``SerialExecutor``,
/// as well as impact the scheduling of default tasks and actors by using
/// a ``TaskExecutor``.
///
/// - SeeAlso: ``SerialExecutor``
/// - SeeAlso: ``TaskExecutor``
///
/// For information about the language-level concurrency model that `Actor` is part of,
/// see [Concurrency][concurrency] in [The Swift Programming Language][tspl].
///
/// [concurrency]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/concurrency#Actors
/// [tspl]: https://docs.swift.org/swift-book/documentation/the-swift-programming-language/
@available(SwiftStdlib 5.1, *)
public protocol Actor: AnyObject, Sendable {

  /// Retrieve the executor for this actor as an optimized, unowned
  /// reference.
  ///
  /// This property must always evaluate to the same executor for a
  /// given actor instance, and holding on to the actor must keep the
  /// executor alive.
  ///
  /// This property will be implicitly accessed when work needs to be
  /// scheduled onto this actor.  These accesses may be merged,
  /// eliminated, and rearranged with other work, and they may even
  /// be introduced when not strictly required.  Visible side effects
  /// are therefore strongly discouraged within this property.
  ///
  /// - SeeAlso: ``SerialExecutor``
  /// - SeeAlso: ``TaskExecutor``
  nonisolated var unownedExecutor: UnownedSerialExecutor { get }
}

@available(SwiftStdlib 5.1, *)
@_silgen_name("swift_task_enqueueMainExecutor")
@usableFromInline
internal func _enqueueOnMain(_ job: UnownedJob)

#if $Macros
/// Produce a reference to the actor to which the enclosing code is
/// isolated, or `nil` if the code is nonisolated.
///
/// If the type annotation provided for `#isolation` is not `(any Actor)?`,
/// the type must match the enclosing actor type. If no type annotation is
/// provided, the type defaults to `(any Actor)?`.
@available(SwiftStdlib 5.1, *)
@freestanding(expression)
public macro isolation<T>() -> T = Builtin.IsolationMacro

#endif

#if $IsolatedAny
@export(implementation)
@available(SwiftStdlib 5.1, *)
@available(*, deprecated, message: "Use `.isolation` on @isolated(any) closure values instead.")
public func extractIsolation<each Arg, Result>(
  _ fn: @escaping @isolated(any) (repeat each Arg) async throws -> Result
) -> (any Actor)? {
  return Builtin.extractFunctionIsolation(fn)
}
#endif

// ==== -----------------------------------------------------------------------
// MARK: Fire-and-forget 'oneway' enqueue

#if $Embedded
/// Run `body(value)` on `executor` from a new discarding task, without
/// waiting for it.
///
/// The task copies the task locals of the caller, inherits its priority the
/// same way `Task.init` does, and starts on `executor`, so tasks enqueued on
/// the same serial executor run in the order they were enqueued. The task's
/// operation never suspends and never hops, so it has no async suspension
/// points of its own.
///
/// `body` must be isolated to `executor`; this is not checked.
///
/// `Value` is class-constrained so that the task's operation captures it as
/// a single direct reference. An unconstrained generic value would be
/// captured indirectly, and the generic specializer would then need an async
/// reabstraction thunk around the specialized operation, whose call into it
/// is a suspension point of its own.
///
/// SPI: used by the compiler to lower fire-and-forget calls to synchronous
/// `oneway` functions. Do not use
@available(SwiftStdlib 6.5, *)
@export(implementation)
@unsafe
public func _enqueueOnewayUnchecked<Value: AnyObject>(
  on executor: UnownedSerialExecutor,
  _ value: Value,
  _ body: @escaping (Value) -> Void
) {
  // The operation below only ever runs on 'executor', after this function
  // returned, and nothing else uses these values
  nonisolated(unsafe) let capturedValue = value
  nonisolated(unsafe) let capturedBody = body

  // Starts on 'executor' (the task's initial serial executor) and must not
  // hop off it: '@_unsafeInheritExecutor' suppresses the hop to the generic
  // executor that a nonisolated async function would otherwise start with.
  // It is 'throws' only to match the task operation type exactly, so that
  // no reabstraction thunk (with its own suspension point) is needed
  @_unsafeInheritExecutor
  func _unsafeInheritExecutor_onewayOperation() async throws {
    capturedBody(capturedValue)
  }

  let flags = taskCreateFlags(
    priority: nil, isChildTask: false, copyTaskLocals: true,
    inheritContext: false, enqueueJob: true,
    addPendingGroupTaskUnconditionally: false,
    isDiscardingTask: true, isSynchronousStart: false)
  let builtinSerialExecutor: Builtin.Executor? = unsafe executor.executor

  // Fire-and-forget: drop our reference to the task right away
  _ = Builtin.createDiscardingTask(
    flags: flags,
    initialSerialExecutor: builtinSerialExecutor,
    operation: _unsafeInheritExecutor_onewayOperation)
}

/// Run the synchronous `body` on `actor` from a new discarding task, without
/// waiting for it.
///
/// The task copies the task locals of the caller and starts on the actor's
/// executor, so calls enqueued on the same actor run in the order they were
/// enqueued. See ``_enqueueOnewayUnchecked(on:_:_:)``.
///
/// SPI: used by the compiler to lower `nowait` calls to synchronous `oneway`
/// actor methods. Do not use
@available(SwiftStdlib 6.5, *)
@export(implementation)
public func _enqueueOneway<A: Actor>(
  on actor: A,
  _ body: @escaping (isolated A) -> Void
) {
  // The body only ever runs on the actor's executor, so it is safe to drop
  // the 'isolated' from its parameter. An 'isolated' parameter is passed like
  // any other, so both function types have the same representation.
  // 'Builtin.reinterpretCast' converts the function value in place, where
  // 'unsafeBitCast' would wrap it in two reabstraction thunks
  let unisolatedBody: (A) -> Void = Builtin.reinterpretCast(body)
  unsafe _enqueueOnewayUnchecked(
    on: actor.unownedExecutor, actor, unisolatedBody)
}

/// Run the synchronous, global-actor-isolated `body` on its global actor from
/// a new discarding task, without waiting for it.
///
/// The task copies the task locals of the caller and starts on the global
/// actor's executor, so calls enqueued on the same global actor run in the
/// order they were enqueued. See ``_enqueueOnewayUnchecked(on:_:_:)``.
///
/// SPI: used by the compiler to lower `nowait` calls to synchronous `oneway`
/// global-actor-isolated functions. Do not use
@available(SwiftStdlib 6.5, *)
@export(implementation)
public func _enqueueOnewayIsolated(
  _ body: @escaping @isolated(any) () -> Void
) {
  guard let isolation = Builtin.extractFunctionIsolation(body) else {
    fatalError("'oneway' call without an isolation to enqueue it on")
  }
  // The body only ever runs on its isolation's executor, so it is safe to
  // call it as a plain synchronous function there. An '@isolated(any)'
  // function value has the same representation as any other thick function:
  // its isolation is stored inside the closure context, which the invocation
  // function receives as usual, so dropping the '@isolated(any)' is a plain
  // 'convert_function'. 'Builtin.reinterpretCast' converts the function value
  // in place, where 'unsafeBitCast' would wrap it in two reabstraction thunks
  let unisolatedBody: () -> Void = Builtin.reinterpretCast(body)
  unsafe _enqueueOnewayUnchecked(
    on: isolation.unownedExecutor, isolation as AnyObject
  ) { _ in
    unisolatedBody()
  }
}
#endif // $Embedded
