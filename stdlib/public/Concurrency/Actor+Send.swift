//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

// ==== -----------------------------------------------------------------------
// MARK: Support for 'send' calls

/// Run the synchronous `body` from a new discarding task, which starts on
/// `executor` or, if it is `nil`, on the global concurrent executor, without
/// waiting for it.
///
/// The task copies the task locals of the caller and inherits its priority
/// the same way `Task.init` does.
///
/// The operation of the task is `body` itself, a synchronous function: the
/// task calls it without an async context, and has no result storage or task
/// allocator slab of its own, so the only other allocation is the closure
/// context of `body`.
///
/// ## Availability of optimizations
/// Runtimes before Swift 6.5 cannot run a task with a
/// synchronous operation, so there it is a task whose async operation calls
/// `body` and never suspends.
///
/// `body` must be isolated to `executor`, or nonisolated if it is `nil`; this
/// is not checked.
///
/// SPI: used by the compiler, through the other '_enqueueOneway' functions,
/// to lower fire-and-forget calls to synchronous functions. Do not use
///
/// All '_enqueueOneway' functions are SPI: they hand a closure which is
/// neither 'sending' nor '@Sendable' to another executor, which is only sound
/// for the closures the compiler builds for a 'send' call. This function and
/// '_enqueueOnewayUnchecked' are also '@unsafe'. '_enqueueOneway' and
/// '_enqueueOnewayIsolatedAny', which the lowering of a 'send' call uses, are not,
/// so that a 'send' does not warn under '-strict-memory-safety'. The compiler
/// finds them by a direct lookup in this module, so a 'send' call does not
/// need the SPI import
@_spi(OnewaySend)
@available(SwiftStdlib 5.1, *)
@export(implementation)
@unsafe
public func _enqueueOnewayOperation(
  on executor: UnownedSerialExecutor?,
  _ body: sending @escaping () -> Void
) {
  // The operation below only ever runs once, on 'executor', after this
  // function returned, and nothing else uses 'body'
  nonisolated(unsafe) let capturedBody = body

  let builtinSerialExecutor: Builtin.Executor? = unsafe executor?.executor

#if $BuiltinCreateSynchronousDiscardingTask
  let synchronousFlags = taskCreateFlags(
    priority: nil, isChildTask: false, copyTaskLocals: true,
    inheritContext: false, enqueueJob: true,
    addPendingGroupTaskUnconditionally: false,
    isDiscardingTask: true, isSynchronousStart: false,
    hasSynchronousOperation: true)
#if $Embedded
  // Fire-and-forget: drop our reference to the task right away
  _ = Builtin.createSynchronousDiscardingTask(
    flags: synchronousFlags,
    initialSerialExecutor: builtinSerialExecutor,
    operation: capturedBody)
  return
#else
  if #available(anyAppleOS 9999, *) { // SwiftStdlib 6.5
    // Fire-and-forget: drop our reference to the task right away
    _ = Builtin.createSynchronousDiscardingTask(
      flags: synchronousFlags,
      initialSerialExecutor: builtinSerialExecutor,
      operation: capturedBody)
    return
  }
#endif
#endif

#if !$Embedded || !$BuiltinCreateSynchronousDiscardingTask
  let flags = taskCreateFlags(
    priority: nil, isChildTask: false, copyTaskLocals: true,
    inheritContext: false, enqueueJob: true,
    addPendingGroupTaskUnconditionally: false,
    isDiscardingTask: true, isSynchronousStart: false)

  // Starts on 'executor' (the task's initial serial executor) and must not
  // hop off it: '@_unsafeInheritExecutor' suppresses the hop to the generic
  // executor that a nonisolated async function would otherwise start with.
  // It is 'throws' only to match the task operation type exactly, so that
  // no reabstraction thunk (with its own suspension point) is needed
  @_unsafeInheritExecutor
  func _unsafeInheritExecutor_onewayOperation() async throws {
    capturedBody()
  }

  // Fire-and-forget: drop our reference to the task right away
  _ = Builtin.createDiscardingTask(
    flags: flags,
    initialSerialExecutor: builtinSerialExecutor,
    operation: _unsafeInheritExecutor_onewayOperation)
#endif
}

/// Run `body(value)` on `executor` from a new discarding task, without
/// waiting for it. See ``_enqueueOnewayOperation(on:_:)``.
///
/// `body` must be isolated to `executor`; this is not checked.
///
/// `Value` is class-constrained so that the task's operation captures it as
/// a single direct reference. An unconstrained generic value would be
/// captured indirectly, and the generic specializer would then need a
/// reabstraction thunk around the specialized operation.
///
/// SPI: used by the compiler to lower fire-and-forget calls to synchronous
/// actor-isolated functions. Do not use
@_spi(OnewaySend)
@available(SwiftStdlib 5.1, *)
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
  unsafe _enqueueOnewayOperation(on: executor) {
    capturedBody(capturedValue)
  }
}

/// Run the synchronous `body` on `actor` from a new discarding task, without
/// waiting for it.
///
/// The task copies the task locals of the caller and starts on the actor's
/// executor, so calls enqueued on the same actor run in the order they were
/// enqueued. See ``_enqueueOnewayUnchecked(on:_:_:)``.
///
/// SPI: used by the compiler to lower `send` calls to synchronous actor
/// methods. Do not use
@_spi(OnewaySend)
@available(SwiftStdlib 5.1, *)
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
/// SPI: used by the compiler to lower `send` calls to synchronous
/// global-actor-isolated functions. Do not use
@_spi(OnewaySend)
@available(SwiftStdlib 6.0, *)
@export(implementation)
public func _enqueueOnewayIsolatedAny(
  _ body: @escaping @isolated(any) () -> Void
) {
  guard let isolation = Builtin.extractFunctionIsolation(body) else {
    fatalError("'send' call without an isolation to enqueue it on")
  }
  // The body only ever runs on its isolation's executor, so it is safe to
  // call it as a plain synchronous function there. An '@isolated(any)'
  // function value has the same representation as any other thick function:
  // its isolation is stored inside the closure context, which the invocation
  // function receives as usual, so dropping the '@isolated(any)' is a plain
  // 'convert_function'. 'Builtin.reinterpretCast' converts the function value
  // in place, where 'unsafeBitCast' would wrap it in two reabstraction thunks
  // The isolation is stored in the closure context, which keeps the actor
  // alive until the task ran 'body'
  let unisolatedBody: () -> Void = Builtin.reinterpretCast(body)
  nonisolated(unsafe) let sentBody = unisolatedBody
  unsafe _enqueueOnewayOperation(on: isolation.unownedExecutor, sentBody)
}
