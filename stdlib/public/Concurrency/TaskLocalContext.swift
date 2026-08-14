//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

/// A type-erased snapshot of the currently visible task-local bindings that
/// can be re-applied onto an arbitrary execution context via ``withValues(_:)``.
///
/// `TaskLocalContext` gives external libraries the same task-local propagation
/// that `Task.init { … }` performs implicitly: it copies the current bindings
/// so they can be observed later — even from an unstructured execution
/// context, a callback dispatched on a plain thread, or a `Task.detached`
/// where inheritance is otherwise deliberately suppressed.
///
/// The snapshot captures the *most-specific* value for each bound key,
/// respecting shadowing. Values are stored by value-witness copy, so classes
/// are retained and value types are copied at capture time.
///
/// ### Example
///
///     enum TL {
///       @TaskLocal static var traceID: String?
///     }
///
///     // Somewhere with bindings in effect:
///     let ctx = TL.$traceID.withValue("abc-123") {
///       TaskLocalContext.current    // snapshot everything visible here
///     }
///
///     // Later, in an unrelated execution context:
///     Task.detached {
///       // No bindings inherited from anywhere.
///       ctx.withValues {
///         precondition(TL.traceID == "abc-123")
///       }
///     }
///
/// ### Sendable
///
/// `TaskLocalContext` is `@unchecked Sendable`: values already had to satisfy
/// `TaskLocal<Value: Sendable>` at capture time, and the underlying snapshot
/// buffer is immutable after capture. The `@unchecked` is required only
/// because the backing storage class holds an `UnsafeMutableRawPointer?` to
/// the runtime-owned snapshot, which is not automatically `Sendable`.
///
/// ### Interaction with a stop-lookup scope
///
/// Capture respects any active runtime stop-lookup marker (used e.g. by the
/// isolated-deinit fast path), so a snapshot taken inside such a scope
/// reflects only what was visible above the marker — possibly empty.
///
/// - SeeAlso: ``TaskLocal``
@available(SwiftStdlib 6.5, *)
public struct TaskLocalContext: @unchecked Sendable {
  @usableFromInline
  internal let _storage: _TaskLocalContextStorage

  /// Snapshot the currently visible task-local bindings.
  ///
  /// If invoked from a context with no visible bindings (e.g. a plain thread
  /// that never set any task locals), returns an empty context whose
  /// ``withValues(_:)-8y52u`` runs the body unchanged.
  public static var current: TaskLocalContext {
    TaskLocalContext(_captureCurrent: ())
  }

  /// An empty context. `withValues` runs the body without pushing anything.
  public init() {
    self.init(_storage: _TaskLocalContextStorage.empty)
  }

  @usableFromInline
  internal init(_storage: _TaskLocalContextStorage) {
    self._storage = _storage
  }

  @inlinable
  internal init(_captureCurrent: ()) {
    let raw = unsafe _swift_task_localsCopyToSnapshot()
    // Fast path: the runtime returns nullptr when there are no visible
    // bindings — reuse the process-wide `.empty` singleton instead of
    // allocating a fresh storage class just to hold a nil pointer.
    guard let raw = unsafe raw else {
      self.init(_storage: _TaskLocalContextStorage.empty)
      return
    }
    self.init(_storage: unsafe _TaskLocalContextStorage(adopting: raw))
  }

  /// The number of distinct task-local keys captured in this snapshot.
  public var count: Int { _storage.count }

  /// Whether the snapshot captured any bindings.
  public var isEmpty: Bool { _storage.count == 0 }

  /// Push all captured bindings for the duration of the synchronous
  /// `operation`, then restore prior state — analogous to
  /// ``TaskLocal/withValue(_:operation:file:line:)`` but for every captured
  /// binding at once.
  ///
  /// Bindings pushed here compose with any bindings already in effect at the
  /// call site: those with keys not present in the snapshot are unaffected;
  /// those with keys present are shadowed by the snapshotted value for the
  /// scope of `operation`.
  @discardableResult
  public func withValues<R>(
    _ operation: () throws -> R,
    file: String = #fileID, line: UInt = #line
  ) rethrows -> R {
    let pushed = _storage.pushAll()
    defer { _storage.popAll(pushed) }
    return try operation()
  }

  /// Push all captured bindings for the duration of the asynchronous
  /// `operation`, then restore prior state.
  ///
  /// The operation is guaranteed to execute in the calling context (see
  /// `nonisolated(nonsending)`), so bindings pushed before `operation` runs
  /// are visible inside it without an executor hop.
  @discardableResult
  public nonisolated(nonsending) func withValues<R>(
    _ operation: nonisolated(nonsending) () async throws -> R,
    file: String = #fileID, line: UInt = #line
  ) async rethrows -> R {
    let pushed = _storage.pushAll()
    defer { _storage.popAll(pushed) }
    return try await operation()
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Internal storage

/// Reference-counted holder for the opaque runtime snapshot pointer.
///
/// A class is used so that ARC reliably runs `deinit` (and therefore the
/// runtime destroy) exactly once regardless of how the surrounding
/// `TaskLocalContext` value is copied.
@available(SwiftStdlib 6.5, *)
@usableFromInline
@safe
internal final class _TaskLocalContextStorage: @unchecked Sendable {
  /// Opaque `TaskLocal::Snapshot *`; nil for empty contexts.
  @usableFromInline
  internal let snapshot: UnsafeMutableRawPointer?

  @usableFromInline
  internal static let empty: _TaskLocalContextStorage =
    unsafe _TaskLocalContextStorage(adopting: nil)

  @usableFromInline
  internal init(adopting raw: UnsafeMutableRawPointer?) {
    unsafe self.snapshot = raw
  }

  /// Number of distinct keys captured in the snapshot. Reads from the runtime
  /// each time — the underlying access is just a `size_t` field read on the
  /// snapshot header, cheaper than caching a copy in this class.
  @usableFromInline
  internal var count: Int {
    guard let s = unsafe snapshot else { return 0 }
    return unsafe _swift_task_localsSnapshotCount(s)
  }

  @usableFromInline
  internal func pushAll() -> Int {
    guard let s = unsafe snapshot else { return 0 }
    return unsafe _swift_task_localsSnapshotPush(s)
  }

  @usableFromInline
  internal func popAll(_ n: Int) {
    guard n > 0 else { return }
    _swift_task_localsSnapshotPop(n)
  }

  deinit {
    if let s = unsafe snapshot {
      unsafe _swift_task_localsSnapshotDestroy(s)
    }
  }
}

// ==== -----------------------------------------------------------------------
// MARK: Runtime shims

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_task_localsCopyToSnapshot")
internal func _swift_task_localsCopyToSnapshot() -> UnsafeMutableRawPointer?

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_task_localsSnapshotCount")
internal func _swift_task_localsSnapshotCount(
  _ snapshot: UnsafeMutableRawPointer
) -> Int

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_task_localsSnapshotPush")
internal func _swift_task_localsSnapshotPush(
  _ snapshot: UnsafeMutableRawPointer
) -> Int

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_task_localsSnapshotPop")
internal func _swift_task_localsSnapshotPop(_ count: Int)

@available(SwiftStdlib 6.5, *)
@usableFromInline
@_silgen_name("swift_task_localsSnapshotDestroy")
internal func _swift_task_localsSnapshotDestroy(
  _ snapshot: UnsafeMutableRawPointer
)
