//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
//
//===----------------------------------------------------------------------===//

import _Concurrency

@available(SwiftStdlib 6.4, *)
public func withContinuousObservation(
  options: ObservationTracking.Options,
  @_inheritActorContext apply: @isolated(any) @Sendable @escaping (borrowing ObservationTracking.Event) -> Void
) -> ObservationTracking.Token {
  let observation = ContinuousObservation(options: options, apply)
  return observation.token
}

@available(SwiftStdlib 6.4, *)
extension ObservationTracking {
  @available(SwiftStdlib 6.4, *)
  public struct Token: ~Copyable {
    fileprivate var state: _ManagedCriticalState<ContinuousObservation.State>

    @available(SwiftStdlib 6.4, *)
    public consuming func cancel() {
        ContinuousObservation.State.cancel(state)
    }

    deinit {
        ContinuousObservation.State.cancel(state)
    }
  }
}

@available(SwiftStdlib 6.4, *)
struct ContinuousObservation: ~Copyable {
  // Tracks whether the didSet of the tracking has occurred (kind of).
  // Whether it has come to the point of suspension after the isolation. "Next suspension on
  // the isolation after the willSet"
  struct State: Sendable {
    // When `true`, this means to keep running because there is more to do (wait for the
    // next suspension point).
    var continuation: UnsafeContinuation<Bool, Never>?
    var isInitial: Bool = true
    // Tracks whether a change occurred before the cancellation, but has not called the
    // synchronize closure yet. This allows that `synchronize` to be called for that value still.
    var dirty = false
    var cancelled = false
    var event: (ObservationTracking.Event.Kind, ObservationTracking?)?
  }

  fileprivate let state = _ManagedCriticalState(State())

  // Initialize with a closure where you access all properties you wish for changes to be tracked.
  // Inside the closure, you should use the properties and transform them to set on other objects.
  // This closure will be called continuously whenever any tracked property changes to a new
  // value.
  //
  // - Note: The closure will be called isolated to the same actor this type is initialized on.
  // - Parameters:
  //     - synchronize: A closure that will be called whenever any of the tracked properties
  //     change.
  //         - `context`: Information as to the current iteration to allow you to change your code.
  //         For instance, `isInitial` so you can choose to do something difference for the initial
  //         value rather than when it changes.
  init(
    options: ObservationTracking.Options,
    @_inheritActorContext _ apply: @isolated(any) @Sendable @escaping (borrowing ObservationTracking.Event) -> Void
  ) {
    ContinuousObservation.run(state, options: options, apply: apply)
  }

  var token: ObservationTracking.Token {
    ObservationTracking.Token(state: state)
  }
}

@available(SwiftStdlib 6.4, *)
extension ContinuousObservation.State {
  fileprivate static func emitEvent(
    _ state: _ManagedCriticalState<ContinuousObservation.State>,
    _ event: borrowing ObservationTracking.Event
  ) {
    let kind = event.kind
    let tracking = event.tracking
    let (continuation, terminal) = state.withCriticalRegion {
      state -> (UnsafeContinuation<Bool, Never>?, Bool) in
      let continuation = state.continuation
      state.isInitial = false
      state.dirty = true
      state.continuation = nil
      state.event = (kind, tracking)
      return (continuation, state.cancelled)
    }
    if let continuation {
      continuation.resume(returning: !terminal)
    }
  }

  // If the return value is true, you should continue to listen for the next change. If false,
  // you should end immediately and not listen to the next change.
  fileprivate static func populate(
    _ state: _ManagedCriticalState<ContinuousObservation.State>,
    continuation: UnsafeContinuation<Bool, Never>
  ) -> Bool {
    // Continuation stays suspend until you get the new willSet trigger and the value is whether
    // you should keep observing.
    let (continuation, dirty) = state.withCriticalRegion {
      state -> (UnsafeContinuation<Bool, Never>?, Bool) in
      assert(state.continuation == nil)
      let dirty = state.dirty
      state.dirty = false
      if state.cancelled {
        return (continuation, dirty)
      } else {
        state.continuation = continuation
        return (nil, dirty)
      }
    }
    if let continuation {
      // This is an early resume saying this is cancelled
      continuation.resume(returning: false)
      // If something already hit the willSet (dirty == true), then continue with one more
      // value change.
      return dirty
    } else {
      return true
    }
  }

  static func cancel(_ state: _ManagedCriticalState<ContinuousObservation.State>)
  {
    state.withCriticalRegion { state in
      let continuation = state.continuation
      state.cancelled = true
      state.continuation = nil
      return continuation
    }?.resume(returning: false)
  }

  // This will sit on an iteration of the loop until a `willSet`
  // occurs for one of the tracked properties. Then, it will perform
  // another iteration of the loop.
  //
  // Taking in the isolation ensures that we don't hop to another actor.
  fileprivate static func trackingLoop(
    isolation: isolated (any Actor)?,
    _ state: _ManagedCriticalState<ContinuousObservation.State>,
    options: ObservationTracking.Options,
    apply:
      @isolated(any) @escaping @Sendable (
        borrowing ObservationTracking.Event
      ) -> Void
  ) async {
    while await track(state, options: options, apply: apply) {}
  }

  fileprivate static func track(
    _ state: _ManagedCriticalState<ContinuousObservation.State>,
    options: ObservationTracking.Options,
    apply:
      @isolated(any) @escaping @Sendable (
        borrowing ObservationTracking.Event
      ) -> Void
  ) async -> Bool {
    return await withTaskCancellationHandler(
      operation: {
        return await withUnsafeContinuation(isolation: apply.isolation) {
          continuation in
          guard
            ContinuousObservation.State.populate(state, continuation: continuation)
          else {
            return
          }
          withObservationTracking(options: options) {
            // This is safe since we have already been isolated to the tracking isolation.
            // It can be asserted to be isolated by
            apply.isolation?.assertIsolated()
            let fn = apply as @Sendable (borrowing ObservationTracking.Event) -> Void
            // This ends up also being how the `didSet` is called because this will occur
            // on the next iteration of the while loop from `trackingLoop` after the
            // onChange.
            let kindAndTracking = state.withCriticalRegion { state in
              defer { state.event = nil }
              return state.event
            }
            if let kindAndTracking {
              fn(.init(kindAndTracking.1, continuousState: state, kind: kindAndTracking.0))
            } else {
              fn(.init(nil, continuousState: state, kind: .initial))
            }
          } onChange: { event in
            // This will trigger this whole `track` method to be called again, causing the
            // `apply` closure to run.
            ContinuousObservation.State.emitEvent(state, event)
          }
        }
      },
      onCancel: {
        ContinuousObservation.State.cancel(state)
      },
      isolation: apply.isolation
    )
  }
}

// MARK: - Runner
@available(SwiftStdlib 6.4, *)
extension ContinuousObservation {
  // This is representing the state of the runner, which is spawning the task that's responsible
  // for suspending (to wait after willSet) and resuming.
  fileprivate struct RunnerState: @unchecked Sendable {
    enum RunnerAction {
      // Start looking at something
      case startSynchronizing(
        _ManagedCriticalState<State>,
        ObservationTracking.Options,
        @isolated(any) @Sendable (borrowing ObservationTracking.Event) -> Void
      )
      // Done looking at something
      case finishedSynchronization
    }

    // Set to true when the `Task` is created
    var running = false
    var continuation: AsyncStream<RunnerAction>.Continuation
    var iterator: AsyncStream<RunnerAction>.Iterator?

    init() {
      let (stream, continuation) = AsyncStream.makeStream(of: RunnerAction.self)
      self.continuation = continuation
      self.iterator = stream.makeAsyncIterator()
    }
  }

  fileprivate static let runnerState = _ManagedCriticalState(RunnerState())
}

// This is needed just to make the `iterator` `Sendable`.
// This is needed for the Coat Check algorithm.
private struct UnsafeBox<Element>: @unchecked Sendable {
  var value: Element
  init(_ value: Element) {
    self.value = value
  }
}

@available(SwiftStdlib 6.4, *)
extension ContinuousObservation.RunnerState {
  // This says I'm waiting for something to do. I'm waiting to perform an action.
  fileprivate static func dequeue(
    _ state: _ManagedCriticalState<ContinuousObservation.RunnerState>,
    isolation: isolated (any Actor)? = #isolation
  ) async -> RunnerAction? {
    guard
      var iteratorBox = state.withCriticalRegion({ state in
        // Atomic swap checkout algorithm (Coat Check Algorithm)
        let iteratorBox = state.iterator.map { UnsafeBox($0) }
        state.iterator = nil
        return iteratorBox
      })
    else {
      fatalError("Iteration should be fully exclusive")
    }
    // At this point, the state.iterator has been set to `nil`
    // You are handing off where the iteration is being done so only one thing is ever awaiting.
    let value = await iteratorBox.value.next(isolation: isolation)
    // iterator is captured to freeze the state of the iteration from being a var
    state.withCriticalRegion { [iteratorBox] state in
      // Checks the value back in
      state.iterator = iteratorBox.value
    }
    return value
  }

  fileprivate static func enqueue(
    _ state: _ManagedCriticalState<ContinuousObservation.RunnerState>,
    action: RunnerAction
  ) {
    state.withCriticalRegion { state in
      return state.continuation
    }.yield(action)
  }
}

@available(SwiftStdlib 6.4, *)
extension ContinuousObservation {
  fileprivate static func run(
    _ state: _ManagedCriticalState<State>,
    options: ObservationTracking.Options,
    apply: @isolated(any) @Sendable @escaping (borrowing ObservationTracking.Event) -> Void
  ) {
    let shouldStart = runnerState.withCriticalRegion { state in
      let isRunning = state.running
      state.running = true
      return !isRunning
    }

    // Only the first time anything is ever trying to run
    if shouldStart {
      Task.detached(name: "ContinuousObservation") {
        await withTaskGroup { group in
          var activeDequeueCount = 0
          while true {
            // the active dequeue action should only have 1 active at any point in time
            if activeDequeueCount < 1 {
              group.addTask {
                await RunnerState.dequeue(runnerState)
              }
              activeDequeueCount += 1
            }
            // ensure that there is precisely 1 active dequeue active
            assert(activeDequeueCount == 1)
            // This gets the next action returned from the group `Task`s
            switch await group.next() {
            case .finishedSynchronization:  // this is only returned by an iteration of tracking
              break
            case .startSynchronizing(let state, let options, let apply):  // this is only emitted by enqueueing an action
              // Allow for the opportunity for another synchronization task to start
              activeDequeueCount -= 1
              group.addTask {
                // This will sit on an iteration of the loop until a `willSet`
                // occurs for one of the tracked properties. Then, it will perform
                // another iteration of the loop.
                await State.trackingLoop(
                  isolation: apply.isolation,
                  state,
                  options: options,
                  apply: apply
                )
                return .finishedSynchronization
              }
            default:
              return
            }
          }
        }
      }
    }
    RunnerState.enqueue(runnerState, action: .startSynchronizing(state, options, apply))
  }
}
