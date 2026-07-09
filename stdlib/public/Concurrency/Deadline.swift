//===----------------------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

import Swift

@available(SwiftStdlib 6.5, *)
fileprivate struct Envelope<T: ~Copyable & ~Escapable>: ~Copyable, ~Escapable, @unchecked Sendable {
  var contents: T
}

fileprivate extension Optional where Wrapped: ~Copyable & ~Escapable {
  @available(SwiftStdlib 6.5, *)
  @_lifetime(copy self)
  mutating func takeSending() -> Envelope<Self> {
    let result = consume self
    self = nil
    return Envelope(contents: result)
  }
}

@available(SwiftStdlib 6.5, *)
fileprivate struct Disconnected<Value: ~Copyable>: ~Copyable, Sendable {
  private nonisolated(unsafe) var value: Value?
  
  init(value: consuming sending Value) {
    self.value = .some(value)
  }
  
  consuming func take() -> sending Value {
    self.value.takeSending().contents!
  }
}

@available(SwiftStdlib 6.5, *)
fileprivate final class RefBox<Value: ~Copyable> {
  private nonisolated(unsafe) var value: Value?
  
  init(value: consuming Value) {
    self.value = consume value
  }
  
  consuming func unbox() -> Value {
    return value.take()!
  }
}

@available(SwiftStdlib 6.5, *)
extension RefBox: Sendable where Value: Sendable & ~Copyable {}

@available(SwiftStdlib 6.5, *)
fileprivate enum TaskResult<Return: Sendable, Failure: Error>: Sendable {
  case success(Return)
  case error(Failure)
  case timedOut
  case cancelled
}

@available(SwiftStdlib 6.5, *)
fileprivate protocol _DeadlineStorage: Sendable {
  associatedtype C: Clock & Identifiable
  
  var expiration: C.Instant { get }
  var tolerance: C.Instant.Duration? { get }
  var clock: C { get }
}

@available(SwiftStdlib 6.5, *)
fileprivate struct DeadlineStorage<C: Clock & Identifiable>: _DeadlineStorage {
  var expiration: C.Instant
  var tolerance: C.Instant.Duration?
  var clock: C
  
  var maximumExpiration: C.Instant {
    if let tolerance {
      return expiration.advanced(by: tolerance)
    } else {
      return expiration
    }
  }
}

@available(SwiftStdlib 6.5, *)
@TaskLocal fileprivate nonisolated var activeDeadlines = [any _DeadlineStorage]()

@available(SwiftStdlib 6.5, *)
fileprivate nonisolated(nonsending) func __withDeadline<Return: ~Copyable, Failure: Error, C: Clock>(
  clock: C,
  until expiration: C.Instant,
  tolerance: C.Instant.Duration?,
  body: inout (@Sendable () async throws(Failure) -> Return)?
) async throws(Failure) -> Return {
  let result: Result<RefBox<Disconnected<Return>>, Failure> = await withTaskGroup(
    of: TaskResult<RefBox<Disconnected<Return>>, Failure>.self
  ) { group in
    let body = body.takeSending().contents!
    group.addTask {
      do throws(Failure) {
        return .success(RefBox(value: Disconnected(value: try await body())))
      } catch {
        return .error(error)
      }
    }
    group.addTask {
      do {
        try await clock.sleep(until: expiration, tolerance: tolerance)
        return .timedOut
      } catch {
        // TODO: this should eventually check the cancellation reason to return .timedOut when the reason is .deadlineExpired
        return .cancelled
      }
    }
    
    switch await group.next()! {
    case .success(let success):
      group.cancelAll(reason: .taskCancelled)
      return .success(success)
    case .error(let failure):
      group.cancelAll(reason: .taskCancelled)
      return .failure(failure)
    case .timedOut:
      group.cancelAll(reason: .deadlineExpired)
    case .cancelled:
      break
    }
    
    switch await group.next() {
    case .success(let success):
      return .success(success)
    case .error(let failure):
      return .failure(failure)
    default:
      // task group executed timeout task twice!
      fatalError("Internal inconsistency")
    }
  }
  return try result.get().unbox().take()
}

@available(SwiftStdlib 6.5, *)
fileprivate nonisolated(nonsending) func _withDeadline<Return: ~Copyable, Failure: Error, C: Clock>(
  clock: C,
  until deadline: C.Instant,
  tolerance: C.Instant.Duration?,
  body: @Sendable () async throws(Failure) -> Return
) async throws(Failure) -> Return {
  try await withoutActuallyEscaping(body) { (escapingBody) async throws(Failure) -> Return in
    var t = Optional(escapingBody)
    return try await __withDeadline(clock: clock, until: deadline, tolerance: tolerance, body: &t)
  }
}

extension Task where Success == Never, Failure == Never {
  @available(SwiftStdlib 6.5, *)
  public static var hasActiveDeadline: Bool {
    !activeDeadlines.isEmpty
  }
  
  @available(SwiftStdlib 6.5, *)
  public static func activeDeadline<C: Clock & Identifiable>(for clock: C) -> C.Instant? {
    for deadline in activeDeadlines {
      if let d = deadline as? DeadlineStorage<C>, d.clock.id == clock.id {
        return d.expiration
      }
    }
    return nil
  }
}

@available(SwiftStdlib 6.5, *)
@_unavailableInEmbedded
public nonisolated(nonsending) func withDeadline<Return: ~Copyable, Failure: Error, C: Clock & Identifiable>(
  _ expiration: C.Instant,
  tolerance: C.Instant.Duration? = nil,
  clock: C = ContinuousClock(),
  body: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return {
  nonisolated(unsafe) let body = body
  
  let proposedDeadline = DeadlineStorage(expiration: expiration, tolerance: tolerance, clock: clock)
  var deadlines = activeDeadlines
  // find and adjust any active deadlines
  for (index, deadline) in deadlines.enumerated() {
    if let d = deadline as? DeadlineStorage<C>, d.clock.id == clock.id {
      if proposedDeadline.maximumExpiration < d.maximumExpiration {
        deadlines[index] = proposedDeadline
        // swap in the proposed deadline in the same hiearchy as the existing one
        let result: RefBox<Result<Return, Failure>> = await $activeDeadlines.withValue(deadlines) {
          do throws(Failure) {
            return RefBox(value: .success(
              try await _withDeadline(
                clock: clock,
                until: expiration,
                tolerance: tolerance,
              ) { () async throws(Failure) -> Return in
                try await body()
              }
            ))
          } catch {
            return RefBox(value: .failure(error))
          }
        }
        return try result.unbox().get()
      } else {
        // optimized such that the active deadline is obviated by an existing one
        return try await body()
      }
    }
  }
  // no matching active deadline was found, build up a new list for this application
  // normal application
  deadlines.append(proposedDeadline)
  let result: RefBox<Result<Return, Failure>> = await $activeDeadlines.withValue(deadlines) {
    do throws(Failure) {
      return RefBox(value: .success(
        try await _withDeadline(
          clock: clock,
          until: expiration,
          tolerance: tolerance,
        ) { () async throws(Failure) -> Return in
          try await body()
        }
      ))
    } catch {
      return RefBox(value: .failure(error))
    }
  }
  return try result.unbox().get()
}

@available(SwiftStdlib 6.5, *)
@_unavailableInEmbedded
public nonisolated(nonsending) func withDeadline<Return: ~Copyable, Failure: Error>(
  in timeout: ContinuousClock.Instant.Duration,
  tolerance: ContinuousClock.Instant.Duration? = nil,
  body: nonisolated(nonsending) () async throws(Failure) -> Return
) async throws(Failure) -> Return {
  try await withDeadline(.now.advanced(by: timeout), tolerance: tolerance, clock: ContinuousClock(), body: body)
}
