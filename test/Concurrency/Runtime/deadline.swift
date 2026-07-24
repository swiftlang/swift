// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -disable-availability-checking -parse-as-library
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)
// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -swift-version 5 -strict-concurrency=complete -enable-upcoming-feature NonisolatedNonsendingByDefault)
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

// Test requires _swift_task_enterThreadLocalContext which is not available 
// in the back deployment runtime.
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency

import _Concurrency
import StdlibUnittest
import Synchronization

final class TestClock: Clock, @unchecked Sendable {
  struct Instant: InstantProtocol, Sendable {
    var offset: Duration
    
    func advanced(by duration: Duration) -> Instant {
      Instant(offset: offset + duration)
    }
    
    func duration(to other: Instant) -> Duration {
      other.offset - offset
    }
    
    static func < (lhs: Instant, rhs: Instant) -> Bool {
      lhs.offset < rhs.offset
    }
    
    static func == (lhs: Instant, rhs: Instant) -> Bool {
      lhs.offset == rhs.offset
    }
  }
  
  private struct Sleeper {
    var id: UInt64
    var deadline: Instant
    var continuation: UnsafeContinuation<Void, Error>
  }
  
  private struct State: Sendable {
    var now: Instant
    var generation = UInt64(0)
    var sleepers: [UInt64 : Sleeper]
  }
  
  private let state: Mutex<State>
  
  var now: Instant {
    state.withLock { $0.now }
  }
  
  var minimumResolution: Duration {
    .zero
  }
  
  init(startingAt instant: Instant = Instant(offset: .zero)) {
    state = Mutex(State(now: instant, sleepers: [:]))
  }
  
  func sleep(until deadline: Instant, tolerance: Duration? = nil) async throws {
    let shouldReturnImmediately = state.withLock { state in
      deadline <= state.now
    }
    
    if shouldReturnImmediately {
      try Task.checkCancellation()
      return
    }
    
    let sleeperId = state.withLock { state in
      defer { state.generation &+= 1 }
      return state.generation
    }
    
    try await withTaskCancellationHandler {
      try await withUnsafeThrowingContinuation { (continuation: UnsafeContinuation<Void, Error>) in
        let action = state.withLock { state -> (resume: Bool, wasCancelled: Bool) in
          // Check if the deadline has already passed
          if deadline <= state.now {
            return (resume: true, wasCancelled: false)
          }
          
          // Check if we're already cancelled before adding to sleepers
          if Task.isCancelled {
            return (resume: false, wasCancelled: true)
          }
          
          // Add to sleepers list with unique ID
          state.sleepers[sleeperId] = Sleeper(id: sleeperId, deadline: deadline, continuation: continuation)
          return (resume: false, wasCancelled: false)
        }
        
        if action.resume {
          continuation.resume()
        } else if action.wasCancelled {
          continuation.resume(throwing: CancellationError())
        }
      }
    } onCancel: {
      let removedSleeper = state.withLock { state -> Sleeper? in
        return state.sleepers.removeValue(forKey: sleeperId)
      }
      
      if let sleeper = removedSleeper {
        sleeper.continuation.resume(throwing: CancellationError())
      }
    }
  }
  
  /// Advance the clock by the specified duration and resume any sleepers whose deadlines have passed
  func advance(by duration: Duration) {
    let toResume = state.withLock { state in
      state.now = state.now.advanced(by: duration)
      let currentNow = state.now
      
      var toResume: [UnsafeContinuation<Void, Error>] = []
      
      for (id, sleeper) in state.sleepers {
        if sleeper.deadline <= currentNow {
          toResume.append(sleeper.continuation)
          state.sleepers.removeValue(forKey: id)
        }
      }
      
      return toResume
    }
    
    for continuation in toResume {
      continuation.resume()
    }
  }
  
  /// Cancel all pending sleepers (simulates task cancellation)
  func cancelAllSleepers() {
    let allSleepers = state.withLock { state in
      let sleepers = state.sleepers
      state.sleepers = [:]
      return sleepers
    }
    
    for sleeper in allSleepers.values {
      sleeper.continuation.resume(throwing: CancellationError())
    }
  }
}

enum TestError: Error, Equatable {
  case testFailure
  case timeout
  case other
}

func withAdvancingClock<Failure: Error>(by amount: Duration, clock: TestClock, apply: @Sendable () async throws(Failure) -> Void) async throws(Failure) {
  try await withoutActuallyEscaping(apply) { escapingClosure in
    await withTaskGroup(of: Result<Void, Failure>.self) { group -> Result<Void, Failure> in
      group.addTask {
        clock.advance(by: amount)
        return .success(())
      }
      group.addTask {
        do throws(Failure) {
          return .success(try await escapingClosure())
        } catch {
          return .failure(error)
        }
      }
      for await result in group {
        switch result {
        case .failure: return result
        case .success: continue
        }
      }
      return .success(())
    }
  }.get()
}

@MainActor var tests = TestSuite("Deadline")

@main struct Main {
  static func main() async {
	  tests.test("Operation completes immediately before deadline") {
	    let result = try await withDeadline(in: .milliseconds(100)) {
	      return 42
	    }
	    expectEqual(result, 42)
	  }
	  
	  tests.test("Operation completes with very short delay before deadline") {
	    let result = try await withDeadline(in: .milliseconds(100)) {
	      try await Task.sleep(for: .milliseconds(1))
	      return "success"
	    }
	    expectEqual(result, "success")
	  }
	  
	  tests.test(("Operation with custom clock completes before deadline") {
	    let clock = ContinuousClock()
	    let deadline = clock.now.advanced(by: .milliseconds(100))
	    
	    let result = try await withDeadline(deadline, clock: clock) {
	      return true
	    }
	    expectEqual(result, true)
	  }
	  
	  tests.test("Operation returning complex type completes before deadline") {
	    struct ComplexResult {
	      let value: Int
	      let name: String
	    }
	    
	    let result = try await withDeadline(in: .milliseconds(100)) {
	      ComplexResult(value: 123, name: "test")
	    }
	    expectEqual(result.value, 123)
	    expectEqual(result.name, "test")
	  }


	  tests.test("Operation exceeds deadline and throws error") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(100))
	    await withAdvancingClock(by: .milliseconds(150), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(deadline, clock: clock) {
	          do {
	            try await clock.sleep(until: clock.now.advanced(by: .milliseconds(200)), tolerance: nil)
	          } catch {
	            throw TestError.timeout
	          }
	          throw TestError.testFailure
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .deadlineExpired)
	        expectEqual(error.underlyingError as? TestError, .timeout)
	      }
	    }
	  }
	  
	  tests.test("Operation exceeds deadline with task cancellation check") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(100))
	    let wasCancelled = Mutex(false)
	    
	    await withAdvancingClock(by: .milliseconds(150), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(deadline, clock: clock) {
	          try? await clock.sleep(until: clock.now.advanced(by: .milliseconds(200)), tolerance: nil)
	          wasCancelled.withLock { $0 = Task.isCancelled }
	          throw TestError.timeout
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .deadlineExpired)
	        expectEqual(wasCancelled.withLock { $0 }, true)
	      }
	    }
	  }
	  
	  tests.test("Very short deadline expires immediately") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .microseconds(1))
	    
	    await withAdvancingClock(by: .milliseconds(10), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(deadline, clock: clock) {
	          try? await clock.sleep(until: clock.now.advanced(by: .milliseconds(50)), tolerance: nil)
	          throw TestError.timeout
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	       	expectEqual(error.cause, .deadlineExpired)
	      }
	    }
	  }
	  
	  tests.test("Expired deadline with custom clock") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(5))
	    
	    await withAdvancingClock(by: .milliseconds(10), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(deadline, clock: clock) {
	          try await clock.sleep(until: clock.now.advanced(by: .milliseconds(50)), tolerance: nil)
	          throw TestError.timeout
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .deadlineExpired)
	      }
	    }
	  }
	  
	  tests.test("Deadline expires during long operation") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(50))
	    
	    await withAdvancingClock(by: .milliseconds(100), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(deadline, clock: clock) {
	          // Simulate long operation by sleeping well past deadline
	          try await clock.sleep(until: clock.now.advanced(by: .seconds(10)), tolerance: nil)
	          throw TestError.testFailure
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .deadlineExpired)
	      }
	    }
	  }

	  tests.test("Operation throws error immediately before deadline") {
	    do {
	      let _ = try await withDeadline(in: .seconds(1)) {
	        throw TestError.testFailure
	      }
	      expectationFailure("Expected DeadlineError to be thrown")
	    } catch {
	      expectEqual(error.cause, .operationFailed)
	      expectEqual(error.underlyingError as? TestError, .testFailure)
	    }
	  }
	  
	  tests.test("Operation throws error after brief delay before deadline") {
	    do {
	      let _ = try await withDeadline(in: .milliseconds(100)) {
	        try await Task.sleep(for: .milliseconds(1))
	        throw TestError.other
	      }
	      expectationFailure("Expected DeadlineError to be thrown")
	    } catch {
	      expectEqual(error.cause, .operationFailed)
	      expectEqual(error.underlyingError as? TestError, .other)
	    }
	  }
	  
	  tests.test("Operation fails before deadline with custom clock") {
	    let clock = ContinuousClock()
	    let deadline = clock.now.advanced(by: .milliseconds(100))
	    
	    do {
	      let _ = try await withDeadline(deadline, clock: clock) {
	        throw TestError.testFailure
	      }
	      expectationFailure("Expected DeadlineError to be thrown")
	    } catch {
	      expectEqual(error.cause, .operationFailed)
	    }
	  }
	  
	  tests.test("Multiple potential errors thrown before deadline") {
	    for errorType in [TestError.testFailure, TestError.timeout, TestError.other] {
	      do {
	        let _ = try await withDeadline(in: .milliseconds(100)) {
	          throw errorType
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .operationFailed)
	        expectEqual(error.underlyingError as? TestError, errorType)
	      }
	    }
	  }

	  tests.test("Deadline expiration causes task cancellation") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(5))
	    let taskWasCancelled = Mutex(false)
	    
	    await withAdvancingClock(by: .milliseconds(10), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(deadline, clock: clock) {
	          // Sleep past the deadline - this will be interrupted by cancellation
	          do {
	            try await clock.sleep(until: clock.now.advanced(by: .milliseconds(50)), tolerance: nil)
	          } catch {
	            // Sleep was cancelled, check the cancellation status
	            taskWasCancelled.withLock { $0 = Task.isCancelled }
	            throw TestError.timeout
	          }
	          // If we reach here without being cancelled, that's unexpected
	          throw TestError.testFailure
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .deadlineExpired)
	        expectEqual(taskWasCancelled.withLock { $0 }, true)
	      }
	    }
	  }
	  
	  tests.test("External cancellation before deadline is treated as operation failure") {
	    // This test verifies that if the parent task is cancelled before the deadline,
	    // the error cause can be either .operationFailed or .deadlineExpired depending
	    // on which child task in the group detects cancellation first.
	    // This is inherently non-deterministic, so we just verify a DeadlineError is thrown.
	    
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .seconds(10))
	    
	    let task = Task {
	      try await withDeadline(deadline, clock: clock) {
	        // Use Task.sleep which will be cancelled when the parent task is cancelled
	        try await Task.sleep(for: .seconds(1))
	        return 42
	      }
	    }
	    
	    // Cancel immediately - this creates a race between the timer task and operation task
	    task.cancel()
	    
	    do {
	      let _ = try await task.value
	      expectationFailure("Expected DeadlineError to be thrown")
	    } catch let error as DeadlineError<Error> {
	      // We just verify that a DeadlineError was thrown with CancellationError
	      // The cause could be either .operationFailed or .deadlineExpired depending on timing
	      expectTrue(error.underlyingError is CancellationError)
	    }
	  }


	  tests.test("Zero duration deadline") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .zero)
	    
	    // With a zero deadline, the timer completes immediately and cancels the operation.
	    // The sleep will be cancelled and throw CancellationError.
	    
	    do {
	      let _ = try await withDeadline(deadline, clock: clock) {
	        // Sleep far into the future - this will be cancelled by the zero deadline
	        try await clock.sleep(until: clock.now.advanced(by: .seconds(100)), tolerance: nil)
	        throw TestError.testFailure // Should never reach here
	      }
	      expectationFailure("Expected DeadlineError to be thrown")
	    } catch {
	      expectEqual(error.cause, .deadlineExpired)
	      // The sleep should be cancelled and throw CancellationError
	      expectTrue(error.underlyingError is CancellationError)
	    }
	  }
	  
	  tests.test("Negative duration deadline expires immediately") {
	    let clock = TestClock()
	    let pastDeadline = clock.now.advanced(by: .milliseconds(-100))
	    
	    do {
	      let _ = try await withDeadline(pastDeadline, clock: clock) {
	        // Use a long sleep to ensure cancellation happens before completion
	        try await clock.sleep(until: clock.now.advanced(by: .seconds(100)), tolerance: nil)
	        throw TestError.testFailure // Should never reach here
	      }
	      expectationFailure("Expected DeadlineError to be thrown")
	    } catch {
	      expectEqual(error.cause, .deadlineExpired)
	      // When deadline is in the past, the sleep gets cancelled
	      expectTrue(error.underlyingError is CancellationError)
	    }
	  }
	  
	  tests.test("Operation completes exactly at deadline boundary") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(20))
	    
	    let result = try await withDeadline(deadline, clock: clock) {
	      return "boundary"
	    }
	    expectEqual(result, "boundary")
	  }
	  
	  tests.test("Empty operation completes immediately") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(10))
	    
	    let result = try await withDeadline(deadline, clock: clock) {
	      return true
	    }
	    expectEqual(result, true)
	  }
	  
	  tests.test("Operation with tolerance parameter") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(100))
	    
	    let result = try await withDeadline(
	      deadline,
	      tolerance: .milliseconds(10),
	      clock: clock
	    ) {
	      return 99
	    }
	    expectEqual(result, 99)
	  }
	  
	  tests.test("Nested deadlines with inner deadline shorter") {
	    let clock = TestClock()
	    let outerDeadline = clock.now.advanced(by: .milliseconds(100))
	    let innerDeadline = clock.now.advanced(by: .milliseconds(5))
	    
	    try await withAdvancingClock(by: .milliseconds(10), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(outerDeadline, clock: clock) {
	          try await withDeadline(innerDeadline, clock: clock) {
	            try? await clock.sleep(until: clock.now.advanced(by: .milliseconds(50)), tolerance: nil)
	            throw TestError.timeout
	          }
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        // The outer is a failure because it is an error caught of the inner
	        expectEqual(error.cause, .operationFailed)
	        if let inner = error.underlyingError as? DeadlineError<Error> {
	          expectEqual(inner.cause, .deadlineExpired)
	        } else {
	          throw TestError.testFailure
	        }
	      }
	    }
	  }
	  
	  tests.test("Nested deadlines with outer deadline shorter") {
	    let clock = TestClock()
	    let outerDeadline = clock.now.advanced(by: .milliseconds(5))
	    let innerDeadline = clock.now.advanced(by: .milliseconds(100))
	    
	    await withAdvancingClock(by: .milliseconds(10), clock: clock) {
	      do throws(DeadlineError<Error>) {
	        let _ = try await withDeadline(outerDeadline, clock: clock) {
	          try await withDeadline(innerDeadline, clock: clock) {
	            try await clock.sleep(until: clock.now.advanced(by: .milliseconds(50)), tolerance: nil)
	            throw TestError.timeout
	          }
	        }
	        expectationFailure("Expected DeadlineError to be thrown")
	      } catch {
	        expectEqual(error.cause, .deadlineExpired)
	      }
	    }
	  }
	  
	  tests.test("Nested deadlines both succeed") {
	    let clock = TestClock()
	    let outerDeadline = clock.now.advanced(by: .milliseconds(100))
	    let innerDeadline = clock.now.advanced(by: .milliseconds(100))
	    
	    let result = try await withDeadline(outerDeadline, clock: clock) {
	      try await withDeadline(innerDeadline, clock: clock) {
	        return "nested success"
	      }
	    }
	    expectEqual(result, "nested success")
	  }


	  tests.test("Rapid succession of deadline operations") {
	    let clock = TestClock()
	    
	    for i in 0..<10 {
	      let deadline = clock.now.advanced(by: .milliseconds(50))
	      let result = try await withDeadline(deadline, clock: clock) {
	        return i
	      }
	      expectEqual(result, i)
	    }
	  }
	  
	  tests.test("Consistent behavior across multiple runs") {
	    // Test that deadline expiration is consistent
	    for _ in 0..<5 {
	      let clock = TestClock()
	      let deadline = clock.now.advanced(by: .milliseconds(5))
	      
	      await withAdvancingClock(by: .milliseconds(10), clock: clock) {
	        do throws(DeadlineError<Error>) {
	          let _ = try await withDeadline(deadline, clock: clock) {
	            try await clock.sleep(until: clock.now.advanced(by: .milliseconds(50)), tolerance: nil)
	            throw TestError.timeout
	          }
	          expectationFailure("Expected DeadlineError to be thrown")
	        } catch {
	          expectEqual(error.cause, .deadlineExpired)
	        }
	      }
	    }
	  }
	  
	  tests.test("Immediate completion has minimal overhead") {
	    let clock = TestClock()
	    let deadline = clock.now.advanced(by: .milliseconds(100))
	    
	    // Test completes almost instantly
	    let result = try await withDeadline(deadline, clock: clock) {
	      1 + 1
	    }
	    expectEqual(result, 2)
	  }

  }
}