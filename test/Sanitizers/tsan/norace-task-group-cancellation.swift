// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library -sanitize=thread)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: libdispatch
// REQUIRES: tsan_runtime
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: use_os_stdlib

// Bug in TSan on FreeBSD
// Thread destruction interceptor marks the thread ignored and then checks that
// the thread isn't being ignored.
// rdar://158450231
// XFAIL: OS=freebsd

@main
public struct TSANDataRaceOnCancel {
  public static func main() async throws {
    for _ in 0 ..< 100 {
      try await withThrowingTaskGroup(of: Void.self) { group in
        group.addTask {
          let thing = Thing(otherThing: OtherThing())

          try await withTaskCancellationHandler {
            try await Task.sleep(nanoseconds: 100_000_000_000)
          } onCancel: {
            thing.cancel()
          }
        }

        // Wait a little bit so the task is scheduled before cancelling.
        try await Task.sleep(nanoseconds: 10_000)
        group.cancelAll()
      }
    }
  }
}

final class Thing: Sendable {
  private let otherThing: OtherThing

  init(otherThing: OtherThing) {
    self.otherThing = otherThing // Write of size 8 by thread Y
  }

  func cancel() {
    self.otherThing.cancel() // Read of size 8 by thread X
  }
}

final class OtherThing: Sendable {
  func cancel() {}
}
