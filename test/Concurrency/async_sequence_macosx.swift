// RUN: %target-swift-frontend -target %target-cpu-apple-macos14.0 %s -emit-sil -o /dev/null -verify

// REQUIRES: concurrency, OS=macosx

func acceptClosure(_: () async throws -> Void) { }
  
@available(macOS 13.0, *)
func f<S: AsyncSequence>(s: S) async throws {
  acceptClosure {
    if #available(SwiftStdlib 6.0, *) {
      for try await x in s {
        print(x)
      }
    }
  }
}


