// RUN: %target-swift-frontend -typecheck -strict-concurrency=complete -target %target-swift-5.1-abi-triple %s

// REQUIRES: concurrency

// We used to crash on this when processing double curry thunks. Make sure that
// we do not do crash in the future.
extension AsyncStream {
  @Sendable func myCancel() {
  }
  func myNext2(_ continuation: UnsafeContinuation<Element?, Never>) {
  }
  func myNext() async -> Element? {
    await withTaskCancellationHandler {
      unsafe await withUnsafeContinuation {
        unsafe myNext2($0)
      }
    } onCancel: { [myCancel] in
      myCancel()
    }
  }
}
