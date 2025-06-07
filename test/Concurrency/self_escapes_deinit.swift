// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -target %target-swift-5.1-abi-triple

@MainActor
class C {
  let x: Int = 0

  deinit {
    // expected-warning@+1 {{capture of 'self' in a closure that outlives deinit; this is an error in the Swift 6 language mode}}
    Task { @MainActor in
      _ = self
    }

    // expected-warning@+1 {{capture of 'self' in a closure that outlives deinit; this is an error in the Swift 6 language mode}}
    Task {
      _ = x
    }
  }
}

func enqueueSomewhereElse(_ closure: @escaping @Sendable () -> Void) {}

@MainActor
class C2 {
  let x: Int = 0

  deinit {
    // expected-warning@+1 {{capture of 'self' in a closure that outlives deinit; this is an error in the Swift 6 language mode}}
    enqueueSomewhereElse {
      _ = self
    }

    // expected-warning@+1 {{capture of 'self' in a closure that outlives deinit; this is an error in the Swift 6 language mode}}
    enqueueSomewhereElse {
      _ = self.x
    }
  }
}
