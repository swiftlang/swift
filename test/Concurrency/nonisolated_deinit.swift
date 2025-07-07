// RUN: %target-typecheck-verify-swift -swift-version 5 %s -strict-concurrency=complete -target %target-swift-5.1-abi-triple

// REQUIRES: concurrency
// REQUIRES: OS=macosx

class NotSendable {}

@MainActor class C {
  var x: Int = 0

  nonisolated deinit {
    print(x)
  }
}

@globalActor
actor SomeGlobalActor {
  static let shared = SomeGlobalActor()
}

// expected-note@+1{{add '@available' attribute to enclosing class}}
@SomeGlobalActor class C2 {
  var x: Int = 0

  isolated deinit { // expected-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@MainActor class C3 {
  var x: Int = 0

  isolated deinit { // okay, this back-deploys
    print(x)
  }
}
