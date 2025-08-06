// RUN: %target-typecheck-verify-swift -swift-version 5 %s -strict-concurrency=complete -target %target-swift-5.1-abi-triple -verify-additional-prefix deployment- -verify-additional-prefix inlining-
// RUN: %target-typecheck-verify-swift -swift-version 5 %s -strict-concurrency=complete -target %target-swift-6.1-abi-triple
// RUN: %target-typecheck-verify-swift -swift-version 5 %s -strict-concurrency=complete -target %target-swift-6.1-abi-triple -target-min-inlining-version min -verify-additional-prefix inlining-

// REQUIRES: concurrency
// REQUIRES: OS=macosx

@MainActor class C {
  var x: Int = 0

  nonisolated deinit {
    print(x)
  }
}

@MainActor public class C1 {
  var x: Int = 0

  nonisolated deinit {
    print(x)
  }
}

@MainActor class C2 {
  var x: Int = 0

  isolated deinit { // okay, this back-deploys
    print(x)
  }
}

@MainActor public class C3 {
  var x: Int = 0

  isolated deinit { // okay, this back-deploys
    print(x)
  }
}


@available(SwiftStdlib 5.1, *)
@globalActor
public actor SomeGlobalActor {
  public static let shared = SomeGlobalActor()
}

// expected-deployment-note@+1{{add '@available' attribute to enclosing class}}
@SomeGlobalActor class C4 {
  var x: Int = 0

  isolated deinit { // expected-deployment-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
@SomeGlobalActor class C5 {
  var x: Int = 0

  isolated deinit {
    print(x)
  }
}

@available(SwiftStdlib 5.1, *)
@SomeGlobalActor public class C6 {
  var x: Int = 0

  isolated deinit { // expected-deployment-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
@SomeGlobalActor public class C7 {
  var x: Int = 0

  isolated deinit {
    print(x)
  }
}

@available(SwiftStdlib 5.1, *)
@_fixed_layout @SomeGlobalActor public class C8 {
  @usableFromInline var x: Int = 0

  @inlinable isolated deinit { // expected-inlining-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
@_fixed_layout @SomeGlobalActor public class C9 {
  @usableFromInline var x: Int = 0

  @inlinable isolated deinit {
    print(x)
  }
}

// expected-deployment-note@+1{{add '@available' attribute to enclosing actor}}
actor A {
  var x: Int = 0

  isolated deinit { // expected-deployment-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@available(SwiftStdlib 5.1, *)
public actor A1 {
  var x: Int = 0

  isolated deinit { // expected-deployment-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@available(SwiftStdlib 5.1, *)
public actor A2 {
  @usableFromInline var x: Int = 0

  @inlinable isolated deinit { // expected-inlining-error{{isolated deinit is only available in macOS 15.4.0 or newer}}
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
public actor A3 {
  @usableFromInline var x: Int = 0

  @inlinable isolated deinit {
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
actor A4 {
  var x: Int = 0

  isolated deinit {
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
public actor A5 {
  var x: Int = 0

  isolated deinit {
    print(x)
  }
}

@available(SwiftStdlib 6.1, *)
public actor A6 {
  @usableFromInline var x: Int = 0

  @inlinable isolated deinit {
    print(x)
  }
}
