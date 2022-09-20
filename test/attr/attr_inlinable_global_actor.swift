// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: concurrency

@available(SwiftStdlib 5.1, *)
@globalActor
private struct PrivateGA { // expected-note 2 {{type declared here}}
  actor Actor {}
  static let shared = Actor()
}

@available(SwiftStdlib 5.1, *)
@globalActor
internal struct InternalGA { // expected-note {{type declared here}}
  actor Actor {}
  static let shared = Actor()
}

@available(SwiftStdlib 5.1, *)
@globalActor
@usableFromInline
internal struct UFIGA {
  @usableFromInline actor Actor {}
  @usableFromInline static let shared = Actor()
}

@available(SwiftStdlib 5.1, *)
@globalActor
public struct PublicGA {
  public actor Actor {}
  public static let shared = Actor()
}

// expected-error@+2 {{internal struct 'UFIStructPrivateGA' cannot have private global actor 'PrivateGA'}}
@available(SwiftStdlib 5.1, *)
@PrivateGA @usableFromInline internal struct UFIStructPrivateGA {} // expected-error {{global actor for struct 'UFIStructPrivateGA' must be '@usableFromInline' or public}}
@available(SwiftStdlib 5.1, *)
@InternalGA @usableFromInline internal struct UFIStructInternalGA {} // expected-error {{global actor for struct 'UFIStructInternalGA' must be '@usableFromInline' or public}}
@available(SwiftStdlib 5.1, *)
@UFIGA @usableFromInline internal struct UFIStructUFIGA {}
@available(SwiftStdlib 5.1, *)
@PublicGA @usableFromInline internal struct UFIStructPublicGA {}

@available(SwiftStdlib 5.1, *)
@inlinable public func testNestedFuncs() {
  // FIXME: Functions isolated to non-resilient global actors nested in
  //        inlinable functions should be diagnosed.
  @PrivateGA func inlineFuncPrivateGA() {}
  @InternalGA func inlineFuncInternalGA() {}
  @UFIGA func inlineFuncUFIGA() {}
  @PublicGA func inlineFuncPublicGA() {}
}

@available(SwiftStdlib 5.1, *)
@inlinable public func testNestedClosures() {
  // FIXME: Closures isolated to non-resilient global actors nested in
  //        inlinable functions should be diagnosed.
  _ = { @PrivateGA in }
  _ = { @InternalGA in }
  _ = { @UFIGA in }
  _ = { @PublicGA in }
}
