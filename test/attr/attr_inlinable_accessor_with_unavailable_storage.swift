// RUN: %target-typecheck-verify-swift -swift-version 5

// REQUIRES: OS=macosx

public struct HasVarWithInternalAccessor {
  @available(macOS, unavailable)
  public internal(set) var internalSetterVar: Int {
    get { return 1 }
    set { } // expected-note {{setter for property 'internalSetterVar' is not '@usableFromInline' or public}}
  }

  @available(macOS, unavailable)
  @inlinable mutating public func inlinable() {
    internalSetterVar = 2 // expected-error {{setter for property 'internalSetterVar' is internal and cannot be referenced from an '@inlinable' function}}
  }
}
