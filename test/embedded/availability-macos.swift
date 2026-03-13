// Checks that in Embedded Swift, we allow using stdlib types even when they
// have declared availability to be higher than our macOS deployment target.

// RUN: %target-typecheck-verify-swift -target arm64-apple-macos14 -enable-experimental-feature Embedded
// RUN: %target-typecheck-verify-swift -target arm64-apple-macos15 -enable-experimental-feature Embedded
// RUN: %target-typecheck-verify-swift -target x86_64-apple-macos14 -enable-experimental-feature Embedded -target-min-inlining-version min

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded
// REQUIRES: OS=macosx

public protocol P { }
struct S: P { }
public enum E: Error { }

public func f(_: Span<Int>) { } // Span is @available(SwiftStdlib 6.1, *)
public func g(_: Int128) { } // Int128 is @available(SwiftStdlib 6.0, *)
public func opaqueReturn() -> some P { return S() }
public func asyncFunc() async { }
public actor Actor { }
public func variadicGeneric<each T>(_: repeat each T) { }
public func typedThrow() throws(E) { }
public struct ValueGeneric<let N: Int> { }
