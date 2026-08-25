// RUN: %target-swift-emit-module-interface(%t.swiftinterface) %s -module-name always_unsafe
// RUN: %target-swift-typecheck-module-from-interface(%t.swiftinterface) -module-name always_unsafe
// RUN: %FileCheck %s --input-file %t.swiftinterface --match-full-lines

// Older compilers don't understand the argument of '@unsafe(always)', so the
// interface falls back to plain '@unsafe' for them.

// CHECK: #if compiler(>=5.3) && $AlwaysUnsafeAttribute
// CHECK: @unsafe(always) public func alwaysUnsafeFunc()
// CHECK: #else
// CHECK: @unsafe public func alwaysUnsafeFunc()
// CHECK: #endif
@unsafe(always)
public func alwaysUnsafeFunc() { }

// A plain '@unsafe' needs no compatibility guard.
// CHECK-NOT: $AlwaysUnsafeAttribute
// CHECK: @unsafe public func unsafeFunc()
@unsafe
public func unsafeFunc() { }

// The 'unsafe' marker is deliberately stripped from inlinable bodies printed
// into an interface, so the printed body contains a bare call. Verifying the
// interface therefore must not be a hard error.
// CHECK: @inlinable public func inlinableUse() {
// CHECK-NEXT: alwaysUnsafeFunc()
@inlinable
public func inlinableUse() {
  unsafe alwaysUnsafeFunc()
}
