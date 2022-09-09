// RUN: %target-typecheck-verify-swift -enable-experimental-move-only

struct S {
  func bar(@_noImplicitCopy s: S) {} // okay
  @_noImplicitCopy func foo(s: S) {} // okay
  @_noImplicitCopy mutating func nopers() {} // expected-error {{'@_noImplicitCopy' attribute can only be applied to local lets and params}}
}
