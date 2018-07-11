// RUN: %target-typecheck-verify-swift

@inlineable public func misspelledInlinable() {}
// expected-error@-1 {{no attribute named '@inlineable'; did you mean '@inlinable'?}}{{2-12=inlinable}}