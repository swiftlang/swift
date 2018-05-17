// RUN: %target-typecheck-verify-swift

@inlineable public func misspelledInlinable() {}
// expected-error@-1 {{No attribute named '@inlineable', did you mean '@inlinable'?}}{{2-12=inlinable}}