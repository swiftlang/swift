// RUN: %target-typecheck-verify-swift -swift-version 5

@_inlineable public func oldInlinableFunction() {}
// expected-error@-1 {{'@_inlineable' has been renamed to '@inlinable'}}{{2-13=inlinable}}

@_versioned func oldVersionedFunction() {}
// expected-error@-1 {{'@_versioned' has been renamed to '@usableFromInline'}}{{2-12=usableFromInline}}
