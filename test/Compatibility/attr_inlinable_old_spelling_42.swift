// RUN: %target-typecheck-verify-swift -swift-version 4.2

// In -swift-version 4.2 mode, the old spelling produces a warning.

@_inlineable public func oldInlinableFunction() {}
// expected-warning@-1 {{'@_inlineable' has been renamed to '@inlinable'}}{{2-13=inlinable}}

@_versioned func oldVersionedFunction() {}
// expected-warning@-1 {{'@_versioned' has been renamed to '@usableFromInline'}}{{2-12=usableFromInline}}

@inlinable @usableFromInline func redundantAttribute() {}
// expected-warning@-1 {{'@inlinable' declaration is already '@usableFromInline'}}
