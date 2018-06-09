// RUN: %target-typecheck-verify-swift -swift-version 3
// RUN: %target-typecheck-verify-swift -swift-version 4

// In -swift-version 4 mode, the old spelling is silently parsed as the new spelling.

@_inlineable public func oldInlinableFunction() {}
@_versioned func oldVersionedFunction() {}
