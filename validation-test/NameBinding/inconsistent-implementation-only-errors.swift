// Just check that we don't crash (rdar://problem/52943397)

// RUN: not %target-swift-frontend -typecheck %s
// RUN: not %target-swift-frontend -typecheck -show-diagnostics-after-fatal %s

@_implementationOnly import Swift
import ThisModuleDoesNotExist
@_implementationOnly import ThisModuleDoesNotExist
@_implementationOnly import NeitherDoesThisOne
