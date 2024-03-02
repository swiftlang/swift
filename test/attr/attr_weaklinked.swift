// RUN: %target-typecheck-verify-swift

#if !os(Windows)
@_weakLinked public func f() { }
#endif

