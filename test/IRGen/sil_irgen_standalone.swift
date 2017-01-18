// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil %s -emit-ir

// Smoke test that SIL-IRGen can compile a standalone program offline.
func f() {}
f()
