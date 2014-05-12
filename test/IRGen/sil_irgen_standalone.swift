// RUN: %swift -target x86_64-apple-darwin10 %s -emit-ir

// Smoke test that SIL-IRGen can compile a standalone program offline.
func f() {}
f()
