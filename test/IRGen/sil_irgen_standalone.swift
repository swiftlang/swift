// RUN: %swift -no-constraint-checker -triple x86_64-apple-darwin10 %s -emit-llvm -sil-irgen

// Smoke test that SIL-IRGen can compile a standalone program offline.
func f() {}
f()
