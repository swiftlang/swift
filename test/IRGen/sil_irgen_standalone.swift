// RUN: %target-swift-frontend %s -emit-ir -disable-incremental-llvm-codegen

// Smoke test that SIL-IRGen can compile a standalone program offline.
func f() {}
f()
