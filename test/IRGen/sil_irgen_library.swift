// RUN: %target-swift-frontend %s -emit-ir -disable-incremental-llvm-codegen -parse-as-library

// Smoke test that SIL-IRGen can compile a library module offline.
func f() {}
