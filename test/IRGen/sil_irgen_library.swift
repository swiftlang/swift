// RUN: %target-swift-frontend %s -emit-ir -parse-as-library

// Smoke test that SIL-IRGen can compile a library module offline.
func f() {}
