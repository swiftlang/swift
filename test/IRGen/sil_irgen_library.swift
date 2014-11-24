// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -parse-as-library

// Smoke test that SIL-IRGen can compile a library module offline.
func f() {}
