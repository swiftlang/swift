// Verify that the SIL performance inliner refuses to inline a
// @_noSanitize(<kind>) callee into a caller with a different mask when the
// corresponding sanitizer is enabled — inlining would otherwise cause the
// callee's body to be (de)instrumented at IRGen time, defeating the attribute.

// REQUIRES: asan_runtime
// REQUIRES: swift_in_compiler

// Baseline: without a sanitizer, the mismatch is inert; the callee is inlined
// (verified indirectly by absence of a direct call in the caller's IR).
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library %s \
// RUN:   | %FileCheck %s --check-prefix=NOSAN

// With ASan enabled, the mismatch blocks inlining, so the caller keeps the
// direct call.
// RUN: %target-swift-frontend -O -sanitize=address -emit-sil \
// RUN:   -parse-as-library %s | %FileCheck %s --check-prefix=WITHSAN

@_noSanitize(address)
public func skippedCallee(_ x: Int) -> Int {
  return x &+ 1
}

// Without a sanitizer, the callee is fully inlined and no function_ref remains
// pointing at it.
// NOSAN-LABEL: sil {{.*}}@$s26no_sanitize_inline_barrier6callerSiyF
// NOSAN-NOT: skippedCallee

// With ASan enabled, the mismatched no-sanitize mask blocks inlining, so the
// caller retains the call.
// WITHSAN-LABEL: sil {{.*}}@$s26no_sanitize_inline_barrier6callerSiyF
// WITHSAN: function_ref {{.*}}skippedCallee
public func caller() -> Int {
  return skippedCallee(41)
}
