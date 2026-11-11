// RUN: %target-swift-frontend -O -emit-sil -parse-as-library \
// RUN:   %s \
// RUN:   | %FileCheck %s --check-prefix=NOSAN
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -sanitize=address \
// RUN:   %s \
// RUN:   | %FileCheck %s --check-prefix=ASAN
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -sanitize=thread \
// RUN:   %s \
// RUN:   | %FileCheck %s --check-prefix=TSAN
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -sanitize=undefined -sanitize-coverage=func \
// RUN:   %s \
// RUN:   | %FileCheck %s --check-prefix=COV


// The heuristic inliner must not inline a callee that disables some
// instrumentation into a caller that is still instrumented, because the
// callee's body would be instrumented again. This only matters when that kind
// of instrumentation is enabled for the build.

@instrumentation(disable: address)
public func noAddressCallee(_ x: Int) -> Int { x &+ 1 }

@instrumentation(disable: thread)
public func noThreadCallee(_ x: Int) -> Int { x &+ 2 }

@instrumentation(disable: coverage)
public func noCoverageCallee(_ x: Int) -> Int { x &+ 3 }

// NOSAN-LABEL: sil {{.*}}@$s{{.*}}6callerSiyF
// NOSAN-NOT: function_ref
// ASAN-LABEL: sil {{.*}}@$s{{.*}}6callerSiyF
// ASAN: function_ref @$s{{.*}}15noAddressCallee
// ASAN-NOT: function_ref @$s{{.*}}14noThreadCallee
// ASAN-NOT: function_ref @$s{{.*}}16noCoverageCallee
// TSAN-LABEL: sil {{.*}}@$s{{.*}}6callerSiyF
// TSAN-NOT: function_ref @$s{{.*}}15noAddressCallee
// TSAN: function_ref @$s{{.*}}14noThreadCallee
// TSAN-NOT: function_ref @$s{{.*}}16noCoverageCallee
// COV-LABEL: sil {{.*}}@$s{{.*}}6callerSiyF
// COV-NOT: function_ref @$s{{.*}}15noAddressCallee
// COV-NOT: function_ref @$s{{.*}}14noThreadCallee
// COV: function_ref @$s{{.*}}16noCoverageCallee
public func caller() -> Int {
  return noAddressCallee(1) &+ noThreadCallee(2) &+ noCoverageCallee(3)
}

@inline(never) public func sink(_ x: Int) {}

// A caller that disables the same kind of instrumentation can take the
// callee's body.
@instrumentation(disable: address, thread, coverage)
public func matchingCaller() -> Int {
  return noAddressCallee(1) &+ noThreadCallee(2) &+ noCoverageCallee(3)
}
// NOSAN-LABEL: sil {{.*}}@$s{{.*}}14matchingCallerSiyF
// NOSAN-NOT: function_ref
// ASAN-LABEL: sil {{.*}}@$s{{.*}}14matchingCallerSiyF
// ASAN-NOT: function_ref
// TSAN-LABEL: sil {{.*}}@$s{{.*}}14matchingCallerSiyF
// TSAN-NOT: function_ref
// COV-LABEL: sil {{.*}}@$s{{.*}}14matchingCallerSiyF
// COV-NOT: function_ref

// The reverse direction is fine: a plain callee has no contract to lose.
public func plainCallee(_ x: Int) -> Int { x &+ 4 }

@instrumentation(disable: address)
public func noAddressCaller() -> Int {
  return plainCallee(1)
}
// ASAN-LABEL: sil {{.*}}@$s{{.*}}15noAddressCallerSiyF
// ASAN-NOT: function_ref
