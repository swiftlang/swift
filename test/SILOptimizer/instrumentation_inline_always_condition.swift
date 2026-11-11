// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -module-name main %s \
// RUN:   | %FileCheck %s --check-prefix=PLAIN
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -module-name main -sanitize=address %s \
// RUN:   | %FileCheck %s --check-prefix=ASAN
// RUN: %target-swift-frontend -O -emit-sil -parse-as-library -module-name main -sanitize=thread %s \
// RUN:   | %FileCheck %s --check-prefix=TSAN

// '@inline(always)' and '@instrumentation(disable:)' can't be combined on one
// declaration, but '#if moduleInstrumentation' lets a function be
// '@inline(always)' in uninstrumented builds and out-of-line and
// uninstrumented in instrumented ones. Each configuration below compiles
// without diagnostics.

#if !moduleInstrumentation(address)
@inline(always)
#else
@instrumentation(disable: address)
#endif
public func f(_ x: Int) -> Int { x &+ 1 }

// PLAIN-LABEL: sil @$s4main6callerSiyF
// PLAIN-NOT: function_ref
// ASAN: sil [no_instrumentation_address] @$s4main1fyS2iF
// ASAN-LABEL: sil @$s4main6callerSiyF
// ASAN: function_ref @$s4main1fyS2iF
// TSAN-LABEL: sil @$s4main6callerSiyF
// TSAN-NOT: function_ref
public func caller() -> Int {
  return f(41)
}
