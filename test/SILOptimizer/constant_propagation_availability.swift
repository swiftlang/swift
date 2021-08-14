// RUN: %target-swift-frontend -target %target-cpu-apple-macos10.15 -emit-sil %s | %FileCheck --check-prefix=CHECK-macosx10_15 %s
// RUN: %target-swift-frontend -target %target-cpu-apple-macos10.14 -emit-sil %s | %FileCheck --check-prefix=CHECK-macosx10_14 %s
// RUN: %target-swift-frontend -O -target %target-cpu-apple-macos10.15 -emit-sil %s | %FileCheck --check-prefix=CHECK-macosx10_15 %s
// RUN: %target-swift-frontend -O -target %target-cpu-apple-macos10.14 -emit-sil %s | %FileCheck --check-prefix=CHECK-macosx10_14 %s

// REQUIRES: OS=macosx

@available(macOS 10.15, *)
@inline(never)
public func newFunction() -> Int {
  return 0
}

@inline(never)
public func oldFunction() -> Int {
  return 1
}

public func testAvailabilityPropagation() -> Int {
  if #available(macOS 10.15, *) {
    return newFunction()
  } else {
    return oldFunction()
  }
}

// CHECK-macosx10_15-LABEL: sil @$s33constant_propagation_availability27testAvailabilityPropagationSiyF : $@convention(thin) () -> Int {
// CHECK-macosx10_15-NOT: apply
// CHECK-macosx10_15:  [[F:%.*]] = function_ref @$s33constant_propagation_availability11newFunctionSiyF
// CHECK-macosx10_15:  apply [[F]]() : $@convention(thin) () -> Int
// CHECK-macosx10_15-NOT: apply
// CHECK-macosx10_15: } // end sil function '$s33constant_propagation_availability27testAvailabilityPropagationSiyF'

// CHECK-macosx10_14-LABEL: sil @$s33constant_propagation_availability27testAvailabilityPropagationSiyF : $@convention(thin) () -> Int {
// CHECK-macosx10_14:  [[F:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK-macosx10_14:  apply [[F]]
// CHECK-macosx10_14:  [[F:%.*]] = function_ref @$s33constant_propagation_availability11newFunctionSiyF
// CHECK-macosx10_14:  apply [[F]]() : $@convention(thin) () -> Int
// CHECK-macosx10_14:  [[F:%.*]] = function_ref @$s33constant_propagation_availability11oldFunctionSiyF
// CHECK-macosx10_14:  apply [[F]]
// CHECK-macosx10_14: } // end sil function '$s33constant_propagation_availability27testAvailabilityPropagationSiyF'

// CHECK-macosx10_14: sil [readnone] [_semantics "availability.osversion"] @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
