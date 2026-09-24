// Availability queries fold when the deployment target satisfies the queried
// version.

// RUN: %empty-directory(%t)

// The queries name the versions that shipped SwiftStdlib 6.0. The first target
// deploys there, so every query folds. The second deploys to SwiftStdlib 5.10,
// below it, so the queries stay.

// RUN: %target-swift-frontend -O -emit-sil -target %target-swift-6.0-abi-triple %s | %FileCheck %s --check-prefixes=CHECK,FOLDED,INLINABLE-FOLDED
// RUN: %target-swift-frontend -O -emit-sil -target %target-swift-5.10-abi-triple %s | %FileCheck %s --check-prefixes=CHECK,KEPT,INLINABLE-KEPT

// Folding a query in an ordinary function doesn't need `-O`. An inlinable
// function is still serialized at that point, so its query only folds at `-O`.
// RUN: %target-swift-frontend -emit-sil -target %target-swift-6.0-abi-triple %s | %FileCheck %s --check-prefixes=CHECK,FOLDED,INLINABLE-KEPT
// RUN: %target-swift-frontend -emit-sil -target %target-swift-5.10-abi-triple %s | %FileCheck %s --check-prefixes=CHECK,KEPT,INLINABLE-KEPT

// An inlinable function can be deserialized into a module with a lower
// deployment target, so its query survives serialization even when the module
// that defines it folds its own copy.

// RUN: %target-swift-frontend -O -target %target-swift-6.0-abi-triple -module-name Test -emit-module -emit-module-path %t/Test.swiftmodule %s
// RUN: %sil-opt -target %target-swift-6.0-abi-triple %t/Test.swiftmodule | %FileCheck %s --check-prefix=SERIALIZED

// REQUIRES: OS=macosx || OS=ios || OS=tvos || OS=watchos || OS=xros

@available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *)
@inline(never)
public func newFunction() {}

@inline(never)
public func oldFunction() {}

// A folded query keeps no version check and no branch, and the branch that
// survives is the one that runs when the query succeeds.

// The checks below match `OSVersionAtLeast` instead of one entry point, because
// the entry point differs by platform. On iOS `_stdlib_isOSVersionAtLeast()` is
// `@_transparent`, so a query there calls `_stdlib_isOSVersionAtLeast_AEIC()`,
// which `-O` inlines into the `targetOSVersionAtLeast` builtin.

// CHECK-LABEL: sil{{.*}}@$s33constant_propagation_availability27testAvailabilityPropagationyyF :
// FOLDED-NOT:    OSVersionAtLeast
// FOLDED-NOT:    cond_br
// KEPT:          OSVersionAtLeast
// KEPT:          cond_br
// CHECK:         function_ref @$s33constant_propagation_availability11newFunctionyyF
// KEPT:          function_ref @$s33constant_propagation_availability11oldFunctionyyF
// FOLDED-NOT:    OSVersionAtLeast
// FOLDED-NOT:    cond_br
// FOLDED-NOT:    function_ref @$s33constant_propagation_availability11oldFunctionyyF
// CHECK:       } // end sil function '$s33constant_propagation_availability27testAvailabilityPropagationyyF'
public func testAvailabilityPropagation() {
  if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
    newFunction()
  } else {
    oldFunction()
  }
}

// The same query folds the same way in a function that is inlinable, but only
// once the optimizer drops the serialized flag. Before that the body may still
// be deserialized into a module with a lower deployment target.

// CHECK-LABEL:            sil{{.*}}@$s33constant_propagation_availability13testInlinableyyF :
// INLINABLE-FOLDED-NOT:     OSVersionAtLeast
// INLINABLE-FOLDED-NOT:     cond_br
// INLINABLE-KEPT:           OSVersionAtLeast
// INLINABLE-KEPT:           cond_br
// CHECK:                    function_ref @$s33constant_propagation_availability11newFunctionyyF
// INLINABLE-FOLDED-NOT:     OSVersionAtLeast
// INLINABLE-FOLDED-NOT:     cond_br
// CHECK:                  } // end sil function '$s33constant_propagation_availability13testInlinableyyF'
@inlinable
public func testInlinable() {
  if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
    newFunction()
  }
}

// The serialized body of the inlinable function keeps its query, because a
// module with a lower deployment target may deserialize it.

// SERIALIZED-LABEL: sil{{.*}}@$s4Test13testInlinableyyF :
// SERIALIZED:         OSVersionAtLeast
// SERIALIZED:         cond_br
// SERIALIZED:         function_ref @$s4Test11newFunctionyyF
// SERIALIZED:       } // end sil function '$s4Test13testInlinableyyF'
