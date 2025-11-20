// RUN: %target-swift-frontend -emit-silgen %s -parse-as-library -module-name Test -experimental-lazy-typecheck -experimental-skip-non-inlinable-function-bodies | %FileCheck %s

// Note: This test has been carefully constructed to create the preconditions of
// a lazy typechecking bug. First, foo() is parsed and typechecked lazily in
// order to generate SIL for it. Type checking the body of foo() causes the
// AvailabilityScope tree to be built for the file, but bar() has not been
// parsed yet so it gets skipped during construction of the tree. Therefore
// when generating the SIL for bar() its scope must be created on-demand in
// order to emit the correct SIL for the if #available condition.

// REQUIRES: OS=macosx

// CHECK: sil{{.*}} @$s4Test3fooyS2iF : $@convention(thin) (Int) -> Int {
// CHECK: } // end sil function '$s4Test3fooyS2iF'
@inlinable
public func foo(_ x: Int) -> Int {
  _ = x
}

// CHECK: sil{{.*}} @$s4Test3barSiyF : $@convention(thin) () -> Int {
// CHECK:   {{.*}} = integer_literal $Builtin.Word, 11
// CHECK:   {{.*}} = integer_literal $Builtin.Word, 0
// CHECK:   {{.*}} = integer_literal $Builtin.Word, 0
// CHECK:   {{.*}} = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF
// CHECK: } // end sil function '$s4Test3barSiyF'
@inlinable
public func bar() -> Int {
  if #available(macOS 11.0, *) {
    return 1
  } else {
    return 2
  }
}
