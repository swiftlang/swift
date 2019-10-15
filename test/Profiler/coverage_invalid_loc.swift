// RUN: %target-swift-frontend -disable-sil-ownership-verifier -Xllvm -sil-full-demangle -profile-generate -profile-coverage-mapping -emit-sorted-sil -emit-sil -module-name coverage_invalid_loc %s | %FileCheck %s

// The implicit tuple and array exprs formed to call `dynamicallyCall`
// happen to have invalid source locations (n.b. this may not always be true,
// but it was true at the time this test was written).
//
// The coverage pass must skip exprs with invalid locations because there is
// no better alternative: creating fake locations for exprs may make coverage
// reporting incorrect, and not all implicit exprs have valid locations.
//
// Test that a) the coverage pass *can* skip exprs with invalid locations and
// that b) this does not result in the children of implicit exprs being skipped.

@dynamicCallable
public struct Callable {
  func dynamicallyCall(withArguments: [(Int) -> Int]) {}
}

// CHECK: sil_coverage_map {{.*}} closure #1 (Swift.Int) -> Swift.Int in coverage_invalid_loc.foo(a: coverage_invalid_loc.Callable) -> ()
// CHECK-NEXT:   [[@LINE+9]]:5 -> {{.*}}:30 : 0
// CHECK-NEXT: }

// CHECK: sil_coverage_map {{.*}} "foo" "foo" 0 {
// CHECK-NEXT:   [[@LINE+4]]:30 -> {{.*}}:2 : 0
// CHECK-NEXT: }

@_silgen_name("foo")
public func foo(a: Callable) {
  a({ (x : Int) -> Int in x })
}
