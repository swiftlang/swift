// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

protocol P1 {
  associatedtype A
}

// CHECK-LABEL: typed_throws.(file).f1@
// CHECK-NEXT: Generic signature: <T where T : P1, T.[P1]A == Never>
func f1<T: P1>(_: T) where () throws(T.A) -> () == () -> () {}

// CHECK-LABEL: typed_throws.(file).f2@
// CHECK-NEXT: Generic signature: <T where T : P1, T.[P1]A == any Error>
func f2<T: P1>(_: T) where () throws(T.A) -> () == () throws -> () {}

protocol P2 {
  associatedtype A where A == () throws(E) -> ()
  associatedtype E: Error
}

// CHECK-LABEL: typed_throws.(file).f3@
// CHECK-NEXT: Generic signature: <T where T : P2, T.[P2]E == Never>
func f3<T: P2>(_: T) where T.A == () -> () {}

// CHECK-LABEL: typed_throws.(file).f4@
// CHECK-NEXT: Generic signature: <T where T : P2, T.[P2]E == any Error>
func f4<T: P2>(_: T) where T.A == () throws -> () {}
