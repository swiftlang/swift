// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module-path %t/print_synthesized_extensions_generics.swiftmodule -emit-module-doc -emit-module-doc-path %t/print_synthesized_extensions_generics.swiftdoc %s
// RUN: %target-swift-ide-test -print-module -synthesize-extension -print-interface -no-empty-line-between-members -module-to-print=print_synthesized_extensions_generics -I %t -source-filename=%s | %FileCheck %s

public protocol P1 {
  associatedtype T
  associatedtype U
}

public class A {}
public class B<T> : A {}

extension P1 where T : B<U> {
  public func qux() {}
}
extension P1 where T : B<Int> {
  public func flob() {}
}

public class C<T : A, U> {}
extension C : P1 {}
// CHECK:      extension C where T : print_synthesized_extensions_generics.B<U> {
// CHECK-NEXT:   func qux()
// CHECK-NEXT: }

// CHECK:      extension C where T : print_synthesized_extensions_generics.B<Int> {
// CHECK-NEXT:   func flob()
// CHECK-NEXT: }

public class D<U> {}
extension D : P1 {
  public typealias T = B<U>
}
// CHECK:      class D<U> {
// CHECK-NEXT:   func qux()
// CHECK-NEXT: }

// FIXME: Arguably we should support this
// CHECK-NOT: extension D where U == Int

public class E {}
extension E: P1 {
  public typealias T = B<U>
  public typealias U = Int
}
// CHECK:      class E {
// CHECK-NEXT:   func qux()
// CHECK-NEXT:   func flob()
// CHECK-NEXT: }

public class F {}
extension F : P1 {
  public typealias T = B<U>
  public typealias U = String
}
// CHECK:      class F {
// CHECK-NEXT:   func qux()
// CHECK-NEXT: }

// CHECK-NOT: extension F where T : B<Int>

public protocol P2 {
  associatedtype T : P1
}

extension P2 where T.T : A {
  public func blah() {}
}

public class G<T : P1> {}
extension G : P2 {}

// CHECK:      extension G where T.T : print_synthesized_extensions_generics.A {
// CHECK-NEXT:   func blah()
// CHECK-NEXT: }

// CHECK:      extension P1 where Self.T : print_synthesized_extensions_generics.B<Self.U> {
// CHECK-NEXT:   func qux()
// CHECK-NEXT: }

// CHECK:      extension P1 where Self.T : print_synthesized_extensions_generics.B<Int> {
// CHECK-NEXT:   func flob()
// CHECK-NEXT: }

// CHECK:      extension P2 where Self.T.T : print_synthesized_extensions_generics.A {
// CHECK-NEXT:   func blah()
// CHECK-NEXT: }

public protocol P3 {
  associatedtype A
}
public protocol P4 {
  associatedtype B: P3
}

public struct S1<A> {}

// Make sure we don't crash. Unfortunately we don't yet correctly output these
// though.
extension S1: P4 where A: P4, A.B.A == A {
  public typealias B = A.B
}
extension P3 where A: P4, A.B.A == A {
  public func foo() {}
}
