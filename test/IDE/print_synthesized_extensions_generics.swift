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
// CHECK:      extension C where T : B<U> {
// CHECK-NEXT:   func qux()
// CHECK-NEXT: }

// CHECK:      extension C where T : B<Int> {
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

// CHECK:      extension G where T.T : A {
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
