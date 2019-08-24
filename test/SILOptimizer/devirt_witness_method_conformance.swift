
// RUN: %target-swift-frontend -module-name devirt_witness_method_conformance -O -emit-ir  -primary-file %s | %FileCheck %s
// This is a swift file because the crash doesn't reproduce with SIL.
@inline(never)
func callFoo<T: X>(_ x: T) {
  x.foo()
}
public func a(y: Sub) {
  callFoo(y)
  // specialization of callFoo for Sub:
// CHECK-LABEL: define linkonce_odr hidden swiftcc void @"$s33devirt_witness_method_conformance7callFooyyxAA1XRzlFAA3SubC_Tg5"({{.*}}) local_unnamed_addr
}
protocol X {
  func foo()
}
extension X {
  @_optimize(none)
  func foo() {}
}
public class Base: X {}
public class Sub: Base {}
