// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s
// RUN: %target-swift-frontend -Xllvm -tf-dump-intermediates -O -emit-sil -verify %s | %FileCheck %s
import TensorFlow

// This test is intended to verify that all of the operations end up in-graph:
// that there are no host/accelerator copies generated.  This specifically
// handles checking for top level code.

// CHECK-LABEL: TFDeabstraction Result: main
// CHECK:  sil @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {

// b/76362740
struct NonInlineMethodExample {
  var a = Tensor<Float>(1.0)
  var b = Tensor<Float>(2.0)

  @inline(never)
  mutating func nonInlineMutatingMethod() {
    // Some random computation, doesn't matter for this example.
    _ = a.toAccelerator() + b.toAccelerator()
  }
}

// Check that the non-inline method doesn't prevent us from promoting nimm to the
// stack, this prevents us from getting sends and receives for the initialization
// logic.
var nimm = NonInlineMethodExample()
nimm.nonInlineMutatingMethod()

// CHECK: [[STACK:%[0-9]+]] = alloc_stack $NonInlineMethodExample
// CHECK-NEXT:  alloc_global @

// CHECK-LABEL: } // end sil function 'main'

