// RUN: %target-swift-frontend -parse-as-library -O -emit-sil %s -disable-availability-checking | %FileCheck %s
// REQUIRES: swift_in_compiler

// A non-escaping closure capturing a ~Escapable value (here a MutableSpan) is wrapped in a
// mark_dependence on the capture. Closure specialization must look through it; otherwise
// callClosure is never specialized and the closure survives as an indirect call.
// https://github.com/swiftlang/swift/issues/89954

@inline(never)
func callClosure(_ body: (UInt8) -> Void) {
  body(42)
}

// The closure is specialized away: no residual partial_apply / mark_dependence, just a direct call.
// CHECK-LABEL: sil {{.*}}@$s{{.*}}3useyySvF :
// CHECK-NOT: partial_apply
// CHECK-NOT: mark_dependence
// CHECK: [[F:%[0-9]+]] = function_ref @{{.*}}callClosure{{.*}}Tf1c_n
// CHECK: apply [[F]]
// CHECK-NOT: partial_apply
// CHECK-NOT: mark_dependence
// CHECK-LABEL: } // end sil function '{{.*}}3useyySvF'
@inline(never)
public func use(_ p: UnsafeMutableRawPointer) {
  var span = unsafe MutableSpan(_unsafeStart: p.assumingMemoryBound(to: UInt8.self), count: 1)
  callClosure { span[0] = $0 }
}
