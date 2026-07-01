// RUN: %target-swift-frontend -parse-as-library -O -emit-sil %s -disable-availability-checking | %FileCheck %s
// REQUIRES: swift_in_compiler

// A non-escaping closure that captures a ~Escapable value (here a `MutableSpan`) is wrapped
// in a `mark_dependence` on the capture. Closure specialization has to look through that
// `mark_dependence`; otherwise `callClosure` is never specialized and the closure survives as
// a `partial_apply` with an indirect call, which pessimizes code using `MutableSpan`.
// https://github.com/swiftlang/swift/issues/89954

@inline(never)
func callClosure(_ body: (UInt8) -> Void) {
  body(42)
}

// The closure capturing `span` must be specialized away: no residual `partial_apply` /
// `mark_dependence`, just a direct call to the specialized `callClosure`.
// CHECK-LABEL: sil {{.*}}@$s{{.*}}3useyySvF :
// CHECK-NOT: partial_apply
// CHECK-NOT: mark_dependence
// CHECK: function_ref @{{.*}}callClosure{{.*}}Tf1c_n
// CHECK-LABEL: } // end sil function '{{.*}}3useyySvF'
@inline(never)
public func use(_ p: UnsafeMutableRawPointer) {
  var span = unsafe MutableSpan(_unsafeStart: p.assumingMemoryBound(to: UInt8.self), count: 1)
  callClosure { span[0] = $0 }
}
