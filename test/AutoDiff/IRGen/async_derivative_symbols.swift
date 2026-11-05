// RUN: %target-swift-frontend -emit-ir -parse-as-library -module-name test -disable-availability-checking -validate-tbd-against-ir=all %s | %FileCheck %s
// REQUIRES: concurrency

import _Differentiation

@differentiable(reverse)
public func square(_ x: Float) async -> Float {
  x * x
}

// CHECK-DAG: @"$s4test6squareyS2fYaFTJfSpSrTu" = {{.*}}global %swift.async_func_pointer
// CHECK-DAG: @"$s4test6squareyS2fYaFTJrSpSrTu" = {{.*}}global %swift.async_func_pointer
