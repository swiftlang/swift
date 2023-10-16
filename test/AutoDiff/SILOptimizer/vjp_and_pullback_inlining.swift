// VJP and pullback inlining tests.

// RUN: %target-swift-frontend -emit-sil -O -verify -Xllvm -debug-only=sil-inliner %s 2>&1 | %FileCheck %s

// REQUIRES: asserts
// REQUIRES: swift_in_compiler

import _Differentiation
#if canImport(Glibc)
import Glibc
#else
import Foundation
#endif

// ======================== VJPs ======================== //
@differentiable(reverse)
@_silgen_name("simple_vjp")
func simple_vjp(x: Float) -> Float {
    let a = x * x;
    let b = x + x;
    let c = x * a;
    let d = a + b;
    let e = b * c;

    return a * b / c + d - e ;
}

@inline(never)
@_silgen_name("caller_of_simple_vjp")
func caller_of_simple_vjp() -> Float {
  gradient(at: Float(4), of: simple_vjp)
}

// CHECK: decision {{{.*}}, b=30, {{.*}}} simple_vjpTJrSpSr
// CHECK-NEXT: "simple_vjpTJrSpSr" inlined into "caller_of_simple_vjp"

// ======================== Pullback w/ control-flow ======================== //

@differentiable(reverse)
@_silgen_name("pb_with_control_flow")
func pb_with_control_flow(_ x: Float) -> Float {
  if (x > 0) {
    return sin(x) * cos(x)
  } else {
    return sin(x) + cos(x)
  }
}

@inline(never)
@_silgen_name("caller_of_pb_with_control_flow")
func caller_of_pb_with_control_flow() -> Float {
    gradient(at: Float(1), of: pb_with_control_flow)
}

// CHECK: decision {{{.*}}, b=70, {{.*}}} pb_with_control_flowTJpSpSr
// CHECK-NEXT: "pb_with_control_flowTJpSpSr" inlined into "caller_of_pb_with_control_flow"


@differentiable(reverse)
func double(x: Float) -> Float {
    return x + x
}

@differentiable(reverse)
func square(x: Float) -> Float {
    return x * x
}

@differentiable(reverse)
@_silgen_name("more_complex_pb_with_control_flow")
func more_complex_pb_with_control_flow(x: Float) -> Float {
    if (x > 0) {
        if ((x+1) < 5) {
            if (x*2 > 4) {
                let y = square(x: x)
                if (y >= x) {
                    let d = double(x: x)
                    return x - (d*y)
                } else {
                    let e = square(x: y)
                    return x + (e*y)
                }
            }
        }
    } else {
        let y = double(x: x)
        return x * y
    }

    return x*3
}

@inline(never)
@_silgen_name("caller_of_more_complex_pb_with_control_flow")
func caller_of_more_complex_pb_with_control_flow() -> Float {
    gradient(at: Float(1), of: more_complex_pb_with_control_flow)
}

// CHECK: decision {{{.*}}, b=70, {{.*}}} more_complex_pb_with_control_flowTJpSpSr
// CHECK-NEXT: "more_complex_pb_with_control_flowTJpSpSr" inlined into "caller_of_more_complex_pb_with_control_flow"
