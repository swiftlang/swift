// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-sil -verify -Xllvm -sil-print-after=differentiation -o /dev/null 2>&1 %s | %FileCheck %s -check-prefix=CHECK-SIL

import _Differentiation

struct Test: Differentiable {
    var val1: Float
    var val2: Float

    @differentiable(reverse)
    mutating func doSomething(input: Float) {
// CHECK-SIL-LABEL: TestV11doSomething5inputySf_tFTJpSSpSr :
// Ensure that only two adjoint buffers will be propagated
// CHECK-SIL: copy_addr %0 to %22 : $*Test.TangentVector
// CHECK-SIL-NEXT: debug_value
// CHECK-SIL-NEXT: copy_addr %0 to %18 : $*Test.TangentVector
// CHECK-SIL-NEXT: switch_enum %1
        self.val1 *= input
        self.val2 *= input

        if self.val1 > input {
            self.val1 = input
        }
        if self.val2 > input {
            self.val2 = input
        }
    }
}

@differentiable(reverse)
func wrapper(input: Float, multiplier: Float) -> Float {
    var test = Test(val1: input, val2: input)
    test.doSomething(input: multiplier)
    return test.val1 * test.val2
}

let grad = gradient(at: 2.0, 3.0, of: wrapper)
print("Grad: \(grad)")
