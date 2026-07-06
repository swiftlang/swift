// RUN: %target-swift-frontend -emit-sil -verify %s
// REQUIRES: swift_in_compiler

// https://github.com/apple/swift/issues/71164

// There are few unreachble blocks created due to mandatory boolean constant
// propagation
// Ensure we do not ceeate linear map types for unreachable BB and that they
// are skipped during VJP and pullback generation
import _Differentiation

struct A: Differentiable {}

struct B: Differentiable {
    @differentiable(reverse)
    func c(b: B) -> A {
        while true {
            if true {
                break
            }
        };
        
        return A()
    }

    @differentiable(reverse)
    func d(b: B) -> A {
        while true {
            if true {
                return c(b : b)
            }
            if true {
                break
            }
            if false {
                return c(b : b)
            } else {
                break
            }
        };
        return A()
    }

}
