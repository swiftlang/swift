// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-forward-mode-differentiation)
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var ForwardModeTests = TestSuite("ForwardModeDifferentiation")

ForwardModeTests.test("Conditionals") {
   func cond1(_ x: Float) -> Float {
     if x > 0 {
       return x * x
     }
     return x + x
   }
   expectEqual(8, derivative(at: 4, in: cond1))
   expectEqual(2, derivative(at: -10, in: cond1))
   func cond2(_ x: Float) -> Float {
     let y: Float
     if x > 0 {
       y = x * x
     } else if x == -1337 {
       y = 0
     } else {
       y = x + x
     }
     return y
   }
   expectEqual(8, derivative(at: 4, in: cond2))
   expectEqual(2, derivative(at: -10, in: cond2))
   expectEqual(0, derivative(at: -1337, in: cond2))
   func cond2_var(_ x: Float) -> Float {
     var y: Float = x
     if x > 0 {
       y = y * x
     } else if x == -1337 {
       y = x  //Dummy assignment; shouldn't affect computation.
       y = x  //Dummy assignment; shouldn't affect computation.
       y = 0
     } else {
       y = x + y
     }
     return y
   }
   expectEqual(8, derivative(at: 4, in: cond2_var))
   expectEqual(2, derivative(at: -10, in: cond2_var))
   expectEqual(0, derivative(at: -1337, in: cond2_var))
   func cond3(_ x: Float, _ y: Float) -> Float {
     if x > 0 {
       return x * y
     }
     return y - x
   }
   expectEqual(9, derivative(at: 4, 5, in: cond3))
   expectEqual(0, derivative(at: -3, -2, in: cond3))
   func cond_tuple(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     let y: (Float, Float) = (x, x)
     if x > 0 {
       return y.0 + y.1
     }
     return y.0 + y.0 - y.1 + y.0
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_tuple))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_tuple))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_tuple))
   func cond_tuple2(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     let y: (Float, Float) = (x, x)
     let y0 = y.0
     if x > 0 {
       let y1 = y.1
       return y0 + y1
     }
     let y0_double = y0 + y.0
     let y1 = y.1
     return y0_double - y1 + y.0
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_tuple2))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_tuple2))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_tuple2))
   func cond_tuple_var(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     var y: (Float, Float) = (x, x)
     var z: (Float, Float) = (x + x, x - x)
     if x > 0 {
       var w = (x, x)
       y.0 = w.1
       y.1 = w.0
       z.0 = z.0 - y.0
       z.1 = z.1 + y.0
     } else {
       z = (x, x)
     }
     return y.0 + y.1 - z.0 + z.1
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_tuple_var))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_tuple_var))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_tuple_var))
   func cond_nestedtuple_var(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     var y: (Float, Float) = (x + x, x - x)
     var z: ((Float, Float), Float) = (y, x)
     if x > 0 {
       var w = (x, x)
       y.0 = w.1
       y.1 = w.0
       z.0.0 = z.0.0 - y.0
       z.0.1 = z.0.1 + y.0
     } else {
       z = ((y.0 - x, y.1 + x), x)
     }
     return y.0 + y.1 - z.0.0 + z.0.1
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_nestedtuple_var))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_nestedtuple_var))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_nestedtuple_var))
   struct FloatPair : Differentiable {
     var first, second: Float
     init(_ first: Float, _ second: Float) {
       self.first = first
       self.second = second
     }
   }
   struct Pair<T : Differentiable, U : Differentiable> : Differentiable {
     var first: T
     var second: U
     init(_ first: T, _ second: U) {
       self.first = first
       self.second = second
     }
   }
   func cond_struct(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     let y = FloatPair(x, x)
     if x > 0 {
       return y.first + y.second
     }
     return y.first + y.first - y.second + y.first
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_struct))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_struct))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_struct))
   func cond_struct2(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     let y = FloatPair(x, x)
     let y0 = y.first
     if x > 0 {
       let y1 = y.second
       return y0 + y1
     }
     let y0_double = y0 + y.first
     let y1 = y.second
     return y0_double - y1 + y.first
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_struct2))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_struct2))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_struct2))
   func cond_struct_var(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     var y = FloatPair(x, x)
     var z = FloatPair(x + x, x - x)
     if x > 0 {
       var w = y
       y.first = w.second
       y.second = w.first
       z.first = z.first - y.first
       z.second = z.second + y.first
     } else {
       z = FloatPair(x, x)
     }
     return y.first + y.second - z.first + z.second
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_struct_var))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_struct_var))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_struct_var))
   func cond_nestedstruct_var(_ x: Float) -> Float {
     // Convoluted function returning `x + x`.
     var y = FloatPair(x + x, x - x)
     var z = Pair(y, x)
     if x > 0 {
       var w = FloatPair(x, x)
       y.first = w.second
       y.second = w.first
       z.first.first = z.first.first - y.first
       z.first.second = z.first.second + y.first
     } else {
       z = Pair(FloatPair(y.first - x, y.second + x), x)
     }
     return y.first + y.second - z.first.first + z.first.second
   }
   expectEqual((8, 2), valueWithDerivative(at: 4, in: cond_nestedstruct_var))
   expectEqual((-20, 2), valueWithDerivative(at: -10, in: cond_nestedstruct_var))
   expectEqual((-2674, 2), valueWithDerivative(at: -1337, in: cond_nestedstruct_var))
}

runAllTests()


