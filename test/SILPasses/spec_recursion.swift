// RUN: %swift -O3 -emit-sil %s |  FileCheck %s

// Make sure we are not looping forever.
extension Array {
  mutating
  func new_method(pred: (T, T) -> Bool, left : Int, right : Int) {
      new_method(pred, left: left, right: right);
  }
}
var x1 = [1]
x1.new_method(<, left: 0, right: 1)


struct Test<T> {
  init() {}
  func  recursive(#x : T)  {
    return recursive(x: x)
  }
}

// Make sure that the specialized function calls itself.
//CHECK:   sil shared @_TTSSi___TFV14spec_recursion4Test9recursiveU__fGS0_Q__FT1xQ__T_
//CHECK: function_ref @_TTSSi___TFV14spec_recursion4Test9recursiveU__fGS0_Q__FT1xQ__T_
//CHECK: return
var x2 = Test<Int>()
x2.recursive(x: 3)


