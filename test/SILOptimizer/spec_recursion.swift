// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -O -emit-sil %s | %FileCheck %s

// Make sure we are not looping forever.

extension Array {
  mutating func new_method(_ predicate: (Element, Element) -> Bool, left : Int, right : Int) {
      new_method(predicate, left: left, right: right);
  }
}
var x1 = [1]
x1.new_method(<, left: 0, right: 1)


struct Test<T> {
  init() {}
  func recursive(x x : T) {
    return recursive(x: x)
  }
}

// Make sure that the specialized function calls itself.
//CHECK:   sil shared @_T014spec_recursion4TestV9recursive{{[_0-9a-zA-Z]*}}FSi_Tg5
//CHECK: function_ref @_T014spec_recursion4TestV9recursive{{[_0-9a-zA-Z]*}}FSi_Tg5
//CHECK: return
var x2 = Test<Int>()
x2.recursive(x: 3)


