// RUN: %swift %s -O -emit-sil

class A {
  func ping() -> Int { return 1 }
}
class B : A {
  override func ping() -> Int { return 2 }
}
class C : A {
  override func ping() -> Int{ return 3 }
}

//CHECK-LABEL: @_TF4main3fooFCS_1ASi
//CHECK: checked_cast_br [exact] %0 : $A to $B
//CHECK: integer_literal $Builtin.Word, 2
//CHECK: checked_cast_br [exact] %0 : $A to $C
//CHECK: integer_literal $Builtin.Word, 3
//CHECK: return
func foo(x : A) -> Int {
  return x.ping()

}
