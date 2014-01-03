// RUN: %swift %s -O3 -emit-sil | FileCheck %s

class Bar<T> {
  var m_x : Int
  init(x : Int) { m_x = x }
  func ping() { print(m_x)}
}

//CHECK: function_with_interesting_stuff
//CHECK: function_ref @_TFC13devirtualize13Bar4pingU__fGS0_Q__FT_T_
//CHECK: return 
func function_with_interesting_stuff() {
  var x = Bar<Int>(3)
  x.ping()
}


