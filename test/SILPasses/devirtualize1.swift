// RUN: %target-swift-frontend %s -O -emit-sil | FileCheck %s

class Bar<T> {
  var m_x : Int
  init(x : Int) { m_x = x }
  func ping() { print(m_x, appendNewline: false)}
}

//CHECK: function_with_interesting_stuff
//CHECK-NOT: class_method
//CHECK: return 
func function_with_interesting_stuff() {
  var x = Bar<Int>(x: 3)
  x.ping()
}


