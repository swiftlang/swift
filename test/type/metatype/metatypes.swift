// RUN: %swift -parse %s -verify

struct Test0 {}

var test0 : Test0.metatype = Test0
test0 = Test0

class Test1a { 
  init() { }
  static def foo() {}
}
class Test1b : Test1a { 
  init() { super.init() }
}

Test1b.foo()
var test1 = Test1a
test1 = Test1b
var x = Test1b();
test1 = typeof(x)
