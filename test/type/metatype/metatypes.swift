// RUN: %target-parse-verify-swift

struct Test0 {}

var test0 : Test0.Type = Test0.self
test0 = Test0.self

class Test1a { 
  init() { }
  class func foo() {}
}
class Test1b : Test1a { 
  override init() { super.init() }
}

Test1b.foo()
var test1 = Test1a.self
test1 = Test1b.self
var x = Test1b()
test1 = x.dynamicType
