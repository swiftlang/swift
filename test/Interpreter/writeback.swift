// RUN: %swift %s -i | FileCheck %s

struct Foo {
  var _x : (Int, Char, String)

  var x : (Int, String, Char) {
  get:
    var (a, b, c) = _x
    return (a, c, b)

  set:
    var (a, b, c) = value
    _x = (a, c, b)
  }

  constructor(a:Int, b:String, c:Char) {
    _x = (a, c, b)
  }
}

var foo = Foo(1, "two", '3')

foo.x.0 = 4
foo.x.1 = "five"
foo.x.2 = '6'

// CHECK: 4
// CHECK: five
// CHECK: 6
println(foo.x.0)
println(foo.x.1)
println(foo.x.2)

struct Bar {
  var _foo : Foo

  var foo : Foo {
  get:
    return _foo

  set:
    _foo = value
  }
}

var bar = Bar(Foo(1, "two", '3'))

bar.foo.x.0 = 7
bar.foo.x.1 = "eight"
bar.foo.x.2 = '9'

// CHECK: 7
// CHECK: eight
// CHECK: 9
println(bar.foo.x.0)
println(bar.foo.x.1)
println(bar.foo.x.2)

(foo, bar.foo) = (bar.foo, foo)

// CHECK: 4
// CHECK: five
// CHECK: 6
println(bar.foo.x.0)
println(bar.foo.x.1)
println(bar.foo.x.2)

(foo.x, bar.foo.x) = (bar.foo.x, foo.x)

// CHECK: 7
// CHECK: eight
// CHECK: 9
println(bar.foo.x.0)
println(bar.foo.x.1)
println(bar.foo.x.2)

(foo.x.0, bar.foo.x.0) = (bar.foo.x.0, foo.x.0)

// CHECK: 4
// CHECK: eight
// CHECK: 9
println(bar.foo.x.0)
println(bar.foo.x.1)
println(bar.foo.x.2)

