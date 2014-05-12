// RUN: %target-run-simple-swift | FileCheck %s

struct Foo {
  var _x : (Int, UnicodeScalar, String)

  var x : (Int, String, UnicodeScalar) {
    get {
      var (a, b, c) = _x
      return (a, c, b)
    }

    mutating
    set {
      var (a, b, c) = newValue
      _x = (a, c, b)
    }
  }

  init(a:Int, b:String, c:UnicodeScalar) {
    _x = (a, c, b)
  }
}

var foo = Foo(a: 1, b: "two", c: "3")

foo.x.0 = 4
foo.x.1 = "five"
foo.x.2 = "6"

// CHECK: 4
// CHECK: five
// CHECK: 6
println(foo.x.0)
println(foo.x.1)
println(foo.x.2)

struct Bar {
  var _foo : Foo

  var foo : Foo {
    get {
      return _foo
    }

    mutating
    set {
      _foo = newValue
    }
  }
}

var bar = Bar(_foo: Foo(a: 1, b: "two", c: "3"))

bar.foo.x.0 = 7
bar.foo.x.1 = "eight"
bar.foo.x.2 = "9"

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

