import OpaqueResultTypes

func getAssocType<T: AssocTypeInference>(_ x: T) -> T.Assoc {
  return x.foo(0)
}

struct MyFoo: Foo {}
struct YourFoo: Foo {}

func someTypeIsTheSame() {
  var a = foo(0)
  a = foo(0)
  a = foo("") // expected-error{{cannot assign}}

  var b = foo("")
  b = foo(0) // expected-error{{cannot assign}}
  b = foo("")

  var c = foo(MyFoo())
  c = foo(0) // expected-error{{cannot assign}}
  c = foo(MyFoo())
  c = foo(YourFoo()) // expected-error{{cannot assign}}

  var barInt = Bar<Int>()
  var barString = Bar<String>()

  var d = barInt.foo(0)
  d = barInt.foo(0)
  d = barString.foo(0) // expected-error{{cannot assign}}
  d = getAssocType(barInt)
  d = getAssocType(barString) // expected-error{{cannot assign}}

  var e = barString.foo(0)
  e = barInt.foo(0) // expected-error{{cannot assign}}
  e = barString.foo(0)
  e = getAssocType(barInt) // expected-error{{cannot assign}}
  e = getAssocType(barString)

  var f = barInt.foo(MyFoo())
  f = barInt.foo(MyFoo())
  f = barString.foo(MyFoo()) // expected-error{{cannot assign}}
  f = barInt.foo(YourFoo()) // expected-error{{cannot assign}}
  f = barString.foo(MyFoo()) // expected-error{{cannot assign}}
}
