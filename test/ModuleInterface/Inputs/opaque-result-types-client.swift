import OpaqueResultTypes

func getAssocType<T: AssocTypeInference>(_ x: T) -> T.Assoc {
  return x.foo(0)
}
func getAssocPropType<T: AssocTypeInference>(_ x: T) -> T.AssocProperty {
  return x.prop
}
func getAssocSubscriptType<T: AssocTypeInference>(_ x: T) -> T.AssocSubscript {
  return x[]
}

struct MyFoo: Foo {}
struct YourFoo: Foo {}

@available(SwiftStdlib 5.1, *)
func someTypeIsTheSame() {
  var a = foo(0)
  a = foo(0)
  a = foo("") // expected-error{{cannot assign value of type 'some Foo' (result of 'foo') to type 'some Foo' (result of 'foo')}}

  var b = foo("")
  b = foo(0) // expected-error{{no 'foo' candidates produce the expected contextual result type 'some Foo'}}
  b = foo("")

  var c = foo(MyFoo())
  c = foo(0) // expected-error{{cannot assign value of type 'some Foo' (result of 'foo') to type 'some Foo' (result of 'foo')}}
  c = foo(MyFoo())
  c = foo(YourFoo()) // expected-error{{cannot convert value of type 'YourFoo' to expected argument type 'MyFoo'}}

  var barInt = Bar<Int>()
  var barString = Bar<String>()

  var d = barInt.foo(0)
  d = barInt.foo(0)
  d = barString.foo(0) // expected-error{{no 'foo' candidates produce the expected contextual result type 'some Foo'}}
  d = getAssocType(barInt)
  d = getAssocType(barString) // expected-error{{cannot assign}}
  
  var d2 = barInt.prop
  d2 = barInt.prop
  d2 = barString.prop // expected-error{{cannot assign}}
  d2 = getAssocPropType(barInt)
  d2 = getAssocPropType(barString) // expected-error{{cannot assign}}

  var d3 = barInt[]
  d3 = barInt[]
  d3 = barString[] // expected-error{{cannot assign}}
  d3 = getAssocSubscriptType(barInt)
  d3 = getAssocSubscriptType(barString) // expected-error{{cannot assign}}

  var e = barString.foo(0)
  e = barInt.foo(0) // expected-error{{no 'foo' candidates produce the expected contextual result type 'some Foo'}}
  e = barString.foo(0)
  e = getAssocType(barInt) // expected-error{{cannot assign}}
  e = getAssocType(barString)

  var f = barInt.foo(MyFoo())
  f = barInt.foo(MyFoo())
  f = barString.foo(MyFoo()) // expected-error{{cannot assign}}
  f = barInt.foo(YourFoo()) // expected-error{{cannot convert value of type 'YourFoo' to expected argument type 'MyFoo'}}
  f = barString.foo(MyFoo()) // expected-error{{cannot assign}}

  var g = globalComputedVar
  g = globalVar // expected-error{{cannot assign}}
  g = globalComputedVar
  g = 123 // expected-error{{cannot assign}}

  var h = globalVar
  h = globalComputedVar // expected-error{{cannot assign}}
  h = globalVar
  h = 123 // expected-error{{cannot assign}}

  var i = globalVarTuple
  i = (123, foo(123)) // expected-error{{cannot assign}}
  i = globalVarTuple
  i = (globalVarTuple.0, globalVarTuple.1)
}
