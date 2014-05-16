// RUN: %swift -parse %s -verify

// Test the use of the postfix '!' to perform a downcast from
// AnyObject to a value of class type.

class X {
  @conversion func __conversion() -> XStruct { }
}

@class_protocol protocol P { }

struct XStruct { }
struct Z { }

func getObject() -> AnyObject {}

func forceDowncast(var obj: AnyObject) {
  var x : X = obj!
  x = getObject()!

  x = (&obj)! // expected-error{{cannot convert the expression's type '()' to type '$T7'}}

  var z : Z = obj! // expected-error{{cannot convert the expression's type '$T1' to type 'Z'}}

  // FIXME: We *could* actually allow this, by downcasting to X and then
  // performing a conversion, but the solver isn't smart enough to find that.
  var xs : XStruct = obj! // expected-error{{cannot convert the expression's type '$T1' to type 'XStruct'}}
}

func forceDowncastGeneric<T : P>(obj: AnyObject) {
  var x : T = obj!
  x = getObject()!
}

func forceDowncastGenericFail<T>(obj: AnyObject) {
  var x : T = obj! // expected-error{{cannot convert the expression's type '$T1' to type 'T'}}
}
