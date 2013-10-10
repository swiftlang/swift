// RUN: %swift -parse %s -verify

// Test the use of the postfix '!' to perform a downcast from
// DynamicLookup to a value of class type.

// FIXME: Test with lvalues
// FIXME: Reject when we're converting to something that's not a class

class X { }
@class_protocol protocol P { }

func getObject() -> DynamicLookup {}

func forceDowncast() {
  var x : X = getObject()!
  x = getObject()!
}

func forceDowncastGeneric<T : P>() {
  var x : T = getObject()!
  x = getObject()!
}
