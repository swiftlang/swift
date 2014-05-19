// RUN: %swift -parse %s -verify

// Test the use of the postfix '!' to perform a downcast from
// AnyObject to a value of bridged type.

class X {
}

@class_protocol protocol P { }

struct XBridged : _BridgedToObjectiveC {
  static func getObjectiveCType() -> Any.Type { return X.self }

  func bridgeToObjectiveC() -> X { return X() }

  static func bridgeFromObjectiveC(source: X) -> XBridged? {
    return nil
  }
}

struct Z { }

func getObject() -> AnyObject {}

func forceDowncast(var obj: AnyObject) {
  var x : X = obj!
  x = getObject()!

  x = (&obj)! // expected-error{{cannot convert the expression's type '()' to type '$T7'}}

  var z : Z = obj! // expected-error{{cannot convert the expression's type '$T1' to type 'Z'}}
}

func forceDowncastBridged(obj: AnyObject, objOpt: AnyObject?, 
                          objImplicitOpt: AnyObject!) {
  let bridged: XBridged = obj!
  let bridgedOpt: XBridged = objOpt!
  let bridgedImplicitOpt: XBridged = objImplicitOpt!
}

func forceDowncastGeneric<T : P>(obj: AnyObject) {
  var x : T = obj!
  x = getObject()!
}

func forceDowncastGenericFail<T>(obj: AnyObject) {
  var x : T = obj! // expected-error{{cannot convert the expression's type '$T1' to type 'T'}}
}
