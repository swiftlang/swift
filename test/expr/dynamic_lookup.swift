// RUN: %swift -verify %s

// Objective-C does not have @property syntax for properties on class
// objects.  One can have static getters and setters, and then use the dot
// syntax on class objects in Objective-C, but there is no @property for us to
// import.  Thus, we don't to support static properties via AnyObject.
@objc class HasStaticProperties {
  class var staticVar1 = 4 // expected-error {{class variables not yet supported}}
}

func testStaticProperty(classObj: AnyObject.Type) {
  var x = classObj.staticVar1 // expected-error {{'AnyObject.Type' does not have a member named 'staticVar1'}}
}
