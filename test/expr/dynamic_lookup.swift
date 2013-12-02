// RUN: %swift -verify %s

// Objective-C does not have @property syntax for properties on class
// objects.  One can have static getters and setters, and then use the dot
// syntax on class objects in Objective-C, but there is no @property for us to
// import.  Thus, we don't to support static properties via DynamicLookup.
@objc class HasStaticProperties {
  static var staticVar1: Int // expected-error {{static variables not yet supported in classes}}
}

func testStaticProperty(classObj: DynamicLookup.metatype) {
  var x = classObj.staticVar1 // expected-error {{'DynamicLookup.metatype' does not have a member named 'staticVar1'}}
}
