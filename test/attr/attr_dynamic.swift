// RUN: %swift -parse -verify %s

struct NotObjCAble {
  var c: Foo
}

@objc class ObjCClass {}

class Foo {
  dynamic init() {}
  dynamic init(x: NotObjCAble) {} // expected-error{{method cannot be marked dynamic because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic var x: Int

  dynamic var nonObjcVar: NotObjCAble // expected-error{{property cannot be marked dynamic because its type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic func foo(x: Int) {}

  dynamic func nonObjcFunc(x: NotObjCAble) {} // expected-error{{method cannot be marked dynamic because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic subscript(x: Int) -> ObjCClass { get {} }

  dynamic subscript(x: Int) -> NotObjCAble { get {} } // expected-error{{subscript cannot be marked dynamic because its type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic deinit {} // expected-error{{only methods, initializers, properties, and subscripts may be dynamic}}
}

struct Bar {
  dynamic init() {} // expected-error{{only members of classes may be dynamic}}

  dynamic var x: Int // expected-error{{only members of classes may be dynamic}}

  dynamic subscript(x: Int) -> ObjCClass { get {} } // expected-error{{only members of classes may be dynamic}}

  dynamic func foo(x: Int) {} // expected-error{{only members of classes may be dynamic}}
}

