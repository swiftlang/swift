// RUN: %target-parse-verify-swift
// RUN: %target-swift-ide-test -print-ast-typechecked -source-filename=%s -print-implicit-attrs

struct NotObjCAble {
  var c: Foo
}

@objc class ObjCClass {}

dynamic prefix operator +!+  // expected-error{{'dynamic' modifier cannot be applied to this declaration}} {{1-9=}}

class Foo {
  dynamic init() {}
  dynamic init(x: NotObjCAble) {} // expected-error{{method cannot be marked dynamic because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic var x: Int

  dynamic var nonObjcVar: NotObjCAble // expected-error{{property cannot be marked dynamic because its type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic func foo(x: Int) {}
  dynamic func bar(x: Int) {}

  dynamic func nonObjcFunc(x: NotObjCAble) {} // expected-error{{method cannot be marked dynamic because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic subscript(x: Int) -> ObjCClass { get {} }

  dynamic subscript(x: Int) -> NotObjCAble { get {} } // expected-error{{subscript cannot be marked dynamic because its type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}

  dynamic deinit {} // expected-error{{'dynamic' modifier cannot be applied to this declaration}} {{3-11=}}

  func notDynamic() {}

  final dynamic func indecisive() {} // expected-error{{a declaration cannot be both 'final' and 'dynamic'}} {{9-17=}}
}

struct Bar {
  dynamic init() {} // expected-error{{only members of classes may be dynamic}} {{3-11=}}

  dynamic var x: Int // expected-error{{only members of classes may be dynamic}} {{3-11=}}

  dynamic subscript(x: Int) -> ObjCClass { get {} } // expected-error{{only members of classes may be dynamic}} {{3-11=}}

  dynamic func foo(x: Int) {} // expected-error{{only members of classes may be dynamic}} {{3-11=}}
}

// CHECK-LABEL: class InheritsDynamic : Foo {
class InheritsDynamic: Foo {
  // CHECK-LABEL: {{^}} dynamic override func foo(x: Int)
  override func foo(x: Int) {}
  // CHECK-LABEL: {{^}} dynamic override func foo(x: Int)
  dynamic override func bar(x: Int) {}

  // CHECK: {{^}} override func notDynamic()
  override func notDynamic() {}
}
