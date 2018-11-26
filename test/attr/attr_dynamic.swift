// RUN: %target-typecheck-verify-swift -enable-objc-interop
// RUN: %target-swift-ide-test -enable-objc-interop -print-ast-typechecked -source-filename=%s -print-implicit-attrs

struct NotObjCAble {
  var c: Foo
}

@objc class ObjCClass {}

dynamic prefix operator +!+  // expected-error{{'dynamic' modifier cannot be applied to this declaration}} {{1-9=}}

class Foo {
  @objc dynamic init() {}
  @objc dynamic init(x: NotObjCAble) {} // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  // expected-error@-1{{'dynamic' initializer 'init(x:)' must also be '@objc}}
  
  @objc dynamic var x: Int
  
  @objc dynamic var nonObjcVar: NotObjCAble // expected-error{{property cannot be marked @objc because its type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  // expected-error@-1{{'dynamic' property 'nonObjcVar' must also be '@objc'}}

  @objc dynamic func foo(x: Int) {}
  @objc dynamic func bar(x: Int) {}

  @objc dynamic func nonObjcFunc(x: NotObjCAble) {} // expected-error{{method cannot be marked @objc because the type of the parameter cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  // expected-error@-1{{'dynamic' instance method 'nonObjcFunc(x:)' must also be '@objc'}}
  
  @objc dynamic subscript(x: Int) -> ObjCClass { get {} }

  @objc dynamic subscript(x: Int) -> NotObjCAble { get {} } // expected-error{{subscript cannot be marked @objc because its type cannot be represented in Objective-C}} expected-note{{Swift structs cannot be represented in Objective-C}}
  // expected-error@-1{{'dynamic' subscript 'subscript(_:)' must also be '@objc'}}
  
  dynamic deinit {} // expected-error{{'dynamic' modifier cannot be applied to this declaration}} {{3-11=}}

  func notDynamic() {}

  @objc final dynamic func indecisive() {} // expected-error{{a declaration cannot be both 'final' and 'dynamic'}} {{15-23=}}
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
  @objc dynamic override func bar(x: Int) {}

  // CHECK: {{^}} override func notDynamic()
  override func notDynamic() {}
}

// SR-5317
@objcMembers
class ObjCMemberCheck {
  dynamic var s = NotObjCAble(c: Foo()) // expected-error{{'dynamic' property 's' must also be '@objc'}}
}
