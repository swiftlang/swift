// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 4 -enable-source-import -I %S/Inputs
// RUN: %target-swift-ide-test -skip-deinit=false -print-ast-typechecked -source-filename %s -prefer-type-repr=false -print-implicit-attrs=true -explode-pattern-binding-decls=true -disable-objc-attr-requires-foundation-module -swift-version 4 -enable-source-import -I %S/Inputs | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

class ObjCSubclass : NSObject {
  func foo() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}
}

class DynamicMembers {
  @objc dynamic func foo() { }
  
  @objc dynamic var bar: NSObject? = nil
}

func test(sc: ObjCSubclass, dm: DynamicMembers) {
  _ = #selector(sc.foo) // expected-error{{argument of '#selector' refers to instance method 'foo()' that is not exposed to Objective-C}}

  _ = #selector(getter: dm.bar)
  _ = #keyPath(DynamicMembers.bar)
}

struct PlainStruct { }

class BadInSwift4 {
  @IBInspectable var badIBInspectable: PlainStruct?
  // expected-error@-1{{property cannot be marked '@IBInspectable' because its type cannot be represented in Objective-C}}

  @GKInspectable var badGKInspectable: PlainStruct?
  // expected-error@-1{{property cannot be marked '@GKInspectable' because its type cannot be represented in Objective-C}}
}

// CHECK-LABEL: class InitsInheritObjCAttrBase
class InitsInheritObjCAttrBase: NSObject {
  override init() {}
  // CHECK: {{^}} @objc convenience init(foo: Int)
  @objc convenience init(foo: Int) { self.init() }
} // CHECK: {{^[}]$}}

// CHECK-LABEL: extension InitsInheritObjCAttrBase
extension InitsInheritObjCAttrBase {
  // CHECK: {{^}} @objc convenience dynamic init(bar: Int)
  @objc convenience init(bar: Int) { self.init() }
} // CHECK: {{^[}]$}}

// CHECK-LABEL: class ConvenienceInitsInheritObjCAttrSub
class ConvenienceInitsInheritObjCAttrSub: InitsInheritObjCAttrBase {
  init(somethingElse: ()) { super.init() }

  // CHECK: {{^}} @objc convenience init(foo: Int)
  convenience init(foo: Int) { self.init(somethingElse: ()) }
  // FIXME: The '@objc' is relied upon, but the 'dynamic' probably shouldn't be!
  // CHECK: {{^}} @objc convenience dynamic init(bar: Int)
  convenience init(bar: Int) { self.init(somethingElse: ()) }

  // CHECK: {{^}} convenience init(unrelated: Int)
  convenience init(unrelated: Int) { self.init(somethingElse: ()) }
} // CHECK: {{^[}]$}}

// CHECK-LABEL: class DesignatedInitsInheritObjCAttrSub
class DesignatedInitsInheritObjCAttrSub: InitsInheritObjCAttrBase {
  // CHECK: {{^}} @objc init(foo: Int)
  init(foo: Int) { super.init() }
  // FIXME: The '@objc' is relied upon, but the 'dynamic' probably shouldn't be!
  // CHECK: {{^}} @objc dynamic init(bar: Int)
  init(bar: Int) { super.init() }
  
  // CHECK: {{^}} init(unrelated: Int)
  init(unrelated: Int) { super.init() }
} // CHECK: {{^[}]$}}
