// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -disable-objc-attr-requires-foundation-module -typecheck -primary-file %s %S/Inputs/property_helper.swift -verify -swift-version 4
import ObjectiveC

// REQUIRES: objc_interop

@objc class HelperClass: NSObject {}

struct Wrapper {
  var objcInstance = ObjCClass()
}

class ObjCClass {
  @objc var myProperty = HelperClass()
  @objc let myConstant = HelperClass() // expected-note 4{{'myConstant' declared here}}
  @objc var myComputedReadOnlyProperty: HelperClass { // expected-note 2{{'myComputedReadOnlyProperty' declared here}}
    get {
      return HelperClass()
    }
  }
  @objc var myComputedReadWriteProperty: HelperClass {
    get {
      return HelperClass()
    }
    set {
    }
  }

  @objc func myFunc() {}
  @objc class func myClassFunc() {}

  func instanceMethod() {
    let _ = #selector(myFunc)
    let _ = #selector(getter: myProperty)
    let _ = #selector(setter: myProperty)
    let _ = #selector(setter: myComputedReadWriteProperty)

    let _ = #selector(setter: myConstant) // expected-error {{argument of '#selector(setter:)' refers to non-settable let 'myConstant'}}
    let _ = #selector(setter: myComputedReadOnlyProperty) // expected-error {{argument of '#selector(setter:)' refers to non-settable var 'myComputedReadOnlyProperty'}}

    let _ = #selector(myClassFunc) // expected-error{{static member 'myClassFunc' cannot be used on instance of type 'ObjCClass'}}
  }

  class func classMethod() {
    let _ = #selector(myFunc)
    let _ = #selector(getter: myProperty)
    let _ = #selector(setter: myProperty)
    let _ = #selector(setter: myComputedReadWriteProperty)

    let _ = #selector(setter: myConstant) // expected-error {{argument of '#selector(setter:)' refers to non-settable let 'myConstant'}}
    let _ = #selector(setter: myComputedReadOnlyProperty) // expected-error {{argument of '#selector(setter:)' refers to non-settable var 'myComputedReadOnlyProperty'}}

    let _ = #selector(myClassFunc)
  }
}

let myObjcInstance = ObjCClass()
let myWrapperInstance = Wrapper()

func testSimple(myObjcInstance: ObjCClass, myWrapperInstance: Wrapper) {
// Check cases that should work
  let _ = #selector(ObjCClass.myFunc)
  let _ = #selector(getter: ObjCClass.myProperty)
  let _ = #selector(setter: ObjCClass.myProperty)

  let _ = #selector(myObjcInstance.myFunc)
  let _ = #selector(getter: myObjcInstance.myProperty)
  let _ = #selector(setter: myObjcInstance.myProperty)

  let _ = #selector(myWrapperInstance.objcInstance.myFunc)
  let _ = #selector(getter: myWrapperInstance.objcInstance.myProperty)
  let _ = #selector(setter: myWrapperInstance.objcInstance.myProperty)
}

func testWrongKind(myObjcInstance: ObjCClass, myWrapperInstance: Wrapper) {

  // Referring to a property with a method selector or a method with a
  // property selector

  let _ = #selector(myObjcInstance.myProperty) // expected-error{{use 'getter:' or 'setter:' to refer to the Objective-C getter or setter of property 'myProperty', respectively}}
  // expected-note@-1{{add 'getter:' to reference the Objective-C getter for 'myProperty'}}{{21-21=getter: }}
  // expected-note@-2{{add 'setter:' to reference the Objective-C setter for 'myProperty'}}{{21-21=setter: }}
  let _ = #selector(myObjcInstance.myComputedReadOnlyProperty) // expected-error{{use 'getter:' to refer to the Objective-C getter of property 'myComputedReadOnlyProperty'}}{{21-21=getter: }}
  let _ = #selector(ObjCClass.myProperty) // expected-error{{use 'getter:' or 'setter:' to refer to the Objective-C getter or setter of property 'myProperty', respectively}}
  // expected-note@-1{{add 'setter:' to reference the Objective-C setter for 'myProperty'}}{{21-21=setter: }}
  // expected-note@-2{{add 'getter:' to reference the Objective-C getter for 'myProperty'}}{{21-21=getter: }}

  // Referring to a method with a property selector
  let _ = #selector(getter: myObjcInstance.myFunc) // expected-error{{cannot reference instance method 'myFunc()' as a property; remove 'getter:'}} {{21-29=}}
  let _ = #selector(setter: myObjcInstance.myFunc) // expected-error{{cannot reference instance method 'myFunc()' as a property; remove 'setter:'}} {{21-29=}}
  let _ = #selector(getter: ObjCClass.myFunc) // expected-error{{cannot reference instance method 'myFunc()' as a property; remove 'getter:'}} {{21-29=}}
  let _ = #selector(setter: ObjCClass.myFunc) // expected-error{{cannot reference instance method 'myFunc()' as a property; remove 'setter:'}} {{21-29=}}

  // Referring to a let property with a setter
  let _ = #selector(setter: myObjcInstance.myConstant) // expected-error {{argument of '#selector(setter:)' refers to non-settable let 'myConstant'}}
  let _ = #selector(setter: ObjCClass.myConstant) // expected-error {{argument of '#selector(setter:)' refers to non-settable let 'myConstant'}}
}

// Referring to non ObjC members

class NonObjCClass {
  var nonObjCPropertyForGetter = HelperClass() // expected-note{{add '@objc' to expose this var to Objective-C}} {{3-3=@objc }}
  var nonObjCPropertyForSetter = HelperClass() // expected-note{{add '@objc' to expose this var to Objective-C}} {{3-3=@objc }}
}

func testNonObjCMembers(nonObjCInstance: NonObjCClass) {
  let _ = #selector(getter: nonObjCInstance.nonObjCPropertyForGetter) // expected-error{{argument of '#selector' refers to var 'nonObjCPropertyForGetter' that is not exposed to Objective-C}}
  let _ = #selector(setter: nonObjCInstance.nonObjCPropertyForSetter) // expected-error{{argument of '#selector' refers to var 'nonObjCPropertyForSetter' that is not exposed to Objective-C}}

  // Referencing undefined symbols

  let _ = #selector(getter: UndefinedClass.myVariable) // expected-error{{use of unresolved identifier 'UndefinedClass'}}
  let _ = #selector(getter: ObjCClass.undefinedProperty) // expected-error{{type 'ObjCClass' has no member 'undefinedProperty'}}
  let _ = #selector(getter: myObjcInstance.undefinedProperty) // expected-error{{value of type 'ObjCClass' has no member 'undefinedProperty'}}
}

// Ambiguous expressions
func testAmbiguous(myObjcInstance: ObjCClass) { // expected-note{{'myObjcInstance' declared here}}

  // Referring to a properties not within a type.
  let myOtherObjcInstance = ObjCClass(); // expected-note{{'myOtherObjcInstance' declared here}}
  let _ = #selector(getter: myObjcInstance) // expected-error{{argument of '#selector' cannot refer to parameter 'myObjcInstance'}}
  let _ = #selector(getter: myOtherObjcInstance) // expected-error{{argument of '#selector' cannot refer to variable 'myOtherObjcInstance'}}
}

// Getter/setter is no keyword
let getter = HelperClass()
let setter = HelperClass()

// Referencing methods named getter and setter
class ObjCClassWithGetterSetter: NSObject {
  @objc func getter() {
  }

  @objc func setter() {
  }

  func referenceGetterSetter() {
    let _ = #selector(getter)
    let _ = #selector(setter)
  }
}

// Looking up inherited members

class BaseClass: NSObject {
  @objc var myVar = 1

  @objc func myFunc() {
  }
}

class SubClass: BaseClass {

}

func testInherited() {
  let _ = #selector(getter: SubClass.myVar)
  let _ = #selector(SubClass.myFunc)

  let subInstance = SubClass()

  let _ = #selector(getter: subInstance.myVar)
  let _ = #selector(subInstance.myFunc)
}

// Looking up instance/static methods on instance/static contexts

class InstanceStaticTestClass {
  @objc static let staticProperty = HelperClass()
  @objc let instanceProperty = HelperClass()

  @objc class func classMethod() {}

  @objc static func staticMethod() {}

  @objc func instanceMethod() {}

  @objc func instanceAndStaticMethod() {}
  @objc class func instanceAndStaticMethod() {}

  class func testClass() {
    let _ = #selector(getter: instanceProperty)
    let _ = #selector(instanceMethod)

    let _ = #selector(classMethod)
    let _ = #selector(staticMethod)
    let _ = #selector(getter: staticProperty)

    let _ = #selector(instanceAndStaticMethod)
  }

  static func testStatic() {
    let _ = #selector(getter: instanceProperty)
    let _ = #selector(getter: staticProperty)

    let _ = #selector(instanceMethod)
    let _ = #selector(classMethod)
    let _ = #selector(staticMethod)

    let _ = #selector(instanceAndStaticMethod)
  }

  func testInstance() {
    let _ = #selector(getter: instanceProperty)
    let _ = #selector(instanceMethod)

    let _ = #selector(getter: staticProperty) // expected-error{{static member 'staticProperty' cannot be used on instance of type 'InstanceStaticTestClass'}}
    let _ = #selector(classMethod) // expected-error{{static member 'classMethod' cannot be used on instance of type 'InstanceStaticTestClass'}}
    let _ = #selector(staticMethod) // expected-error{{static member 'staticMethod' cannot be used on instance of type 'InstanceStaticTestClass'}}

    let _ = #selector(instanceAndStaticMethod)
  }
}

// Accessibility
let otherObjCInstance = OtherObjCClass()

let v11 = #selector(getter: OtherObjCClass.privateVar) // expected-error{{'privateVar' is inaccessible due to 'private' protection level}}
let v12 = #selector(setter: OtherObjCClass.privateVar) // expected-error{{'privateVar' is inaccessible due to 'private' protection level}}
let v13 = #selector(getter: otherObjCInstance.privateVar) // expected-error{{}}
let v14 = #selector(setter: otherObjCInstance.privateVar) // expected-error{{privateVar' is inaccessible due to 'private' protection level}}

let v21 = #selector(getter: OtherObjCClass.privateSetVar)
let v22 = #selector(setter: OtherObjCClass.privateSetVar) // expected-error{{setter of var 'privateSetVar' is inaccessible}}
let v23 = #selector(getter: otherObjCInstance.privateSetVar)
let v24 = #selector(setter: otherObjCInstance.privateSetVar) // expected-error{{setter of var 'privateSetVar' is inaccessible}}

let v31 = #selector(getter: OtherObjCClass.internalVar)
let v32 = #selector(setter: OtherObjCClass.internalVar)
let v33 = #selector(getter: otherObjCInstance.internalVar)
let v34 = #selector(setter: otherObjCInstance.internalVar)

let v41 = #selector(OtherObjCClass.internalFunc)
let v42 = #selector(otherObjCInstance.internalFunc)

let v51 = #selector(OtherObjCClass.privateFunc) // expected-error{{'privateFunc' is inaccessible due to 'private' protection level}}
let v52 = #selector(otherObjCInstance.privateFunc) // expected-error{{'privateFunc' is inaccessible due to 'private' protection level}}
