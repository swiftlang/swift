// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -I %S/../Inputs/ObjCBridging %S/../Inputs/ObjCBridging/Appliances.swift -module-name Appliances -o %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -parse -I %S/../Inputs/ObjCBridging -I %t -parse-as-library -verify %s

// REQUIRES: objc_interop

import Appliances
import Foundation

func checkThatBridgingIsWorking(fridge: Refrigerator) {
  let bridgedFridge = fridge as APPRefrigerator // no-warning
  _ = bridgedFridge as Refrigerator // no-warning
}

class Base : NSObject {
  func test(a: Refrigerator, b: Refrigerator) -> Refrigerator? { // expected-note {{potential overridden instance method 'test(a:b:)' here}}
    return nil
  }
  func testGeneric(a: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? { // expected-note {{potential overridden instance method 'testGeneric(a:b:)' here}} {{none}}
    return nil
  }
  class func testInout(_: inout Refrigerator) {} // expected-note {{potential overridden class method 'testInout' here}}
  func testUnmigrated(a: NSRuncingMode, b: Refrigerator, c: NSCoding) {} // expected-note {{potential overridden instance method 'testUnmigrated(a:b:c:)' here}}
  func testPartialMigrated(a: NSRuncingMode, b: Refrigerator) {} // expected-note {{potential overridden instance method 'testPartialMigrated(a:b:)' here}}

  subscript(a a: Refrigerator, b b: Refrigerator) -> Refrigerator? { // expected-note {{potential overridden subscript 'subscript(a:b:)' here}} {{none}}
    return nil
  }
  subscript(generic a: ManufacturerInfo<NSString>, b b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? { // expected-note {{potential overridden subscript 'subscript(generic:b:)' here}} {{none}}
    return nil
  }

  init?(a: Refrigerator, b: Refrigerator) {} // expected-note {{potential overridden initializer 'init(a:b:)' here}} {{none}}
  init?(generic: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) {} // expected-note {{potential overridden initializer 'init(generic:b:)' here}} {{none}}
  init(singleArgument: Refrigerator) {} // expected-note {{potential overridden initializer 'init(singleArgument:)' here}} {{none}}

  // FIXME: expected-note@+1 {{getter for 'prop' declared here}}
  var prop: Refrigerator // expected-note {{attempt to override property here}}
  // FIXME: expected-note@+1 {{getter for 'propGeneric' declared here}}
  var propGeneric: ManufacturerInfo<NSString> // expected-note {{attempt to override property here}}
}

class Sub : Base {
  // expected-note@+1 {{type does not match superclass instance method with type '(a: Refrigerator, b: Refrigerator) -> Refrigerator?'}} {{25-40=Refrigerator}} {{45-61=Refrigerator?}} {{66-81=Refrigerator}}
  override func test(a: APPRefrigerator, b: APPRefrigerator?) -> APPRefrigerator { // expected-error {{method does not override any method from its superclass}} {{none}}
    return a
  }
  // expected-note@+1 {{type does not match superclass instance method with type '(a: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>?'}} {{32-62=ManufacturerInfo<NSString>}} {{67-98=ManufacturerInfo<NSString>?}} {{103-133=ManufacturerInfo<NSString>}}
  override func testGeneric(a: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> { // expected-error {{method does not override any method from its superclass}} {{none}}
    return a
  }
  // expected-note@+1 {{type does not match superclass class method with type '(inout Refrigerator) -> ()'}} {{36-57=inout Refrigerator}}
  override class func testInout(_: inout APPRefrigerator) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  override func testUnmigrated(a: NSObject, b: NSObject, c: NSObject) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  // expected-note@+1 {{type does not match superclass instance method with type '(a: NSRuncingMode, b: Refrigerator) -> ()'}} {{53-68=Refrigerator}}
  override func testPartialMigrated(a: NSObject, b: APPRefrigerator) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  // expected-note@+1 {{type does not match superclass subscript with type '(a: Refrigerator, b: Refrigerator) -> Refrigerator?'}} {{27-42=Refrigerator}} {{49-65=Refrigerator?}} {{70-85=Refrigerator}}
  override subscript(a a: APPRefrigerator, b b: APPRefrigerator?) -> APPRefrigerator { // expected-error {{subscript does not override any subscript from its superclass}} {{none}}
    return a
  }
  // expected-note@+1 {{type does not match superclass subscript with type '(generic: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>?'}} {{33-63=ManufacturerInfo<NSString>}} {{70-101=ManufacturerInfo<NSString>?}} {{106-136=ManufacturerInfo<NSString>}}
  override subscript(generic a: APPManufacturerInfo<AnyObject>, b b: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> { // expected-error {{subscript does not override any subscript from its superclass}} {{none}}
    return a
  }

  // expected-note@+1 {{type does not match superclass initializer with arguments '(a: Refrigerator, b: Refrigerator)'}} {{20-35=Refrigerator}} {{40-56=Refrigerator?}}
  override init(a: APPRefrigerator, b: APPRefrigerator?) {} // expected-error {{initializer does not override a designated initializer from its superclass}}
  // expected-note@+1 {{type does not match superclass initializer with arguments '(generic: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>)'}} {{28-58=ManufacturerInfo<NSString>}} {{63-94=ManufacturerInfo<NSString>?}}
  override init(generic a: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?) {} // expected-error {{initializer does not override a designated initializer from its superclass}}
  // expected-note@+1 {{type does not match superclass initializer with argument '(singleArgument: Refrigerator)'}} {{33-48=Refrigerator}}
  override init(singleArgument: APPRefrigerator) {} // expected-error {{initializer does not override a designated initializer from its superclass}}

  // FIXME: expected-error@+2 {{getter for 'prop' with Objective-C selector 'prop' conflicts with getter for 'prop' from superclass 'Base' with the same Objective-C selector}}
  // expected-note@+1 {{type does not match superclass var with type 'Refrigerator'}} {{22-37=Refrigerator}}
  override var prop: APPRefrigerator { // expected-error {{property 'prop' with type 'APPRefrigerator' cannot override a property with type 'Refrigerator'}}
    return super.prop // implicitly bridged
  }
  // FIXME: expected-error@+2 {{getter for 'propGeneric' with Objective-C selector 'propGeneric' conflicts with getter for 'propGeneric' from superclass 'Base' with the same Objective-C selector}}
  // expected-note@+1 {{type does not match superclass var with type 'ManufacturerInfo<NSString>'}} {{29-59=ManufacturerInfo<NSString>}}
  override var propGeneric: APPManufacturerInfo<AnyObject> { // expected-error {{property 'propGeneric' with type 'APPManufacturerInfo<AnyObject>' cannot override a property with type 'ManufacturerInfo<NSString>'}}
    return super.prop // expected-error {{return type}}
  }
}
