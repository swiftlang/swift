// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-module -I %S/../Inputs/ObjCBridging %S/../Inputs/ObjCBridging/Appliances.swift -module-name Appliances -o %t

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -typecheck -I %S/../Inputs/ObjCBridging -I %t -parse-as-library -verify %s

// REQUIRES: objc_interop

import Appliances
import Foundation

func checkThatBridgingIsWorking(fridge: Refrigerator) {
  let bridgedFridge = fridge as APPRefrigerator // no-warning
  _ = bridgedFridge as Refrigerator // no-warning
}

@objcMembers class Base : NSObject {
  func test(a: Refrigerator, b: Refrigerator) -> Refrigerator? { // expected-note {{potential overridden instance method 'test(a:b:)' here}}
    return nil
  }
  func testGeneric(a: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? { // expected-note {{potential overridden instance method 'testGeneric(a:b:)' here}} {{none}}
    return nil
  }
  class func testInout(_: inout Refrigerator) {} // expected-note {{potential overridden class method 'testInout' here}}
  func testUnmigrated(a: NSRuncingMode, b: Refrigerator, c: NSCoding) {} // expected-note {{potential overridden instance method 'testUnmigrated(a:b:c:)' here}}
  func testPartialMigrated(a: NSRuncingMode, b: Refrigerator) {} // expected-note {{potential overridden instance method 'testPartialMigrated(a:b:)' here}}
  func testAny(a: Any, b: Any) -> Any? {} // expected-note {{potential overridden instance method 'testAny(a:b:)' here}}

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

@objcMembers class Sub : Base {
  // expected-note@+1 {{type does not match superclass instance method with type '(Refrigerator, Refrigerator) -> Refrigerator?'}} {{25-40=Refrigerator}} {{45-61=Refrigerator?}} {{66-81=Refrigerator}}
  override func test(a: APPRefrigerator, b: APPRefrigerator?) -> APPRefrigerator { // expected-error {{method does not override any method from its superclass}} {{none}}
    return a
  }
  // expected-note@+1 {{type does not match superclass instance method with type '(ManufacturerInfo<NSString>, ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>?'}} {{32-62=ManufacturerInfo<NSString>}} {{67-98=ManufacturerInfo<NSString>?}} {{103-133=ManufacturerInfo<NSString>}}
  override func testGeneric(a: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> { // expected-error {{method does not override any method from its superclass}} {{none}}
    return a
  }
  // expected-note@+1 {{type does not match superclass class method with type '(inout Refrigerator) -> ()'}} {{36-57=inout Refrigerator}}
  override class func testInout(_: inout APPRefrigerator) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  override func testUnmigrated(a: NSObject, b: NSObject, c: NSObject) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  // expected-note@+1 {{type does not match superclass instance method with type '(NSRuncingMode, Refrigerator) -> ()'}} {{53-68=Refrigerator}}
  override func testPartialMigrated(a: NSObject, b: APPRefrigerator) {} // expected-error {{method does not override any method from its superclass}} {{none}}

  // expected-note@+1 {{type does not match superclass instance method with type '(Any, Any) -> Any?'}} {{28-37=Any}} {{42-52=Any?}} {{57-66=Any}}
  override func testAny(a: AnyObject, b: AnyObject?) -> AnyObject {} // expected-error {{method does not override any method from its superclass}}

  // expected-note@+1 {{type does not match superclass subscript with type '(Refrigerator, Refrigerator) -> Refrigerator?'}} {{27-42=Refrigerator}} {{49-65=Refrigerator?}} {{70-85=Refrigerator}}
  override subscript(a a: APPRefrigerator, b b: APPRefrigerator?) -> APPRefrigerator { // expected-error {{subscript does not override any subscript from its superclass}} {{none}}
    return a
  }
  // expected-note@+1 {{type does not match superclass subscript with type '(ManufacturerInfo<NSString>, ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>?'}} {{33-63=ManufacturerInfo<NSString>}} {{70-101=ManufacturerInfo<NSString>?}} {{106-136=ManufacturerInfo<NSString>}}
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
  // expected-note@+1 {{type does not match superclass property with type 'Refrigerator'}} {{22-37=Refrigerator}}
  override var prop: APPRefrigerator { // expected-error {{property 'prop' with type 'APPRefrigerator' cannot override a property with type 'Refrigerator'}}
    return super.prop as APPRefrigerator
  }
  // FIXME: expected-error@+2 {{getter for 'propGeneric' with Objective-C selector 'propGeneric' conflicts with getter for 'propGeneric' from superclass 'Base' with the same Objective-C selector}}
  // expected-note@+1 {{type does not match superclass property with type 'ManufacturerInfo<NSString>'}} {{29-59=ManufacturerInfo<NSString>}}
  override var propGeneric: APPManufacturerInfo<AnyObject> { // expected-error {{property 'propGeneric' with type 'APPManufacturerInfo<AnyObject>' cannot override a property with type 'ManufacturerInfo<NSString>'}}
    return super.prop // expected-error {{return type}}
  }
}

protocol TestProto {
  func test(a: Refrigerator, b: Refrigerator) -> Refrigerator? // expected-note {{protocol requires}}
  func testGeneric(a: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? // expected-note {{protocol requires}}
  static func testInout(_: inout Refrigerator) // expected-note {{protocol requires}}
  func testUnmigrated(a: NSRuncingMode, b: Refrigerator, c: NSCoding) // expected-note {{protocol requires}}
  func testPartialMigrated(a: NSRuncingMode, b: Refrigerator) // expected-note {{protocol requires}}

  subscript(a a: Refrigerator, b b: Refrigerator) -> Refrigerator? { get } // expected-note {{protocol requires}}
  subscript(generic a: ManufacturerInfo<NSString>, b b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? { get } // expected-note {{protocol requires}}

  init?(a: Refrigerator, b: Refrigerator) // expected-note {{protocol requires}}
  init?(generic: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) // expected-note {{protocol requires}}
  init(singleArgument: Refrigerator) // expected-note {{protocol requires}}

  var prop: Refrigerator? { get } // expected-note {{protocol requires}}
  var propGeneric: ManufacturerInfo<NSString>? { get } // expected-note {{protocol requires}}
}

@objcMembers class TestProtoImpl : NSObject, TestProto { // expected-error {{type 'TestProtoImpl' does not conform to protocol 'TestProto'}}
  // expected-note@+1 {{candidate has non-matching type '(APPRefrigerator, APPRefrigerator?) -> APPRefrigerator'}} {{16-31=Refrigerator}} {{36-52=Refrigerator?}} {{57-72=Refrigerator}}
  func test(a: APPRefrigerator, b: APPRefrigerator?) -> APPRefrigerator {
    return a
  }
  // expected-note@+1 {{candidate has non-matching type '(APPManufacturerInfo<AnyObject>, APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject>'}} {{23-53=ManufacturerInfo<NSString>}} {{58-89=ManufacturerInfo<NSString>?}} {{94-124=ManufacturerInfo<NSString>}}
  func testGeneric(a: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> {
    return a
  }
  // expected-note@+1 {{candidate has non-matching type '(inout APPRefrigerator) -> ()'}} {{27-48=inout Refrigerator}}
  class func testInout(_: inout APPRefrigerator) {}

  // expected-note@+1 {{candidate has non-matching type '(NSObject, NSObject, NSObject) -> ()'}}
  func testUnmigrated(a: NSObject, b: NSObject, c: NSObject) {}

  // expected-note@+1 {{candidate has non-matching type '(NSObject, APPRefrigerator) -> ()'}} {{44-59=Refrigerator}}
  func testPartialMigrated(a: NSObject, b: APPRefrigerator) {}

  // expected-note@+1 {{candidate has non-matching type '(APPRefrigerator, APPRefrigerator?) -> APPRefrigerator'}} {{18-33=Refrigerator}} {{40-56=Refrigerator?}} {{61-76=Refrigerator}}
  subscript(a a: APPRefrigerator, b b: APPRefrigerator?) -> APPRefrigerator {
    return a
  }
  // expected-note@+1 {{candidate has non-matching type '(APPManufacturerInfo<AnyObject>, APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject>'}} {{24-54=ManufacturerInfo<NSString>}} {{61-92=ManufacturerInfo<NSString>?}} {{97-127=ManufacturerInfo<NSString>}}
  subscript(generic a: APPManufacturerInfo<AnyObject>, b b: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> {
    return a
  }

  // expected-note@+1 {{candidate has non-matching type '(a: APPRefrigerator, b: APPRefrigerator?)'}} {{11-26=Refrigerator}} {{31-47=Refrigerator?}}
  init(a: APPRefrigerator, b: APPRefrigerator?) {}
  // expected-note@+1 {{candidate has non-matching type '(generic: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?)'}} {{19-49=ManufacturerInfo<NSString>}} {{54-85=ManufacturerInfo<NSString>?}}
  init(generic a: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?) {}
  // expected-note@+1 {{candidate has non-matching type '(singleArgument: APPRefrigerator)'}} {{24-39=Refrigerator}}
  init(singleArgument: APPRefrigerator) {}

  // expected-note@+1 {{candidate has non-matching type 'APPRefrigerator?'}} {{13-29=Refrigerator?}} {{none}}
  var prop: APPRefrigerator? {
    return nil
  }
  // expected-note@+1 {{candidate has non-matching type 'APPManufacturerInfo<AnyObject>?'}} {{20-51=ManufacturerInfo<NSString>?}}
  var propGeneric: APPManufacturerInfo<AnyObject>? {
    return nil
  }
}


@objc protocol TestObjCProto {
  @objc optional func test(a: Refrigerator, b: Refrigerator) -> Refrigerator? // expected-note {{here}} {{none}}
  @objc optional func testGeneric(a: ManufacturerInfo<NSString>, b: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? // expected-note {{here}} {{none}}
  @objc optional func testUnmigrated(a: NSRuncingMode, b: Refrigerator, c: NSCoding) // expected-note {{here}} {{none}}
  @objc optional func testPartialMigrated(a: NSRuncingMode, b: Refrigerator) // expected-note {{here}} {{none}}

  @objc optional subscript(a a: Refrigerator) -> Refrigerator? { get } // expected-note {{here}} {{none}}
  @objc optional subscript(generic a: ManufacturerInfo<NSString>) -> ManufacturerInfo<NSString>? { get } // expected-note {{here}} {{none}}

  @objc optional var prop: Refrigerator? { get } // expected-note {{here}} {{none}}
  @objc optional var propGeneric: ManufacturerInfo<NSString>? { get } // expected-note {{here}} {{none}}
}

@objcMembers class TestObjCProtoImpl : NSObject, TestObjCProto {
  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type '(APPRefrigerator, APPRefrigerator?) -> APPRefrigerator'}} {{16-31=Refrigerator}} {{36-52=Refrigerator?}} {{57-72=Refrigerator}}
  func test(a: APPRefrigerator, b: APPRefrigerator?) -> APPRefrigerator { // expected-warning {{instance method 'test(a:b:)' nearly matches optional requirement 'test(a:b:)' of protocol 'TestObjCProto'}}
    return a
  }
  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type '(APPManufacturerInfo<AnyObject>, APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject>'}} {{23-53=ManufacturerInfo<NSString>}} {{58-89=ManufacturerInfo<NSString>?}} {{94-124=ManufacturerInfo<NSString>}}
  func testGeneric(a: APPManufacturerInfo<AnyObject>, b: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> { // expected-warning {{instance method 'testGeneric(a:b:)' nearly matches optional requirement 'testGeneric(a:b:)' of protocol 'TestObjCProto'}} {{none}}
    return a
  }

  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type '(NSObject, NSObject, NSObject) -> ()'}} {{none}}
  func testUnmigrated(a: NSObject, b: NSObject, c: NSObject) {} // expected-warning {{instance method 'testUnmigrated(a:b:c:)' nearly matches optional requirement 'testUnmigrated(a:b:c:)' of protocol 'TestObjCProto'}}

  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type '(NSObject, APPRefrigerator) -> ()'}} {{44-59=Refrigerator}}
  func testPartialMigrated(a: NSObject, b: APPRefrigerator) {} // expected-warning {{instance method 'testPartialMigrated(a:b:)' nearly matches optional requirement 'testPartialMigrated(a:b:)' of protocol 'TestObjCProto'}} {{none}}

  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type '(APPRefrigerator?) -> APPRefrigerator'}} {{18-34=Refrigerator?}} {{39-54=Refrigerator}}
  subscript(a a: APPRefrigerator?) -> APPRefrigerator { // expected-warning {{subscript 'subscript(a:)' nearly matches optional requirement 'subscript(a:)' of protocol 'TestObjCProto'}} {{none}}
  // expected-note@-1 {{here}}
    return a!
  }
  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type '(APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject>'}} {{24-55=ManufacturerInfo<NSString>?}} {{60-90=ManufacturerInfo<NSString>}}
  subscript(generic a: APPManufacturerInfo<AnyObject>?) -> APPManufacturerInfo<AnyObject> { // expected-warning {{subscript 'subscript(generic:)' nearly matches optional requirement 'subscript(generic:)' of protocol 'TestObjCProto'}} {{none}}
  // expected-error@-1 {{subscript getter with Objective-C selector 'objectForKeyedSubscript:' conflicts with previous declaration with the same Objective-C selector}}
    return a!
  }

  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type 'APPRefrigerator?'}} {{13-29=Refrigerator?}}
  var prop: APPRefrigerator? { // expected-warning {{property 'prop' nearly matches optional requirement 'prop' of protocol 'TestObjCProto'}} {{none}}
    return nil
  }

  // expected-note@+2 {{private}} expected-note@+2 {{@nonobjc}} expected-note@+2 {{extension}}
  // expected-note@+1 {{candidate has non-matching type 'APPManufacturerInfo<AnyObject>?'}} {{20-51=ManufacturerInfo<NSString>?}}
  var propGeneric: APPManufacturerInfo<AnyObject>? { // expected-warning {{property 'propGeneric' nearly matches optional requirement 'propGeneric' of protocol 'TestObjCProto'}} {{none}}
    return nil
  }
}

// Check explicit conversions for bridged generic types.
// rdar://problem/27539951
func testExplicitConversion(objc: APPManufacturerInfo<NSString>,
                            swift: ManufacturerInfo<NSString>) {
  // Bridging to Swift
  let _ = objc as ManufacturerInfo<NSString>
  let _ = objc as ManufacturerInfo<NSNumber> // expected-error{{'APPManufacturerInfo<NSString>' is not convertible to 'ManufacturerInfo<NSNumber>'; did you mean to use 'as!' to force downcast?}}
  let _ = objc as ManufacturerInfo<NSObject> // expected-error{{'APPManufacturerInfo<NSString>' is not convertible to 'ManufacturerInfo<NSObject>'; did you mean to use 'as!' to force downcast?}}

  // Bridging to Objective-C
  let _ = swift as APPManufacturerInfo<NSString>
  let _ = swift as APPManufacturerInfo<NSNumber> // expected-error{{'ManufacturerInfo<NSString>' is not convertible to 'APPManufacturerInfo<NSNumber>'; did you mean to use 'as!' to force downcast?}}
  let _ = swift as APPManufacturerInfo<NSObject> // expected-error{{'ManufacturerInfo<NSString>' is not convertible to 'APPManufacturerInfo<NSObject>'; did you mean to use 'as!' to force downcast?}}
}
