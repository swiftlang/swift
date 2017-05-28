// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -typecheck -verify %s -swift-version 3 -warn-swift3-objc-inference-minimal
// REQUIRES: objc_interop

import Foundation

struct SwiftStruct { }

class ObjCSubclass : NSObject {
  func foo() { } // expected-note 2{{add '@objc' to expose this instance method to Objective-C}}{{3-3=@objc }}
  var bar: NSObject? = nil // expected-note 2{{add '@objc' to expose this var to Objective-C}}{{3-3=@objc }}

  var isEditing: Bool { // expected-warning{{var 'isEditing' with '@objc' setter depends on deprecated inference of '@objc'}}{{3-3=@objc }}
    get { return false }

    @objc(setEditing:)
    set { }
  }

  subscript (i: Int) -> NSObject? { // expected-warning{{'subscript' with '@objc' getter depends on deprecated inference of '@objc'}}{{3-3=@objc }}
    @objc(objectAtIndex:)
    get {
      return nil
    }

    set { }
  }

  @IBInspectable var ibvar: SwiftStruct = SwiftStruct() // expected-warning{{'@IBInspectable' attribute is meaningless on a property that cannot be represented in Objective-C}}{{3-18=}}
  @GKInspectable var gkvar: SwiftStruct = SwiftStruct() // expected-warning{{'@GKInspectable' attribute is meaningless on a property that cannot be represented in Objective-C}}{{3-18=}}
}

class DynamicMembers {
  dynamic func foo() { } // expected-warning{{inference of '@objc' for 'dynamic' members is deprecated}}{{3-3=@objc }}
  
  dynamic var bar: NSObject? = nil // expected-warning{{inference of '@objc' for 'dynamic' members is deprecated}}{{3-3=@objc }}

  func overridableFunc() { }
  var overridableVar: NSObject? = nil
}

extension ObjCSubclass {
  func overridableFunc() { } // expected-note{{add '@objc' to expose this instance method to Objective-C}}{{3-3=@objc }}

  var overridableVar: NSObject? { // expected-note{{add '@objc' to expose this var to Objective-C}}{{3-3=@objc }}
    get { return nil }
    set { }
  }
}

class ObjCSubSubclass : ObjCSubclass {
  override func foo() { } // okay: from the class

  override var bar: NSObject? { // okay: from the class
  get { return nil }
    set { }
  }

  override func overridableFunc() { } // expected-warning{{override of instance method 'overridableFunc()' from extension of 'ObjCSubclass' depends on deprecated inference of '@objc'}}

  override var overridableVar: NSObject? { // expected-warning{{override of var 'overridableVar' from extension of 'ObjCSubclass' depends on deprecated inference of '@objc'}}
    get { return nil }
    set { }
  }
}

class SubclassDynamicMembers : DynamicMembers {
  override func overridableFunc() { }  // okay, explicit dynamic

  override var overridableVar: NSObject? { // okay, explicit dynamic
    get { return nil }
    set { }
  }
}

func testSelector(sc: ObjCSubclass, dm: DynamicMembers) {
  _ = #selector(sc.foo) // expected-warning{{argument of '#selector' refers to instance method 'foo()' in 'ObjCSubclass' that depends on '@objc' attribute inference deprecated in Swift 4}}
  _ = #selector(getter: dm.bar) // okay, explicit dynamic
}

func testKeypath(dm: DynamicMembers) {
  _ = #keyPath(ObjCSubclass.bar)   // expected-warning{{argument of '#keyPath' refers to property 'bar' in 'ObjCSubclass' that depends on '@objc' attribute inference deprecated in Swift 4}}
  _ = #keyPath(DynamicMembers.bar) // okay: dynamic keyword implies @objc
}

func testDynamicCalls(ao: AnyObject) {
  ao.foo?() // expected-warning{{reference to instance method 'foo()' of 'ObjCSubclass' depends on '@objc' attribute inference deprecated in Swift 4}}
  _ = ao.bar! // expected-warning{{reference to var 'bar' of 'ObjCSubclass' depends on '@objc' attribute inference deprecated in Swift 4}}
}
