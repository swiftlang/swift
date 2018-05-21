// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import AppKit
import objc_ext
import TestProtocols
import ObjCParseExtras
import ObjCParseExtrasInitHelper

// rdar://problem/18500201
extension NSSet {
  convenience init<T>(array: Array<T>) {
    self.init()
  }
}

// Subclassing and designated initializers
func testNSInterestingDesignated() {
  NSInterestingDesignated() // expected-warning{{unused}}
  NSInterestingDesignated(string:"hello") // expected-warning{{unused}}
  NSInterestingDesignatedSub() // expected-warning{{unused}}
  NSInterestingDesignatedSub(string:"hello") // expected-warning{{unused}}
}

extension URLDocument {
  convenience init(string: String) {
    self.init(url: string)
  }
}

class MyDocument1 : URLDocument {
  override init() { 
    super.init()
  }
}

func createMyDocument1() {
  var md = MyDocument1()
  md = MyDocument1(url: "http://llvm.org")

  // Inherited convenience init.
  md = MyDocument1(string: "http://llvm.org")
  _ = md
}

class MyDocument2 : URLDocument {
  init(url: String) {
    super.init(url: url) // expected-error{{must call a designated initializer of the superclass 'URLDocument'}}
  }
}

class MyDocument3 : NSAwesomeDocument {
  override init() { 
    super.init()
  }
}

func createMyDocument3(_ url: NSURL) {
  var md = MyDocument3()
#if os(macOS)
  // Limit this particular test to macOS; it depends on availability.
  md = try! MyDocument3(contentsOf: url as URL, ofType:"")
#endif
  _ = md
}

class MyInterestingDesignated : NSInterestingDesignatedSub { 
  override init(string str: String) {
    super.init(string: str)
  }

  init(int i: Int) {
    super.init() // expected-error{{must call a designated initializer of the superclass 'NSInterestingDesignatedSub'}}
  }
}

func createMyInterestingDesignated() {
  _ = MyInterestingDesignated(url: "http://llvm.org")
}

func testNoReturn(_ a : NSAwesomeDocument) -> Int {
  a.noReturnMethod(42)
  return 17    // TODO: In principle, we should produce an unreachable code diagnostic here.
}

// Initializer inheritance from protocol-specified initializers.
class MyViewController : NSViewController {
}

class MyView : NSView {
  override init() { super.init() }
} // expected-error{{'required' initializer 'init(coder:)' must be provided by subclass of 'NSView'}}

class MyMenu : NSMenu {
  override init(title: String) { super.init(title: title) }
} // expected-error{{'required' initializer 'init(coder:)' must be provided by subclass of 'NSMenu'}}

class MyTableViewController : NSTableViewController {
}

class MyOtherTableViewController : NSTableViewController {
  override init(int i: Int) {
    super.init(int: i)
  }
} // expected-error{{'required' initializer 'init(coder:)' must be provided by subclass of 'NSTableViewController'}}

class MyThirdTableViewController : NSTableViewController {
  override init(int i: Int) {
    super.init(int: i)
  }

  required init(coder: NSCoder) {
    super.init(coder: coder)!
  }
}

func checkInitWithCoder(_ coder: NSCoder) {
  NSViewController(coder: coder) // expected-warning{{unused}}
  NSTableViewController(coder: coder) // expected-warning{{unused}}
  MyViewController(coder: coder) // expected-warning{{unused}}
  MyTableViewController(coder: coder) // expected-warning{{unused}}
  MyOtherTableViewController(coder: coder) // expected-error{{incorrect argument label in call (have 'coder:', expected 'int:')}}
  MyThirdTableViewController(coder: coder) // expected-warning{{unused}}
}

// <rdar://problem/16838409>
class MyDictionary1 : NSDictionary {}

func getMyDictionary1() {
  _ = MyDictionary1()
}

// <rdar://problem/16838515>
class MyDictionary2 : NSDictionary {
  override init() {
    super.init()
  }
}

class MyString : NSString {
  override init() { super.init() }
} // expected-error{{'required' initializer 'init(coder:)' must be provided by subclass of 'NSString'}}

// <rdar://problem/17281900>
class View: NSView {
  override func addSubview(_ aView: NSView) {
    _ = MyViewController.init()
  }
}

// rdar://problem/19726164
class NonNullDefaultInitSubSub : NonNullDefaultInitSub {
  func foo() {
    _ = NonNullDefaultInitSubSub() as NonNullDefaultInitSubSub?
  }
}

class DesignatedInitSub : DesignatedInitBase {
  var foo: Int?

  override init(int: Int) {}
}

class DesignedInitSubSub : DesignatedInitSub {
  init(double: Double) { super.init(int: 0) } // okay
  init(string: String) { super.init() } // expected-error {{must call a designated initializer of the superclass 'DesignatedInitSub'}}
}

class DesignatedInitWithClassExtensionSubImplicit : DesignatedInitWithClassExtension {}

class DesignatedInitWithClassExtensionSub : DesignatedInitWithClassExtension {
  override init(int: Int) { super.init(int: 0) }
  override init(float: Float) { super.init(float: 0) }
}

class DesignatedInitWithClassExtensionInAnotherModuleSub : DesignatedInitWithClassExtensionInAnotherModule {}

func testInitializerInheritance() {
  _ = DesignatedInitWithClassExtensionSubImplicit(int: 0)
  _ = DesignatedInitWithClassExtensionSubImplicit(convenienceInt: 0)
  _ = DesignatedInitWithClassExtensionSubImplicit(float: 0)

  _ = DesignatedInitWithClassExtensionSub(int: 0)
  _ = DesignatedInitWithClassExtensionSub(convenienceInt: 0)
  _ = DesignatedInitWithClassExtensionSub(float: 0)

  _ = DesignatedInitWithClassExtensionInAnotherModuleSub(int: 0)
  _ = DesignatedInitWithClassExtensionInAnotherModuleSub(convenienceInt: 0)
  _ = DesignatedInitWithClassExtensionInAnotherModuleSub(float: 0)
}

// Make sure that our magic doesn't think the class property with the type name is an init
func classPropertiesAreNotInit() -> ProcessInfo {
  var procInfo = NSProcessInfo.processInfo // expected-error{{'NSProcessInfo' has been renamed to 'ProcessInfo'}}
  procInfo = ProcessInfo.processInfo // okay
  return procInfo
}
