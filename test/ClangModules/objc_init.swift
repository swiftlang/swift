// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop
// REQUIRES: OS=macosx
// FIXME: <rdar://problem/19452886> test/ClangModules/objc_init.swift should not require REQUIRES: OS=macosx

import AppKit
import objc_ext
import TestProtocols
import ObjCParseExtras

// rdar://problem/18500201
extension NSSet {
  convenience init<T>(array: Array<T>) {
    self.init()
  }
}

// Subclassing and designated initializers
func testNSInterestingDesignated() {
  NSInterestingDesignated()
  NSInterestingDesignated(string:"hello")
  NSInterestingDesignatedSub()
  NSInterestingDesignatedSub(string:"hello")
}

extension NSDocument {
  convenience init(string: String) {
    self.init(URL: string)
  }
}

class MyDocument1 : NSDocument {
  override init() { 
    super.init()
  }
}

func createMyDocument1() {
  var md = MyDocument1()
  md = MyDocument1(URL: "http://llvm.org")

  // Inherited convenience init.
  md = MyDocument1(string: "http://llvm.org")
}

class MyDocument2 : NSDocument {
  init(URL url: String) {
    super.init(URL: url) // expected-error{{must call a designated initializer of the superclass 'NSDocument'}}
  }
}

class MyDocument3 : NSAwesomeDocument {
  override init() { 
    super.init()
  }
}

func createMyDocument3() {
  var md = MyDocument3()
  md = MyDocument3(URL: "http://llvm.org")
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
  var md = MyInterestingDesignated(URL: "http://llvm.org")
}

func testNoReturn(a : NSAwesomeDocument) -> Int {
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
  override init(int i: Int)  {
    super.init(int: i)
  }
} // expected-error{{'required' initializer 'init(coder:)' must be provided by subclass of 'NSTableViewController'}}

class MyThirdTableViewController : NSTableViewController {
  override init(int i: Int)  {
    super.init(int: i)
  }

  required init(coder: NSCoder) {
    super.init(coder: coder)
  }
}

func checkInitWithCoder(coder: NSCoder) {
  NSViewController(coder: coder)
  NSTableViewController(coder: coder)
  MyViewController(coder: coder)
  MyTableViewController(coder: coder)
  MyOtherTableViewController(coder: coder) // expected-error{{cannot invoke initializer for type 'MyOtherTableViewController' with an argument list of type '(coder: NSCoder)'}} expected-note{{expected an argument list of type '(int: Int)'}}
  MyThirdTableViewController(coder: coder)
}

// <rdar://problem/16838409>
class MyDictionary1 : NSDictionary {}

func getMyDictionary1() {
  var nsd = MyDictionary1()
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
  override func addSubview(aView: NSView) {
    var p = MyViewController.init()
  }
}

// rdar://problem/19726164
class NonNullDefaultInitSubSub : NonNullDefaultInitSub {
  func foo() {
    var x: NonNullDefaultInitSubSub? = NonNullDefaultInitSubSub()
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
