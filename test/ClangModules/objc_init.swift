// RUN: rm -rf %t/clang-module-cache
// RUN: %swift %clang-importer-sdk -emit-sil -module-cache-path %t/clang-module-cache -I %S/Inputs/custom-modules -target x86_64-apple-darwin13 %s -verify
// RUN: ls -lR %t/clang-module-cache | grep ObjectiveC.pcm

import AppKit
import objc_ext
import TestProtocols
import ObjCParseExtras

// Subclassing and designated initializers
func testNSInterestingDesignated() {
  NSInterestingDesignated()
  NSInterestingDesignated(withString:"hello")
  NSInterestingDesignatedSub()
  NSInterestingDesignatedSub(withString:"hello")
}

class MyDocument1 : NSDocument {
  init() { 
    super.init()
  }
}

func createMyDocument1() {
  var md = MyDocument1()
  md = MyDocument1(withURL: "http://llvm.org")
}

class MyDocument2 : NSDocument {
  init withURL(url: String) {
    return super.init(withURL: url) // expected-error{{must call a designated initializer of the superclass 'NSDocument'}}
  }
}

class MyDocument3 : NSAwesomeDocument {
  init() { 
    super.init()
  }
}

func createMyDocument3() {
  var md = MyDocument3()
  md = MyDocument3(withURL: "http://llvm.org")
}

class MyInterestingDesignated : NSInterestingDesignatedSub { 
  init withString(str: String) {
    super.init(withString: str)
  }

  init withInt(i: Int) {
    super.init() // expected-error{{must call a designated initializer of the superclass 'NSInterestingDesignatedSub'}}
  }
}

func createMyInterestingDesignated() {
  var md = MyInterestingDesignated(withURL: "http://llvm.org")
}

func testNoReturn(a : NSAwesomeDocument) -> Int {
  a.noReturnMethod(42)
  return 17    // TODO: In principle, we should produce an unreachable code diagnostic here.
}
