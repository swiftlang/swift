// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module %s -DLIBRARY -o %t/Lib.swiftmodule
// RUN: %target-swift-frontend -typecheck %s -I %t -verify

// REQUIRES: objc_interop

#if LIBRARY

import Foundation
public class Test: NSObject {
  @objc public var prop: NSObject?
}

#else

import Lib

func test() {
  _ = #keyPath(Test.prop) // okay
  _ = #keyPath(Test.nonexistent) // expected-error {{type 'Test' has no member 'nonexistent'}}
}

#endif
