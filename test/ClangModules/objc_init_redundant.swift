// RUN: %target-swift-frontend %clang-importer-sdk -emit-sil -I %S/Inputs/custom-modules %s -verify

// REQUIRES: objc_interop

import Foundation

// rdar://problem/17687082
extension NSObject {
  convenience init() { self.init() } // expected-error{{initializer 'init()' redeclares Objective-C method 'init'}}
}

extension NSObject {
  @objc(class) func foo() { } // expected-error{{method 'foo()' redeclares Objective-C method 'class'}}
}
