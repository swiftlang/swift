// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect
// REQUIRES: swift_feature_ObjCImplementation

// '@objcDirect' implies 'final', except in an '@objc @implementation' extension.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// In Swift 4 mode, 'open' on other implicitly final declarations is a warning.
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -swift-version 4 -enable-experimental-feature ObjCDirect %t/local.swift
// RUN: %target-swift-frontend -emit-module -o %t/Lib.swiftmodule \
// RUN:   -module-name Lib -enable-experimental-feature ObjCDirect %t/Lib.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -I %t %t/client.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated \
// RUN:   -import-objc-header %t/Widget.h \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -enable-experimental-feature ObjCImplementation \
// RUN:   -target %target-stable-abi-triple %t/impl.swift

//--- local.swift

import Foundation

class Base: NSObject {
  @objcDirect func greet() {}
  // expected-note@-1 {{overridden declaration is here}}
  // expected-note@-2 {{instance method 'greet()' is implicitly 'final' because of '@objcDirect'}}
  @objcDirect class func make() {}
  // expected-note@-1 {{overridden declaration is here}}
  // expected-note@-2 {{class method 'make()' is implicitly 'final' because of '@objcDirect'}}

  @objcDirect final func explicitlyFinal() {}
  // expected-note@-1 {{overridden declaration is here}}
  // expected-note@-2 {{instance method 'explicitlyFinal()' is implicitly 'final' because of '@objcDirect'}}

  // 'static' already implies 'final'.
  @objcDirect static func shared() {}
  // expected-note@-1 {{overridden declaration is here}}
}

class Sub: Base {
  override func greet() {}
  // expected-error@-1 {{instance method overrides a 'final' instance method}}
  override class func make() {}
  // expected-error@-1 {{class method overrides a 'final' class method}}
  override func explicitlyFinal() {}
  // expected-error@-1 {{instance method overrides a 'final' instance method}}
  override static func shared() {}
  // expected-error@-1 {{cannot override static method}}
}

open class OpenBase: NSObject {
  @objcDirect open func f() {}
  // expected-error@-1 {{'@objcDirect' declarations are implicitly 'final'; use 'public' instead of 'open'}} {{15-19=public}}
  @objcDirect open static func g() {}
  // expected-error@-1 {{static declarations are implicitly 'final'; use 'public' instead of 'open'}} {{15-19=public}}
}

//--- Lib.swift

import Foundation

open class LibBase: NSObject {
  @objcDirect public func greet() {}
}

//--- client.swift

import Foundation
import Lib

// The deserialized member is final too.
class ClientSub: LibBase {
  override func greet() {}
  // expected-error@-1 {{instance method overrides a 'final' instance method}}
  // expected-error@-2 {{overriding non-open instance method outside of its defining module}}
}

//--- Widget.h

#import <Foundation/Foundation.h>

@interface Widget : NSObject
- (int)direct __attribute__((objc_direct));
@end

//--- impl.swift

import Foundation

// Not made final here, or this would become a Swift-only helper and leave the
// header's method unimplemented.
@objc @implementation extension Widget {
  @objcDirect func direct() -> Int32 { 1 }
}
