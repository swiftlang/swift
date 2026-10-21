// REQUIRES: objc_interop
// REQUIRES: swift_feature_ObjCDirect
// REQUIRES: swift_feature_ObjCImplementation

// A class declared in ObjC can have direct members even when Swift imports it as a nested type.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify \
// RUN:   -import-objc-header %t/Widget.h \
// RUN:   -enable-experimental-feature ObjCDirect \
// RUN:   -enable-experimental-feature ObjCImplementation \
// RUN:   -target %target-stable-abi-triple %t/main.swift

//--- Widget.h

#import <Foundation/Foundation.h>

@interface Widget : NSObject
@end

__attribute__((swift_name("Widget.Part")))
@interface WidgetPart : NSObject
- (int)direct __attribute__((objc_direct));
@end

//--- main.swift

import Foundation

@objc @implementation extension Widget.Part {
  @objcDirect func direct() -> Int32 { 1 }
}

extension Widget.Part {
  @objcDirect func helper() {}
}

extension Widget {
  @objc(WidgetSwiftPart) class SwiftPart: NSObject {
    @objcDirect func f() {}
    // expected-error@-1 {{'@objcDirect' can only be applied to members of a class declared at file scope}}
  }
}
