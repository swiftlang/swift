// Test various ObjC-specific features with C arrays.

// REQUIRES: objc_interop

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unrelated %t/typecheck.swift -I %t/include -verify-additional-prefix legacy-c-array-

//--- include/module.modulemap
module CLib {
  header "CLib.h"
  export *
}

//--- include/CLib.h
#import <Foundation/Foundation.h>

NS_ASSUME_NONNULL_BEGIN

@interface Direct : NSObject
- (void)takes:(int (* _Null_unspecified)[4])p;
@property (nonatomic, null_unspecified) int (*prop)[4];
@end

NS_ASSUME_NONNULL_END

//--- typecheck.swift
import CLib

// Modern projection of a method should not be visible in legacy mode.

@available(anyAppleOS 26.0, *)
func useHiddenProjection(_ c: Direct, _ p: UnsafeMutablePointer<InlineArray<4, CInt>>?) {
  c.takes(p)
  // expected-legacy-c-array-error@-1 {{instance method 'takes' was not imported because its C array type could not be represented as a tuple}}
  c.prop = p
  // expected-legacy-c-array-error@-1 {{property 'prop' was not imported because its C array type could not be represented as a tuple}}
}
