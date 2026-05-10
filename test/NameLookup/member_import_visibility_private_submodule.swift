// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -typecheck %t/main.swift %t/other.swift \
// RUN:   -F %t/Frameworks \
// RUN:   -enable-upcoming-feature MemberImportVisibility \
// RUN:   -verify \
// RUN:   -verify-additional-prefix member-visibility-

// RUN: %target-swift-frontend -typecheck %t/main.swift %t/other.swift \
// RUN:   -F %t/Frameworks \
// RUN:   -verify \
// RUN:   -verify-additional-prefix no-member-visibility-

// REQUIRES: objc_interop
// REQUIRES: swift_feature_MemberImportVisibility

//--- Frameworks/ToasterKit.framework/Headers/ToasterKit.h
#import <Foundation/Foundation.h>

@interface Toaster : NSObject
- (void)makeToast __attribute__((deprecated("ToasterKit.h")));
@end

//--- Frameworks/ToasterKit.framework/PrivateHeaders/ToasterKit_Private.h
#import <ToasterKit/ToasterKit.h>

@protocol ToastMaker
- (void)makeToast __attribute__((deprecated("ToasterKit_Private.h")));
@end

@interface Toaster () <ToastMaker>
@end


//--- Frameworks/ToasterKit.framework/Modules/module.modulemap
framework module ToasterKit {
  umbrella header "ToasterKit.h"
  export *
  module * { export * }
}

//--- Frameworks/ToasterKit.framework/Modules/module.private.modulemap
framework module ToasterKit_Private {
  header "ToasterKit_Private.h"
  export *
}

//--- main.swift

import ToasterKit

func test(t: Toaster) {
  t.makeToast()
  // expected-no-member-visibility-warning@-1 {{'makeToast()' is deprecated: ToasterKit.h}}
  // expected-member-visibility-warning@-2 {{'makeToast()' is deprecated: ToasterKit.h}}
}

//--- other.swift

import ToasterKit
import ToasterKit_Private
