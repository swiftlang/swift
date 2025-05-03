// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %t %t/test.swift -O -emit-sil | %FileCheck %s

// REQUIRES: objc_interop

//--- module.modulemap

module CModule {
  header "c-header.h"
  export *
}


//--- c-header.h

@import Foundation;

@interface TestClass : NSObject
- (void)callHandlerInline: (NS_NOESCAPE _Nonnull dispatch_block_t)block;
@end


//--- test.swift

import CModule

@objc @implementation
extension TestClass {
  // CHECK-LABEL: sil private [thunk] @$sSo9TestClassC4testE17callHandlerInlineyyyyXEFTo :
  // CHECK-NOT:     copy_block
  // CHECK:         apply %0
  // CHECK-NOT:     destroy_value
  // CHECK:       } // end sil function '$sSo9TestClassC4testE17callHandlerInlineyyyyXEFTo' 
  func callHandlerInline(_ handler: () -> Void) {
    handler()
  }
}

