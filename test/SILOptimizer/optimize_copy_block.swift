// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -I %t %t/test.swift -enable-experimental-feature CopyBlockOptimization -O -emit-sil | %FileCheck %s

// REQUIRES: objc_interop
// REQUIRES: swift_feature_CopyBlockOptimization

//--- module.modulemap

module CModule {
  header "c-header.h"
  export *
}


//--- c-header.h

@import Foundation;

@interface TestClass : NSObject
- (void)callHandler1: (NS_NOESCAPE _Nonnull dispatch_block_t)block;
- (void)callHandler2: (NS_NOESCAPE _Nonnull dispatch_block_t)block;
- (void)callHandler3: (NS_NOESCAPE _Nonnull dispatch_block_t)block;
@end


//--- test.swift

import CModule

@objc @implementation
extension TestClass {
  // CHECK-LABEL: sil private [thunk] @$sSo9TestClassC4testE12callHandler1yyyyXEFTo :
  // CHECK-NOT:     copy_block
  // CHECK:         apply %0
  // CHECK-NOT:     destroy_value
  // CHECK:       } // end sil function '$sSo9TestClassC4testE12callHandler1yyyyXEFTo'
  func callHandler1(_ handler: () -> Void) {
    handler()
  }

  // CHECK-LABEL: sil private [thunk] @$sSo9TestClassC4testE12callHandler2yyyyXEFTo :
  // CHECK-NOT:     copy_block
  // CHECK:       } // end sil function '$sSo9TestClassC4testE12callHandler2yyyyXEFTo' 
  func callHandler2(_ handler: () -> Void) {
    callee(handler)
  }

  // CHECK-LABEL: sil private [thunk] @$sSo9TestClassC4testE12callHandler3yyyyXEFTo :
  // CHECK-NOT:     copy_block
  // CHECK:       } // end sil function '$sSo9TestClassC4testE12callHandler3yyyyXEFTo' 
  func callHandler3(_ handler: () -> Void) {
    callClosure {
      handler()
    }
  }
}

@_silgen_name("callee")
func callee(_ handler: () -> Void)

@_silgen_name("callClosure")
func callClosure(_ closure: () -> ())
