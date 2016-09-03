// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo() {
  // CHECK-LABEL: sil private [transparent] [thunk] @_TToFCF10objc_local3fooFT_T_L_3Foog1xSi
  // CHECK-LABEL: sil private [transparent] [thunk] @_TToFCF10objc_local3fooFT_T_L_3Foos1xSi
  class Foo : NSObject { @objc var x: Int = 0 }
}
