// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo() {
  // CHECK-LABEL: sil private [transparent] @_TToFCF10objc_local3fooFT_T_L_3Foog1xSi
  // CHECK-LABEL: sil private [transparent] @_TToFCF10objc_local3fooFT_T_L_3Foos1xSi
  class Foo : NSObject { @objc var x: Int = 0 }
}
