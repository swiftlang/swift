// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays

// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -emit-silgen %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

class Butt: NSObject {
  dynamic func butt(_ b: (Int) -> Int) {}
}

class Tubb<GenericParamName>: Butt {
  override func butt(_ b: (Int) -> Int) {
    super.butt(b)
  }
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRGrXFdCb_dSi_dSi_XFo_dSi_dSi_ : $@convention(thin) <GenericParamName> (Int, @owned @convention(block) (Int) -> Int) -> Int {
