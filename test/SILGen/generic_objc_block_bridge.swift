// RUN: %target-swift-frontend -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

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
