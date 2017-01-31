// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

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

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SiSiIyByd_SiSiIxyd_TR : $@convention(thin) (Int, @owned @convention(block) (Int) -> Int) -> Int {
