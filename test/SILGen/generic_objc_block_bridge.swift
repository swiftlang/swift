// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s -enable-objc-interop | %FileCheck %s

import Foundation

class Butt: NSObject {
  @objc dynamic func butt(_ b: (Int) -> Int) {}
}

class Tubb<GenericParamName>: Butt {
  override func butt(_ b: (Int) -> Int) {
    super.butt(b)
  }
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SS2iIyByd_S2iIegyd_TR : $@convention(thin) (Int, @guaranteed @convention(block) @noescape (Int) -> Int) -> Int {
