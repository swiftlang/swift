// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk) -emit-silgen -enable-sil-ownership %s | %FileCheck %s
// REQUIRES: objc_interop

import Foundation

@objc protocol P {
  func p(_: String)
}
@objc class C: NSObject {
  func c(_: String) {}
}

// CHECK-LABEL: sil shared [serializable] [thunk] @_T0{{.*}}1P{{.*}}1p{{.*}} : $@convention(method) <Self where Self : P> (@owned String, @guaranteed Self) -> ()
func foo(x: Bool, y: C & P) -> (String) -> () {
  return x ? y.c : y.p
}
