// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-sil-ownership -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo() {
  // CHECK-LABEL: sil private [thunk] @$s10objc_local3fooyyF3FooL_C1xSivgTo
  // CHECK-LABEL: sil private [thunk] @$s10objc_local3fooyyF3FooL_C1xSivsTo
  class Foo : NSObject { @objc var x: Int = 0 }
}
