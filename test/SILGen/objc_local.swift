// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

func foo() {
  // CHECK-LABEL: sil private [thunk] [ossa] @$s10objc_local3fooyyF3FooL_C1xSivgTo
  // CHECK-LABEL: sil private [thunk] [ossa] @$s10objc_local3fooyyF3FooL_C1xSivsTo
  class Foo : NSObject { @objc var x: Int = 0 }
}
