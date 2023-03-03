// RUN: %target-swift-frontend %s -emit-ir | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

// Make sure we emit a metadata accessor for foreign types even if the type
// metadata is not required by this TU. Another TU could require it and the
// linker could choose the less defined one of the two.

// CHECK: @"$sSo8_NSRangeVMn" = linkonce_odr hidden constant <{ {{.*}}sSo8_NSRangeVMa{{.*}} }>, section "__TEXT,__constg_swiftt"

func use(_ closure: @escaping (Int) -> ()) {}

public func captureRange(_ r: NSRange?) {
  var l = r
  use {
    if $0 == 0 {
      l = NSRange()
    }
  }
}
