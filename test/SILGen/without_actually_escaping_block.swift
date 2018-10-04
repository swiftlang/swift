// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-emit-silgen(mock-sdk: -sdk %S/Inputs -I %t) -module-name without_actually_escaping -enable-sil-ownership %s -sdk %S/Inputs -enable-objc-interop | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

typealias Callback = @convention(block) () -> Void

// CHECK-LABEL: sil {{.*}} @$s25without_actually_escaping9testBlock5blockyyyXB_tF
// CHECK: bb0([[ARG:%.*]] : @guaranteed $@convention(block) @noescape () -> ()):
// CHECK:  [[C1:%.*]] = copy_block [[ARG]]
// CHECK:  [[B1:%.*]] = begin_borrow [[C1]]
// CHECK:  [[C2:%.*]] = copy_value [[B1]]
// CHECK:  [[CVT:%.*]] = convert_function [[C2]] : $@convention(block) @noescape () -> () to [without_actually_escaping] $@convention(block) () -> ()
// CHECK:  [[B2:%.*]] = begin_borrow [[CVT]]
// CHECK:  [[FN:%.*]] = function_ref @$s25without_actually_escaping9testBlock5blockyyyXB_tFyyyXBXEfU_
// CHECK:  apply [[FN]]([[B2]])
// CHECK:  end_borrow [[B2]]
// CHECK:  destroy_value [[CVT]]
// CHECK:  end_borrow [[B1]]
// CHECK:  destroy_value [[C1]]
// CHECK:  return

func testBlock(block: Callback) {
  withoutActuallyEscaping(block) { $0() }
}
