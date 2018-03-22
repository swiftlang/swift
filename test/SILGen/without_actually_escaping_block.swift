// RUN: %target-swift-frontend -module-name without_actually_escaping -emit-silgen -enable-sil-ownership %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

typealias Callback = @convention(block) () -> Void

// CHECK-LABEL: sil {{.*}} @$S25without_actually_escaping9testBlock5blockyyyXB_tF
// CHECK: bb0([[ARG:%.*]] : @owned $@convention(block) @noescape () -> ()):
// CHECK:  [[C1:%.*]] = copy_block [[ARG]]
// CHECK:  [[B1:%.*]] = begin_borrow [[C1]]
// CHECK:  [[C2:%.*]] = copy_value [[B1]]
// CHECK:  [[CVT:%.*]] = convert_function [[C2]] : $@convention(block) @noescape () -> () to $@convention(block) () -> ()
// CHECK:  [[FN:%.*]] = function_ref @$S25without_actually_escaping9testBlock5blockyyyXB_tFyyyXBXEfU_
// CHECK:  apply [[FN]]([[CVT]])
// CHECK:  end_borrow [[B1]] from [[C1]]
// CHECK:  destroy_value [[C1]]
// CHECK:  destroy_value [[ARG]]
// CHECK:  return

func testBlock(block: Callback) {
  withoutActuallyEscaping(block) { $0() }
}
