// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

import gizmo

protocol [class_protocol, objc] NSRuncing {
  func runce()
}

// CHECK: define void @_T14objc_protocols12objc_genericUS_9NSRuncing__FT1xQ__T_(%objc_object*, %swift.type* %T, i8** %T.NSRuncing) {
func objc_generic<T:NSRuncing>(x:T) {
  x.runce()
  // CHECK: [[SELECTOR:%.*]] = load i8** @"\01L_selector(runce)", align 8
  // CHECK: call void bitcast (void ()* @objc_msgSend to void (%objc_object*, i8*)*)(%objc_object* {{%.*}}, i8* [[SELECTOR]])
}

/* TODO: ObjC protocol types. Blocked on accurately representing ObjC protocol
 * types at the IR level.
func objc_protocol(x:NSRuncing) {
  x.runce()
}
*/
