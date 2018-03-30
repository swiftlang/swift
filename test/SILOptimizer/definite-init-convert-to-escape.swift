// RUN: %target-swift-frontend -module-name A -verify -emit-sil -import-objc-header %S/Inputs/Closure.h -disable-objc-attr-requires-foundation-module -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name A -verify -emit-sil -import-objc-header %S/Inputs/Closure.h -disable-objc-attr-requires-foundation-module -enable-sil-ownership -Xllvm -sil-di-disable-convert-escape-to-noescape-switch-peephole %s | %FileCheck %s --check-prefix=NOPEEPHOLE

// REQUIRES: objc_interop

import Foundation

// Make sure that we keep the escaping closures alive accross the ultimate call.
// CHECK-LABEL: sil @$S1A19bridgeNoescapeBlock5optFn0D3Fn2yySSSgcSg_AFtF
// CHECK: bb0
// CHECK: alloc_stack
// CHECK: alloc_stack
// CHECK:  retain_value %0
// CHECK: bb2
// CHECK:  destroy_addr
// CHECK:  enum
// CHECK:  store
// CHECK:  convert_escape_to_noescape %
// CHECK-NOT:  strong_release
// CHECK: bb6
// CHECK:  retain_value %1
// CHECK: bb8
// CHECK:  destroy_addr
// CHECK:  enum
// CHECK:  store
// CHECK:  convert_escape_to_noescape %
// CHECK-NOT:  strong_release
// CHECK: br
// CHECK: bb12
// CHECK:  [[F:%.*]] = function_ref @noescapeBlock3
// CHECK:  apply [[F]]
// CHECK:  release_value {{.*}} : $Optional<NSString>
// CHECK:  release_value {{.*}} : $Optional<@convention(block) @noescape (Optional<NSString>) -> ()>
// CHECK:  release_value {{.*}} : $Optional<@convention(block) @noescape (Optional<NSString>)
// CHECK:  release_value %1 : $Optional<@callee_guaranteed (@owned Optional<String>) -> ()>
// CHECK:  release_value %0 : $Optional<@callee_guaranteed (@owned Optional<String>) -> ()>
// CHECK:  destroy_addr {{.*}}Optional<@callee_guaranteed
// CHECK:  dealloc_stack
// CHECK:  destroy_addr {{.*}}Optional<@callee_guaranteed
// CHECK:  dealloc_stack
public func bridgeNoescapeBlock( optFn: ((String?) -> ())?, optFn2: ((String?) -> ())?) {
  noescapeBlock3(optFn, optFn2, "Foobar")
}


@_silgen_name("_returnOptionalEscape")
public func returnOptionalEscape() -> (() ->())?

// Make sure that we keep the escaping closure alive accross the ultimate call.

// CHECK-LABEL: sil @$S1A19bridgeNoescapeBlockyyF : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK:  [[SLOT:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> ()>
// CHECK:  [[NONE:%.*]] = enum $Optional
// CHECK:  store [[NONE]] to [[SLOT]]
// CHECK:  [[V0:%.*]] = function_ref @_returnOptionalEscape
// CHECK:  [[V1:%.*]] = apply [[V0]]
// CHECK: bb2:
// CHECK:  [[V2:%.*]] = unchecked_enum_data
// CHECK:  destroy_addr [[SLOT]]
// CHECK:  [[SOME:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.some!enumelt.1, [[V2]]
// CHECK:  store [[SOME]] to [[SLOT]]
// CHECK:  convert_escape_to_noescape %
// CHECK-NOT:  strong_release
// CHECK:  br
// CHECK: bb6({{.*}} : $Optional<@convention(block) @noescape () -> ()>)
// CHECK:  [[F:%.*]] = function_ref @noescapeBlock
// CHECK:  apply [[F]]({{.*}})
// CHECK:  destroy_addr [[SLOT]]
// CHECK:  dealloc_stack  [[SLOT]]

// NOPEEPHOLE-LABEL: sil @$S1A19bridgeNoescapeBlockyyF : $@convention(thin) () -> () {
// NOPEEPHOLE: bb0:
// NOPEEPHOLE:  [[SLOT:%.*]] = alloc_stack $Optional<@callee_guaranteed () -> ()>
// NOPEEPHOLE:  [[NONE:%.*]] = enum $Optional
// NOPEEPHOLE:  store [[NONE]] to [[SLOT]]
// NOPEEPHOLE:  [[V0:%.*]] = function_ref @_returnOptionalEscape
// NOPEEPHOLE:  [[V1:%.*]] = apply [[V0]]
// NOPEEPHOLE: bb2:
// NOPEEPHOLE:  [[V2:%.*]] = unchecked_enum_data
// NOPEEPHOLE:  destroy_addr [[SLOT]]
// NOPEEPHOLE:  [[SOME:%.*]] = enum $Optional<@callee_guaranteed () -> ()>, #Optional.some!enumelt.1, [[V2]]
// NOPEEPHOLE:  store [[SOME]] to [[SLOT]]
// NOPEEPHOLE:  convert_escape_to_noescape %
// NOPEEPHOLE-NOT:  strong_release
// NOPEEPHOLE:  br
// NOPEEPHOLE: bb6({{.*}} : $Optional<@convention(block) @noescape () -> ()>)
// NOPEEPHOLE:  [[F:%.*]] = function_ref @noescapeBlock
// NOPEEPHOLE:  apply [[F]]({{.*}})
// NOPEEPHOLE:  destroy_addr [[SLOT]]
// NOPEEPHOLE:  dealloc_stack  [[SLOT]]
public func bridgeNoescapeBlock() {
  noescapeBlock(returnOptionalEscape())
}


