// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -verify -emit-silgen -I %S/Inputs -disable-objc-attr-requires-foundation-module %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc class Foo {
  // CHECK-LABEL: sil hidden [thunk] @_TToFC20objc_blocks_bridging3Foo3foo
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb_dSi_dSi_XFo_dSi_dSi_
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[COPY]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo3foo{{.*}} : $@convention(method) (@owned @callee_owned (Int) -> Int, Int, @guaranteed Foo) -> Int
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], %1, %2)
  dynamic func foo(_ f: (Int) -> Int, x: Int) -> Int {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_TToFC20objc_blocks_bridging3Foo3bar
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb_dCSo8NSString_aS__XFo_oSS_oSS_
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[COPY]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo3bar{{.*}} : $@convention(method) (@owned @callee_owned (@owned String) -> @owned String, @owned String, @guaranteed Foo) -> @owned String
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{%.*}}, %2)
  dynamic func bar(_ f: (String) -> String, x: String) -> String {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk]  @_TToFC20objc_blocks_bridging3Foo3bas
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb_dGSqCSo8NSString__aGSqS___XFo_oGSqSS__oGSqSS__
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[COPY]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo3bas{{.*}} : $@convention(method) (@owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>, @owned Optional<String>, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{%.*}}, %2)
  dynamic func bas(_ f: (String?) -> String?, x: String?) -> String? {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_TToFC20objc_blocks_bridging3Foo16cFunctionPointer
  // CHECK:       bb0([[F:%.*]] : $@convention(c) (Int) -> Int, [[X:%.*]] : $Int, [[SELF:%.*]] : $Foo):
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo16cFunctionPointer
  // CHECK:         apply [[NATIVE]]([[F]], [[X]], [[SELF]])
  dynamic func cFunctionPointer(_ fp: @convention(c) (Int) -> Int, x: Int) -> Int {
    _ = fp(x)
  }

  // Blocks and C function pointers must not be reabstracted when placed in optionals.
  // CHECK-LABEL: sil hidden [thunk] @_TToFC20objc_blocks_bridging3Foo7optFunc
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[BLOCK:%.*]] = unchecked_enum_data [[COPY]]
  // TODO: redundant reabstractions here
  // CHECK:         [[BLOCK_THUNK:%.*]] = function_ref @_TTRXFdCb_dCSo8NSString_aS__XFo_oSS_oSS_
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[BLOCK_THUNK]]([[BLOCK]])
  // CHECK:         [[REABSTRACT_THUNK:%.*]] = function_ref @_TTRXFo_oSS_oSS_XFo_iSS_iSS_
  // CHECK:         [[REABSTRACT:%.*]] = partial_apply [[REABSTRACT_THUNK]]([[BRIDGED]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo7optFunc{{.*}} : $@convention(method) (@owned Optional<(String) -> String>, @owned String, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]
  dynamic func optFunc(_ f: ((String) -> String)?, x: String) -> String? {
    return f?(x)
  }

  // CHECK-LABEL: sil hidden @_TFC20objc_blocks_bridging3Foo19optCFunctionPointer
  // CHECK:         [[FP_BUF:%.*]] = unchecked_enum_data %0
  dynamic func optCFunctionPointer(_ fp: (@convention(c) (String) -> String)?, x: String) -> String? {
    return fp?(x)
  }
}

// CHECK-LABEL: sil hidden @_TF20objc_blocks_bridging10callBlocks
func callBlocks(_ x: Foo,
  f: @escaping (Int) -> Int,
  g: @escaping (String) -> String,
  h: @escaping (String?) -> String?
) -> (Int, String, String?, String?) {
  // CHECK: [[FOO:%.*]] =  class_method [volatile] %0 : $Foo, #Foo.foo!1.foreign
  // CHECK: [[F_BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
  // CHECK: [[F_BLOCK_CAPTURE:%.*]] = project_block_storage [[F_BLOCK_STORAGE]]
  // CHECK: store %1 to [[F_BLOCK_CAPTURE]]
  // CHECK: [[F_BLOCK_INVOKE:%.*]] = function_ref @_TTRXFo_dSi_dSi_XFdCb_dSi_dSi_
  // CHECK: [[F_STACK_BLOCK:%.*]] = init_block_storage_header [[F_BLOCK_STORAGE]] : {{.*}}, invoke [[F_BLOCK_INVOKE]]
  // CHECK: [[F_BLOCK:%.*]] = copy_block [[F_STACK_BLOCK]]
  // CHECK: apply [[FOO]]([[F_BLOCK]]

  // CHECK: [[BAR:%.*]] = class_method [volatile] %0 : $Foo, #Foo.bar!1.foreign
  // CHECK: [[G_BLOCK_INVOKE:%.*]] = function_ref @_TTRXFo_oSS_oSS_XFdCb_dCSo8NSString_aS__
  // CHECK: [[G_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[G_BLOCK_INVOKE]]
  // CHECK: [[G_BLOCK:%.*]] = copy_block [[G_STACK_BLOCK]]
  // CHECK: apply [[BAR]]([[G_BLOCK]]

  // CHECK: [[BAS:%.*]] = class_method [volatile] %0 : $Foo, #Foo.bas!1.foreign
  // CHECK: [[H_BLOCK_INVOKE:%.*]] = function_ref @_TTRXFo_oGSqSS__oGSqSS__XFdCb_dGSqCSo8NSString__aGSqS___
  // CHECK: [[H_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[H_BLOCK_INVOKE]]
  // CHECK: [[H_BLOCK:%.*]] = copy_block [[H_STACK_BLOCK]]
  // CHECK: apply [[BAS]]([[H_BLOCK]]

  // CHECK: [[G_BLOCK:%.*]] = copy_block {{%.*}} : $@convention(block) (NSString) -> @autoreleased NSString
  // CHECK: enum $Optional<@convention(block) (NSString) -> NSString>, #Optional.some!enumelt.1, [[G_BLOCK]]

  return (x.foo(f, x: 0), x.bar(g, x: "one"), x.bas(h, x: "two"), x.optFunc(g, x: "three"))
}

class Test: NSObject {
  func blockTakesBlock() -> ((Int) -> Int) -> Int {}
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo_oXFo_dSi_dSi__dSi_XFdCb_dXFdCb_dSi_dSi__dSi_
// CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[ORIG_BLOCK:%.*]] :
// CHECK:         [[CLOSURE:%.*]] = partial_apply {{%.*}}([[BLOCK_COPY]])
// CHECK:         [[RESULT:%.*]] = apply {{%.*}}([[CLOSURE]])
// CHECK:         return [[RESULT]]

func clearDraggingItemImageComponentsProvider(_ x: NSDraggingItem) {
  x.imageComponentsProvider = {}
}
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__oGSaP___XFdCb__aGSqCSo7NSArray__
// CHECK:         [[CONVERT:%.*]] = function_ref @_TFE10FoundationSa19_bridgeToObjectiveCfT_CSo7NSArray
// CHECK:         [[CONVERTED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL:%.*]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[CONVERTED]]
// CHECK:         return [[OPTIONAL]]

// CHECK-LABEL: sil hidden @{{.*}}bridgeNonnullBlockResult{{.*}}
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_TTRXFo__oSS_XFdCb__aGSqCSo8NSString__
// CHECK:         [[CONVERT:%.*]] = function_ref @_TFE10FoundationSS19_bridgeToObjectiveCfT_CSo8NSString
// CHECK:         [[BRIDGED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:         return [[OPTIONAL_BRIDGED]]
func bridgeNonnullBlockResult() {
  nonnullStringBlockResult { return "test" }
}

// CHECK-LABEL: sil hidden @{{.*}}bridgeNoescapeBlock{{.*}}
func bridgeNoescapeBlock() {
  // CHECK: function_ref @_TTRXFo___XFdCb___
  noescapeBlockAlias { }
  // CHECK: function_ref @_TTRXFo___XFdCb___
  noescapeNonnullBlockAlias { }
}
