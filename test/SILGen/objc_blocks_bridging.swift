// RUN: rm -rf %t/clang-module-cache
// RUN: %swift -verify -emit-silgen -module-cache-path %t/clang-module-cache -target x86_64-apple-darwin13 -sdk %S/Inputs -I %S/Inputs -enable-source-import %s | FileCheck %s

import Foundation

@objc class Foo {
  // CHECK-LABEL: sil @_TToFC20objc_blocks_bridging3Foo3foo
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb_dSi_dSi_XFo_dSi_dSi_
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[COPY]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo3foo{{.*}} : $@cc(method) @thin (@owned @callee_owned (Int) -> Int, Int, @owned Foo) -> Int
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], %1, %2)
  @objc func foo(f: Int -> Int, x: Int) -> Int {
    return f(x)
  }

  // CHECK-LABEL: sil @_TToFC20objc_blocks_bridging3Foo3bar
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb_dCSo8NSString_aS__XFo_oSS_oSS_
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[COPY]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo3bar{{.*}} : $@cc(method) @thin (@owned @callee_owned (@owned String) -> @owned String, @owned String, @owned Foo) -> @owned String
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{%.*}}, %2)
  @objc func bar(f: String -> String, x: String) -> String {
    return f(x)
  }

  // CHECK-LABEL: sil @_TToFC20objc_blocks_bridging3Foo3bas
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb_dGSqCSo8NSString__aGSqS___XFo_oGSqSS__oGSqSS__
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[COPY]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo3bas{{.*}} : $@cc(method) @thin (@owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>, @owned Optional<String>, @owned Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{%.*}}, %2)
  @objc func bas(f: String? -> String?, x: String?) -> String? {
    return f(x)
  }

  // Blocks must not be reabstracted when placed in optionals.
  // CHECK-LABEL: sil @_TToFC20objc_blocks_bridging3Foo7optFunc
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         store [[COPY]]
  // CHECK-NOT:     store %0
  // CHECK:         [[GET_OPTIONAL:%.*]] = function_ref @_TFSs17_getOptionalValueU__FGSqQ__Q_
  // CHECK:         [[BLOCK_ADDR:%.*]] = alloc_stack $@cc(cdecl) @objc_block (NSString) -> @autoreleased NSString
  // CHECK:         apply [transparent] [[GET_OPTIONAL]]<@objc_block String -> String>([[BLOCK_ADDR]]#1, {{%.*}}#1)
  // CHECK:         [[BLOCK:%.*]] = load [[BLOCK_ADDR]]
  // TODO: redundant reabstractions here
  // CHECK:         [[BLOCK_THUNK:%.*]] = function_ref @_TTRXFdCb_dCSo8NSString_aS__XFo_oSS_oSS_
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[BLOCK_THUNK]]([[BLOCK]])
  // CHECK:         [[REABSTRACT_THUNK:%.*]] = function_ref @_TTRXFo_oSS_oSS_XFo_iSS_iSS_
  // CHECK:         [[REABSTRACT:%.*]] = partial_apply [[REABSTRACT_THUNK]]([[BRIDGED]])
  // CHECK:         [[NATIVE:%.*]] = function_ref @_TFC20objc_blocks_bridging3Foo7optFunc{{.*}} : $@cc(method) @thin (@owned Optional<String -> String>, @owned String, @owned Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]
  @objc func optFunc(f: (String -> String)?, x: String) -> String? {
    return f?(x)
  }
}

// CHECK-LABEL: sil @_TF20objc_blocks_bridging10callBlocks
func callBlocks(x: Foo,
  f: Int -> Int,
  g: String -> String,
  h: String? -> String?
) -> (Int, String, String?, String?) {
  // CHECK: [[FOO:%.*]] =  class_method [volatile] %0 : $Foo, #Foo.foo!1.foreign
  // CHECK: [[F_BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
  // CHECK: [[F_BLOCK_CAPTURE:%.*]] = project_block_storage [[F_BLOCK_STORAGE]]#1
  // CHECK: store %1 to [[F_BLOCK_CAPTURE]]
  // CHECK: [[F_BLOCK_INVOKE:%.*]] = function_ref @_TTRXFo_dSi_dSi_XFdCb_dSi_dSi_
  // CHECK: [[F_STACK_BLOCK:%.*]] = init_block_storage_header [[F_BLOCK_STORAGE]]#1 : {{.*}}, invoke [[F_BLOCK_INVOKE]]
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

  // CHECK: [[G_BLOCK:%.*]] = copy_block {{%.*}} : $@cc(cdecl) @objc_block (NSString) -> @autoreleased NSString
  // CHECK: store [[G_BLOCK]] to [[SLOT:%.*]]#1
  // CHECK: [[INJECT:%.*]] = function_ref @_TFSs24_injectValueIntoOptionalU__FQ_GSqQ__
  // CHECK: apply [transparent] [[INJECT]]<@objc_block String -> String>({{%.*}}, [[SLOT]]#1)

  return (x.foo(f, x: 0), x.bar(g, x: "one"), x.bas(h, x: "two"), x.optFunc(g, x: "three"))
}

