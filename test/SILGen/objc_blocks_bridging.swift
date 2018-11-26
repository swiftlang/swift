
// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -module-name objc_blocks_bridging -verify -emit-silgen -I %S/Inputs -disable-objc-attr-requires-foundation-module -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -module-name objc_blocks_bridging -verify -emit-silgen -I %S/Inputs -disable-objc-attr-requires-foundation-module -enable-sil-ownership  %s | %FileCheck %s --check-prefix=GUARANTEED

// REQUIRES: objc_interop

import Foundation

@objc class Foo {
// CHECK-LABEL: sil hidden [thunk] @$S20objc_blocks_bridging3FooC3foo_1xS3iXE_SitFTo :
  // CHECK: bb0([[ARG1:%.*]] : @unowned $@convention(block) @noescape (Int) -> Int, {{.*}}, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[ARG1_COPY:%.*]] = copy_block [[ARG1]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @$SS2iIyByd_S2iIegyd_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[ARG1_COPY]])
  // CHECK:         [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[BRIDGED]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @$S20objc_blocks_bridging3FooC3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) (@noescape @callee_guaranteed (Int) -> Int, Int, @guaranteed Foo) -> Int
  // CHECK:         apply [[NATIVE]]([[CONVERT]], {{.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK: } // end sil function '$S20objc_blocks_bridging3FooC3foo_1xS3iXE_SitFTo'
  dynamic func foo(_ f: (Int) -> Int, x: Int) -> Int {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @$S20objc_blocks_bridging3FooC3bar_1xS3SXE_SStFTo : $@convention(objc_method) (@convention(block) @noescape (NSString) -> @autoreleased NSString, NSString, Foo) -> @autoreleased NSString {
  // CHECK:       bb0([[BLOCK:%.*]] : @unowned $@convention(block) @noescape (NSString) -> @autoreleased NSString, [[NSSTRING:%.*]] : @unowned $NSString, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:         [[NSSTRING_COPY:%.*]] = copy_value [[NSSTRING]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @$SSo8NSStringCABIyBya_S2SIeggo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[BLOCK_COPY]])
  // CHECK:         [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[BRIDGED]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @$S20objc_blocks_bridging3FooC3bar{{[_0-9a-zA-Z]*}}F : $@convention(method) (@noescape @callee_guaranteed (@guaranteed String) -> @owned String, @guaranteed String, @guaranteed Foo) -> @owned String
  // CHECK:         apply [[NATIVE]]([[CONVERT]], {{%.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK: } // end sil function '$S20objc_blocks_bridging3FooC3bar_1xS3SXE_SStFTo'
  dynamic func bar(_ f: (String) -> String, x: String) -> String {
    return f(x)
  }

  // GUARANTEED-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SSo8NSStringCABIyBya_S2SIeggo_TR : $@convention(thin) (@guaranteed String, @guaranteed @convention(block) @noescape (NSString) -> @autoreleased NSString) -> @owned String {
  // GUARANTEED: bb0(%0 : @guaranteed $String, [[BLOCK:%.*]] : @guaranteed $@convention(block) @noescape (NSString) -> @autoreleased NSString):
  // GUARANTEED:   [[BRIDGE:%.*]] = function_ref @$SSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // GUARANTEED:   [[NSSTR:%.*]] = apply [[BRIDGE]](%0)
  // GUARANTEED:   apply [[BLOCK]]([[NSSTR]]) : $@convention(block) @noescape (NSString) -> @autoreleased NSString
  // GUARANTEED: } // end sil function '$SSo8NSStringCABIyBya_S2SIeggo_TR'

  // CHECK-LABEL: sil hidden [thunk] @$S20objc_blocks_bridging3FooC3bas_1xSSSgA2FXE_AFtFTo : $@convention(objc_method) (@convention(block) @noescape (Optional<NSString>) -> @autoreleased Optional<NSString>, Optional<NSString>, Foo) -> @autoreleased Optional<NSString> {
  // CHECK:       bb0([[BLOCK:%.*]] : @unowned $@convention(block) @noescape (Optional<NSString>) -> @autoreleased Optional<NSString>, [[OPT_STRING:%.*]] : @unowned $Optional<NSString>, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:         [[OPT_STRING_COPY:%.*]] = copy_value [[OPT_STRING]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @$SSo8NSStringCSgACIyBya_SSSgADIeggo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[BLOCK_COPY]])
  // CHECK:         [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[BRIDGED]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @$S20objc_blocks_bridging3FooC3bas{{[_0-9a-zA-Z]*}}F : $@convention(method) (@noescape @callee_guaranteed (@guaranteed Optional<String>) -> @owned Optional<String>, @guaranteed Optional<String>, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]([[CONVERT]], {{%.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  dynamic func bas(_ f: (String?) -> String?, x: String?) -> String? {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @$S20objc_blocks_bridging3FooC16cFunctionPointer{{[_0-9a-zA-Z]*}}FTo
  // CHECK:       bb0([[F:%.*]] : @trivial $@convention(c) @noescape (Int) -> Int, [[X:%.*]] : @trivial $Int, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @$S20objc_blocks_bridging3FooC16cFunctionPointer{{[_0-9a-zA-Z]*}}F
  // CHECK:         apply [[NATIVE]]([[F]], [[X]], [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:         destroy_value [[SELF_COPY]]
  dynamic func cFunctionPointer(_ fp: @convention(c) (Int) -> Int, x: Int) -> Int {
    _ = fp(x)
  }

  // Blocks and C function pointers must not be reabstracted when placed in optionals.
  // CHECK-LABEL: sil hidden [thunk] @$S20objc_blocks_bridging3FooC7optFunc{{[_0-9a-zA-Z]*}}FTo
  // CHECK: bb0([[ARG0:%.*]] : @unowned $Optional<@convention(block) (NSString) -> @autoreleased NSString>,
  // CHECK:         [[COPY:%.*]] = copy_block [[ARG0]]
  // CHECK:         switch_enum [[COPY]] : $Optional<@convention(block) (NSString) -> @autoreleased NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_BB]]([[BLOCK:%.*]] : @owned $@convention(block) (NSString) -> @autoreleased NSString):
  // TODO: redundant reabstractions here
  // CHECK:         [[BLOCK_THUNK:%.*]] = function_ref @$SSo8NSStringCABIeyBya_S2SIeggo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[BLOCK_THUNK]]([[BLOCK]])
  // CHECK:         enum $Optional<@callee_guaranteed (@guaranteed String) -> @owned String>, #Optional.some!enumelt.1, [[BRIDGED]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @$S20objc_blocks_bridging3FooC7optFunc{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed Optional<@callee_guaranteed (@guaranteed String) -> @owned String>, @guaranteed String, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]
  dynamic func optFunc(_ f: ((String) -> String)?, x: String) -> String? {
    return f?(x)
  }

  // CHECK-LABEL: sil hidden @$S20objc_blocks_bridging3FooC19optCFunctionPointer{{[_0-9a-zA-Z]*}}F
  // CHECK:         switch_enum %0
  //
  // CHECK: bb2([[FP_BUF:%.*]] : @trivial $@convention(c) (NSString) -> @autoreleased NSString):
  dynamic func optCFunctionPointer(_ fp: (@convention(c) (String) -> String)?, x: String) -> String? {
    return fp?(x)
  }
}

// => SEMANTIC SIL TODO: This test needs to be filled out more for ownership
//
// CHECK-LABEL: sil hidden @$S20objc_blocks_bridging10callBlocks{{[_0-9a-zA-Z]*}}F
func callBlocks(_ x: Foo,
  f: @escaping (Int) -> Int,
  g: @escaping (String) -> String,
  h: @escaping (String?) -> String?
) -> (Int, String, String?, String?) {
  // CHECK: bb0([[ARG0:%.*]] : @guaranteed $Foo, [[ARG1:%.*]] : @guaranteed $@callee_guaranteed (Int) -> Int, [[ARG2:%.*]] : @guaranteed $@callee_guaranteed (@guaranteed String) -> @owned String, [[ARG3:%.*]] : @guaranteed $@callee_guaranteed (@guaranteed Optional<String>) -> @owned Optional<String>):
  // CHECK: [[CLOSURE_COPY:%.*]] = copy_value [[ARG1]]
  // CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE_COPY]]
  // CHECK: [[F_BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
  // CHECK: [[F_BLOCK_CAPTURE:%.*]] = project_block_storage [[F_BLOCK_STORAGE]]
  // CHECK: store [[CONVERT]] to [trivial] [[F_BLOCK_CAPTURE]]
  // CHECK: [[F_BLOCK_INVOKE:%.*]] = function_ref @$SS2iIgyd_S2iIyByd_TR
  // CHECK: [[F_STACK_BLOCK:%.*]] = init_block_storage_header [[F_BLOCK_STORAGE]] : {{.*}}, invoke [[F_BLOCK_INVOKE]]
  // CHECK: [[F_BLOCK:%.*]] = copy_block [[F_STACK_BLOCK]]
  // CHECK: [[FOO:%.*]] =  objc_method [[ARG0]] : $Foo, #Foo.foo!1.foreign
  // CHECK: apply [[FOO]]([[F_BLOCK]]

  // CHECK: [[G_BLOCK_INVOKE:%.*]] = function_ref @$SS2SIggo_So8NSStringCABIyBya_TR
  // CHECK: [[G_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[G_BLOCK_INVOKE]]
  // CHECK: [[G_BLOCK:%.*]] = copy_block [[G_STACK_BLOCK]]
  // CHECK: [[BAR:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.bar!1.foreign
  // CHECK: apply [[BAR]]([[G_BLOCK]]

  // CHECK: [[H_BLOCK_INVOKE:%.*]] = function_ref @$SSSSgAAIggo_So8NSStringCSgADIyBya_TR
  // CHECK: [[H_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[H_BLOCK_INVOKE]]
  // CHECK: [[H_BLOCK:%.*]] = copy_block [[H_STACK_BLOCK]]
  // CHECK: [[BAS:%.*]] = objc_method [[ARG0]] : $Foo, #Foo.bas!1.foreign
  // CHECK: apply [[BAS]]([[H_BLOCK]]

  // CHECK: [[G_BLOCK:%.*]] = copy_block {{%.*}} : $@convention(block) (NSString) -> @autoreleased NSString
  // CHECK: enum $Optional<@convention(block) (NSString) -> @autoreleased NSString>, #Optional.some!enumelt.1, [[G_BLOCK]]

  return (x.foo(f, x: 0), x.bar(g, x: "one"), x.bas(h, x: "two"), x.optFunc(g, x: "three"))
}

class Test: NSObject {
  func blockTakesBlock() -> ((Int) -> Int) -> Int {}
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SS2iIgyd_SiIegyd_S2iIyByd_SiIeyByd_TR
// CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[ORIG_BLOCK:%.*]] :
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[BLOCK_COPY]])
// CHECK: [[CONVERT:%.*]] = convert_escape_to_noescape [not_guaranteed] [[CLOSURE]]
// CHECK:         [[RESULT:%.*]] = apply {{%.*}}([[CONVERT]])
// CHECK:         return [[RESULT]]

func clearDraggingItemImageComponentsProvider(_ x: NSDraggingItem) {
  x.imageComponentsProvider = {}
}
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SSayypGIego_So7NSArrayCSgIeyBa_TR
// CHECK:         [[CONVERT:%.*]] = function_ref @$SSa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF
// CHECK:         [[CONVERTED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL:%.*]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[CONVERTED]]
// CHECK:         return [[OPTIONAL]]

// CHECK-LABEL: sil hidden @{{.*}}bridgeNonnullBlockResult{{.*}}
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SSSIego_So8NSStringCSgIeyBa_TR
// CHECK:         [[CONVERT:%.*]] = function_ref @$SSS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:         [[BRIDGED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:         return [[OPTIONAL_BRIDGED]]
func bridgeNonnullBlockResult() {
  nonnullStringBlockResult { return "test" }
}

// CHECK-LABEL: sil hidden @$S20objc_blocks_bridging19bridgeNoescapeBlock2fn5optFnyyyXE_yycSgtF
func bridgeNoescapeBlock(fn: () -> (), optFn: (() -> ())?) {
  // CHECK: [[CLOSURE_FN:%.*]] = function_ref @$S20objc_blocks_bridging19bridgeNoescapeBlock2fn5optFnyyyXE_yycSgtFyyXEfU_
  // CHECK: [[CONV_FN:%.*]] = convert_function [[CLOSURE_FN]]
  // CHECK: [[THICK_FN:%.*]] = thin_to_thick_function [[CONV_FN]]
  // CHECK: [[BLOCK_ALLOC:%.*]] = alloc_stack $@block_storage @noescape @callee_guaranteed () -> ()
  // CHECK: [[BLOCK_ADDR:%.*]] = project_block_storage [[BLOCK_ALLOC]]
  // CHECK: store [[THICK_FN]] to [trivial] [[BLOCK_ADDR]]
  // CHECK: [[THUNK:%.*]] = function_ref @$SIg_IyB_TR : $@convention(c) (@inout_aliasable @block_storage @noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: [[BLOCK_STACK:%.*]] = init_block_storage_header [[BLOCK_ALLOC]] : {{.*}}, invoke [[THUNK]] : {{.*}}

  // FIXME: We're passing the block as a no-escape -- so we don't have to copy it
  // CHECK: [[BLOCK:%.*]] = copy_block [[BLOCK_STACK]]

  // CHECK: [[SOME_BLOCK:%.*]] = enum $Optional<@convention(block) @noescape () -> ()>, #Optional.some!enumelt.1, [[BLOCK]]
  // CHECK: dealloc_stack [[BLOCK_ALLOC]]
  // CHECK: [[FN:%.*]] = function_ref @noescapeBlock : $@convention(c) (Optional<@convention(block) @noescape () -> ()>) -> ()
  // CHECK: apply [[FN]]([[SOME_BLOCK]])
  noescapeBlock { }
  // CHECK: destroy_value [[SOME_BLOCK]]

  // CHECK: [[BLOCK_ALLOC:%.*]] = alloc_stack $@block_storage @noescape @callee_guaranteed () -> ()
  // CHECK: [[BLOCK_ADDR:%.*]] = project_block_storage [[BLOCK_ALLOC]]
  // CHECK: store %0 to [trivial] [[BLOCK_ADDR]]
  // CHECK: [[THUNK:%.*]] = function_ref @$SIg_IyB_TR : $@convention(c) (@inout_aliasable @block_storage @noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: [[BLOCK_STACK:%.*]] = init_block_storage_header [[BLOCK_ALLOC]] : {{.*}}, invoke [[THUNK]] : {{.*}}

  // FIXME: We're passing the block as a no-escape -- so we don't have to copy it
  // CHECK: [[BLOCK:%.*]] = copy_block [[BLOCK_STACK]]

  // CHECK: [[SOME_BLOCK:%.*]] = enum $Optional<@convention(block) @noescape () -> ()>, #Optional.some!enumelt.1, [[BLOCK]]
  // CHECK: dealloc_stack [[BLOCK_ALLOC]]
  // CHECK: [[FN:%.*]] = function_ref @noescapeBlock : $@convention(c) (Optional<@convention(block) @noescape () -> ()>) -> ()
  // CHECK: apply [[FN]]([[SOME_BLOCK]])
  noescapeBlock(fn)
  // CHECK: destroy_value [[SOME_BLOCK]]

  // CHECK: [[NIL_BLOCK:%.*]] = enum $Optional<@convention(block) @noescape () -> ()>, #Optional.none!enumelt
  // CHECK: [[FN:%.*]] = function_ref @noescapeBlock : $@convention(c) (Optional<@convention(block) @noescape () -> ()>) -> ()
  // CHECK: apply [[FN]]([[NIL_BLOCK]])
  noescapeBlock(nil)

  // CHECK: [[CLOSURE_FN:%.*]] = function_ref @$S20objc_blocks_bridging19bridgeNoescapeBlock2fn5optFnyyyXE_yycSgtF
  // CHECK: [[CONV_FN:%.*]] = convert_function [[CLOSURE_FN]]
  // CHECK: [[THICK_FN:%.*]] = thin_to_thick_function [[CONV_FN]]
  // CHECK: [[BLOCK_ALLOC:%.*]] = alloc_stack $@block_storage @noescape @callee_guaranteed () -> ()
  // CHECK: [[BLOCK_ADDR:%.*]] = project_block_storage [[BLOCK_ALLOC]]
  // CHECK: store [[THICK_FN]] to [trivial] [[BLOCK_ADDR]]
  // CHECK: [[THUNK:%.*]] = function_ref @$SIg_IyB_TR : $@convention(c) (@inout_aliasable @block_storage @noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: [[BLOCK_STACK:%.*]] = init_block_storage_header [[BLOCK_ALLOC]] : {{.*}}, invoke [[THUNK]] : {{.*}}

  // FIXME: We're passing the block as a no-escape -- so we don't have to copy it
  // CHECK: [[BLOCK:%.*]] = copy_block [[BLOCK_STACK]]

  // CHECK: [[FN:%.*]] = function_ref @noescapeNonnullBlock : $@convention(c) (@convention(block) @noescape () -> ()) -> ()
  // CHECK: apply [[FN]]([[BLOCK]])
  noescapeNonnullBlock { }
  // CHECK: destroy_value [[BLOCK]]

  // CHECK: [[BLOCK_ALLOC:%.*]] = alloc_stack $@block_storage @noescape @callee_guaranteed () -> ()
  // CHECK: [[BLOCK_ADDR:%.*]] = project_block_storage [[BLOCK_ALLOC]]
  // CHECK: store %0 to [trivial] [[BLOCK_ADDR]]
  // CHECK: [[THUNK:%.*]] = function_ref @$SIg_IyB_TR : $@convention(c) (@inout_aliasable @block_storage @noescape @callee_guaranteed () -> ()) -> ()
  // CHECK: [[BLOCK_STACK:%.*]] = init_block_storage_header [[BLOCK_ALLOC]] : {{.*}}, invoke [[THUNK]] : {{.*}}

  // FIXME: We're passing the block as a no-escape -- so we don't have to copy it
  // CHECK: [[BLOCK:%.*]] = copy_block [[BLOCK_STACK]]

  // CHECK: [[FN:%.*]] = function_ref @noescapeNonnullBlock : $@convention(c) (@convention(block) @noescape () -> ()) -> ()
  // CHECK: apply [[FN]]([[BLOCK]])
  noescapeNonnullBlock(fn)

  noescapeBlock(optFn)

  noescapeBlockAlias { }
  noescapeBlockAlias(fn)
  noescapeBlockAlias(nil)

  noescapeNonnullBlockAlias { }
  noescapeNonnullBlockAlias(fn)
}

public func bridgeNoescapeBlock( optFn: ((String?) -> ())?, optFn2: ((String?) -> ())?) {
  noescapeBlock3(optFn, optFn2, "Foobar")
}


@_silgen_name("_returnOptionalEscape")
public func returnOptionalEscape() -> (() ->())?

public func bridgeNoescapeBlock() {
  noescapeBlock(returnOptionalEscape())
}

class ObjCClass : NSObject {}

extension ObjCClass {
  func someDynamicMethod(closure: (() -> ()) -> ()) {}
}

struct GenericStruct<T> {
  let closure: (() -> ()) -> ()

  func doStuff(o: ObjCClass) {
    o.someDynamicMethod(closure: closure)
  }
}
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SIg_Igy_IyB_IyBy_TR : $@convention(c) (@inout_aliasable @block_storage @noescape @callee_guaranteed (@noescape @callee_guaranteed () -> ()) -> (), @convention(block) @noescape () -> ()) -> () {
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$SIyB_Ieg_TR : $@convention(thin) (@guaranteed @convention(block) @noescape () -> ()) -> ()

// rdar://35402696
func takeOptStringFunction(fn: (String) -> String?) {}
func testGlobalBlock() {
  takeOptStringFunction(fn: GlobalBlock)
}
// CHECK-LABEL: sil hidden @$S20objc_blocks_bridging15testGlobalBlockyyF
// CHECK: global_addr @GlobalBlock : $*@convention(block) (NSString) -> @autoreleased Optional<NSString>
// CHECK: function_ref @$SSo8NSStringCABSgIeyBya_S2SIeggo_TR : $@convention(thin) (@guaranteed String, @guaranteed @convention(block) (NSString) -> @autoreleased Optional<NSString>) -> @owned String
