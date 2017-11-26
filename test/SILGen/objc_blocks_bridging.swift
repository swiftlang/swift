// RUN: %empty-directory(%t)
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -verify -emit-silgen -I %S/Inputs -disable-objc-attr-requires-foundation-module -enable-sil-ownership %s | %FileCheck %s
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -verify -emit-silgen -I %S/Inputs -disable-objc-attr-requires-foundation-module -enable-sil-ownership -enable-guaranteed-closure-contexts %s | %FileCheck %s --check-prefix=GUARANTEED

// REQUIRES: objc_interop

import Foundation

@objc class Foo {
// CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC3fooS3ic_Si1xtFTo :
  // CHECK: bb0([[ARG1:%.*]] : @unowned $@convention(block) @noescape (Int) -> Int, {{.*}}, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[ARG1_COPY:%.*]] = copy_block [[ARG1]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @_T0S2iIyByd_S2iIgyd_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[ARG1_COPY]])
  // CHECK:         [[CONVERT:%.*]] = convert_function [[BRIDGED]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned @noescape @callee_guaranteed (Int) -> Int, Int, @guaranteed Foo) -> Int
  // CHECK:         apply [[NATIVE]]([[CONVERT]], {{.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK: } // end sil function '_T020objc_blocks_bridging3FooC3fooS3ic_Si1xtFTo'
  dynamic func foo(_ f: (Int) -> Int, x: Int) -> Int {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC3barS3Sc_SS1xtFTo : $@convention(objc_method) (@convention(block) @noescape (NSString) -> @autoreleased NSString, NSString, Foo) -> @autoreleased NSString {
  // CHECK:       bb0([[BLOCK:%.*]] : @unowned $@convention(block) @noescape (NSString) -> @autoreleased NSString, [[NSSTRING:%.*]] : @unowned $NSString, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:         [[NSSTRING_COPY:%.*]] = copy_value [[NSSTRING]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @_T0So8NSStringCABIyBya_S2SIgxo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[BLOCK_COPY]])
  // CHECK:         [[CONVERT:%.*]] = convert_function [[BRIDGED]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC3bar{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned @noescape @callee_guaranteed (@owned String) -> @owned String, @owned String, @guaranteed Foo) -> @owned String
  // CHECK:         apply [[NATIVE]]([[CONVERT]], {{%.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK: } // end sil function '_T020objc_blocks_bridging3FooC3barS3Sc_SS1xtFTo'
  dynamic func bar(_ f: (String) -> String, x: String) -> String {
    return f(x)
  }

  // GUARANTEED-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T0So8NSStringCABIyBya_S2SIgxo_TR : $@convention(thin) (@owned String, @guaranteed @convention(block) @noescape (NSString) -> @autoreleased NSString) -> @owned String {
  // GUARANTEED: bb0(%0 : @owned $String, [[BLOCK:%.*]] : @guaranteed $@convention(block) @noescape (NSString) -> @autoreleased NSString):
  // GUARANTEED:   [[BRIDGE:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
  // GUARANTEED:   [[STR:%.*]] = begin_borrow %0 : $String
  // GUARANTEED:   [[NSSTR:%.*]] = apply [[BRIDGE]]([[STR]])
  // GUARANTEED:   apply [[BLOCK]]([[NSSTR]]) : $@convention(block) @noescape (NSString) -> @autoreleased NSString
  // GUARANTEED: } // end sil function '_T0So8NSStringCABIyBya_S2SIgxo_TR'

  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC3basSSSgA2Ec_AE1xtFTo : $@convention(objc_method) (@convention(block) @noescape (Optional<NSString>) -> @autoreleased Optional<NSString>, Optional<NSString>, Foo) -> @autoreleased Optional<NSString> {
  // CHECK:       bb0([[BLOCK:%.*]] : @unowned $@convention(block) @noescape (Optional<NSString>) -> @autoreleased Optional<NSString>, [[OPT_STRING:%.*]] : @unowned $Optional<NSString>, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:         [[OPT_STRING_COPY:%.*]] = copy_value [[OPT_STRING]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @_T0So8NSStringCSgACIyBya_SSSgADIgxo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[THUNK]]([[BLOCK_COPY]])
  // CHECK:         [[CONVERT:%.*]] = convert_function [[BRIDGED]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC3bas{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned @noescape @callee_guaranteed (@owned Optional<String>) -> @owned Optional<String>, @owned Optional<String>, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]([[CONVERT]], {{%.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  dynamic func bas(_ f: (String?) -> String?, x: String?) -> String? {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC16cFunctionPointer{{[_0-9a-zA-Z]*}}FTo
  // CHECK:       bb0([[F:%.*]] : @trivial $@convention(c) @noescape (Int) -> Int, [[X:%.*]] : @trivial $Int, [[SELF:%.*]] : @unowned $Foo):
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC16cFunctionPointer{{[_0-9a-zA-Z]*}}F
  // CHECK:         apply [[NATIVE]]([[F]], [[X]], [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK:         destroy_value [[SELF_COPY]]
  dynamic func cFunctionPointer(_ fp: @convention(c) (Int) -> Int, x: Int) -> Int {
    _ = fp(x)
  }

  // Blocks and C function pointers must not be reabstracted when placed in optionals.
  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC7optFunc{{[_0-9a-zA-Z]*}}FTo
  // CHECK: bb0([[ARG0:%.*]] : @unowned $Optional<@convention(block) (NSString) -> @autoreleased NSString>,
  // CHECK:         [[COPY:%.*]] = copy_block [[ARG0]]
  // CHECK:         switch_enum [[COPY]] : $Optional<@convention(block) (NSString) -> @autoreleased NSString>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
  // CHECK: [[SOME_BB]]([[BLOCK:%.*]] : @owned $@convention(block) (NSString) -> @autoreleased NSString):
  // TODO: redundant reabstractions here
  // CHECK:         [[BLOCK_THUNK:%.*]] = function_ref @_T0So8NSStringCABIeyBya_S2SIegxo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [callee_guaranteed] [[BLOCK_THUNK]]([[BLOCK]])
  // CHECK:         enum $Optional<@callee_guaranteed (@owned String) -> @owned String>, #Optional.some!enumelt.1, [[BRIDGED]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC7optFunc{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned Optional<@callee_guaranteed (@owned String) -> @owned String>, @owned String, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]
  dynamic func optFunc(_ f: ((String) -> String)?, x: String) -> String? {
    return f?(x)
  }

  // CHECK-LABEL: sil hidden @_T020objc_blocks_bridging3FooC19optCFunctionPointer{{[_0-9a-zA-Z]*}}F
  // CHECK:         [[FP_BUF:%.*]] = unchecked_enum_data %0
  dynamic func optCFunctionPointer(_ fp: (@convention(c) (String) -> String)?, x: String) -> String? {
    return fp?(x)
  }
}

// => SEMANTIC SIL TODO: This test needs to be filled out more for ownership
//
// CHECK-LABEL: sil hidden @_T020objc_blocks_bridging10callBlocks{{[_0-9a-zA-Z]*}}F
func callBlocks(_ x: Foo,
  f: @escaping (Int) -> Int,
  g: @escaping (String) -> String,
  h: @escaping (String?) -> String?
) -> (Int, String, String?, String?) {
  // CHECK: bb0([[ARG0:%.*]] : @owned $Foo, [[ARG1:%.*]] : @owned $@callee_guaranteed (Int) -> Int, [[ARG2:%.*]] : @owned $@callee_guaranteed (@owned String) -> @owned String, [[ARG3:%.*]] : @owned $@callee_guaranteed (@owned Optional<String>) -> @owned Optional<String>):
  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
  // CHECK: [[CLOSURE_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
  // CHECK: [[CONVERT:%.*]] = convert_function [[CLOSURE_COPY]]
  // CHECK: [[F_BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
  // CHECK: [[F_BLOCK_CAPTURE:%.*]] = project_block_storage [[F_BLOCK_STORAGE]]
  // CHECK: store [[CONVERT]] to [init] [[F_BLOCK_CAPTURE]]
  // CHECK: [[F_BLOCK_INVOKE:%.*]] = function_ref @_T0S2iIgyd_S2iIyByd_TR
  // CHECK: [[F_STACK_BLOCK:%.*]] = init_block_storage_header [[F_BLOCK_STORAGE]] : {{.*}}, invoke [[F_BLOCK_INVOKE]]
  // CHECK: [[F_BLOCK:%.*]] = copy_block [[F_STACK_BLOCK]]
  // CHECK: [[FOO:%.*]] =  objc_method [[BORROWED_ARG0]] : $Foo, #Foo.foo!1.foreign
  // CHECK: apply [[FOO]]([[F_BLOCK]]

  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[G_BLOCK_INVOKE:%.*]] = function_ref @_T0S2SIgxo_So8NSStringCABIyBya_TR
  // CHECK: [[G_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[G_BLOCK_INVOKE]]
  // CHECK: [[G_BLOCK:%.*]] = copy_block [[G_STACK_BLOCK]]
  // CHECK: [[BAR:%.*]] = objc_method [[BORROWED_ARG0]] : $Foo, #Foo.bar!1.foreign
  // CHECK: apply [[BAR]]([[G_BLOCK]]

  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[H_BLOCK_INVOKE:%.*]] = function_ref @_T0SSSgAAIgxo_So8NSStringCSgADIyBya_TR
  // CHECK: [[H_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[H_BLOCK_INVOKE]]
  // CHECK: [[H_BLOCK:%.*]] = copy_block [[H_STACK_BLOCK]]
  // CHECK: [[BAS:%.*]] = objc_method [[BORROWED_ARG0]] : $Foo, #Foo.bas!1.foreign
  // CHECK: apply [[BAS]]([[H_BLOCK]]

  // CHECK: [[G_BLOCK:%.*]] = copy_block {{%.*}} : $@convention(block) (NSString) -> @autoreleased NSString
  // CHECK: enum $Optional<@convention(block) (NSString) -> @autoreleased NSString>, #Optional.some!enumelt.1, [[G_BLOCK]]

  return (x.foo(f, x: 0), x.bar(g, x: "one"), x.bas(h, x: "two"), x.optFunc(g, x: "three"))
}

class Test: NSObject {
  func blockTakesBlock() -> ((Int) -> Int) -> Int {}
}

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T0S2iIgyd_SiIegxd_S2iIyByd_SiIeyByd_TR
// CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[ORIG_BLOCK:%.*]] :
// CHECK:         [[CLOSURE:%.*]] = partial_apply [callee_guaranteed] {{%.*}}([[BLOCK_COPY]])
// CHECK: [[CONVERT:%.*]] = convert_function [[CLOSURE]]
// CHECK:         [[RESULT:%.*]] = apply {{%.*}}([[CONVERT]])
// CHECK:         return [[RESULT]]

func clearDraggingItemImageComponentsProvider(_ x: NSDraggingItem) {
  x.imageComponentsProvider = {}
}
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T0SayypGIego_So7NSArrayCSgIeyBa_TR
// CHECK:         [[CONVERT:%.*]] = function_ref @_T0Sa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF
// CHECK:         [[CONVERTED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL:%.*]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[CONVERTED]]
// CHECK:         return [[OPTIONAL]]

// CHECK-LABEL: sil hidden @{{.*}}bridgeNonnullBlockResult{{.*}}
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T0SSIego_So8NSStringCSgIeyBa_TR
// CHECK:         [[CONVERT:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:         [[BRIDGED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:         return [[OPTIONAL_BRIDGED]]
func bridgeNonnullBlockResult() {
  nonnullStringBlockResult { return "test" }
}

// CHECK-LABEL: sil hidden @{{.*}}bridgeNoescapeBlock{{.*}}
func bridgeNoescapeBlock() {
  // CHECK: function_ref @_T0Ig_IyB_TR
  noescapeBlockAlias { }
  // CHECK: function_ref @_T0Ig_IyB_TR
  noescapeNonnullBlockAlias { }
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

// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T0Ig_Igx_IyB_IyBy_TR : $@convention(c) (@inout_aliasable @block_storage @noescape @callee_guaranteed (@owned @noescape @callee_guaranteed () -> ()) -> (), @convention(block) @noescape () -> ()) -> ()
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @_T0IyB_Ig_TR : $@convention(thin) (@guaranteed @convention(block) @noescape () -> ()) -> ()

// rdar://35402696
func takeOptStringFunction(fn: (String) -> String?) {}
func testGlobalBlock() {
  takeOptStringFunction(fn: GlobalBlock)
}
// CHECK-LABEL: sil hidden @_T020objc_blocks_bridging15testGlobalBlockyyF
// CHECK: global_addr @GlobalBlock : $*@convention(block) (NSString) -> @autoreleased Optional<NSString>
// CHECK: function_ref @_T0So8NSStringCABSgIeyBya_S2SIegxo_TR : $@convention(thin) (@owned String, @guaranteed @convention(block) (NSString) -> @autoreleased Optional<NSString>) -> @owned String
