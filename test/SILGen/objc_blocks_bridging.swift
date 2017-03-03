// RUN: rm -rf %t && mkdir -p %t
// RUN: %build-silgen-test-overlays
// RUN: %target-swift-frontend(mock-sdk: -sdk %S/Inputs -I %t) -Xllvm -new-mangling-for-tests -verify -emit-silgen -I %S/Inputs -disable-objc-attr-requires-foundation-module %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation

@objc class Foo {
// CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC3fooS3ic_Si1xtFTo :
  // CHECK: bb0([[ARG1:%.*]] : $@convention(block) (Int) -> Int, {{.*}}, [[SELF:%.*]] : $Foo):
  // CHECK:         [[ARG1_COPY:%.*]] = copy_block [[ARG1]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @_T0S2iIyByd_S2iIxyd_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[ARG1_COPY]])
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC3foo{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned @callee_owned (Int) -> Int, Int, @guaranteed Foo) -> Int
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK: } // end sil function '_T020objc_blocks_bridging3FooC3fooS3ic_Si1xtFTo'
  dynamic func foo(_ f: (Int) -> Int, x: Int) -> Int {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC3barS3Sc_SS1xtFTo : $@convention(objc_method) (@convention(block) (NSString) -> @autoreleased NSString, NSString, Foo) -> @autoreleased NSString {
  // CHECK:       bb0([[BLOCK:%.*]] : $@convention(block) (NSString) -> @autoreleased NSString, [[NSSTRING:%.*]] : $NSString, [[SELF:%.*]] : $Foo):
  // CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:         [[NSSTRING_COPY:%.*]] = copy_value [[NSSTRING]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @_T0So8NSStringCABIyBya_S2SIxxo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[BLOCK_COPY]])
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC3bar{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned @callee_owned (@owned String) -> @owned String, @owned String, @guaranteed Foo) -> @owned String
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{%.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  // CHECK: } // end sil function '_T020objc_blocks_bridging3FooC3barS3Sc_SS1xtFTo'
  dynamic func bar(_ f: (String) -> String, x: String) -> String {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC3basSSSgA2Ec_AE1xtFTo : $@convention(objc_method) (@convention(block) (Optional<NSString>) -> @autoreleased Optional<NSString>, Optional<NSString>, Foo) -> @autoreleased Optional<NSString> {
  // CHECK:       bb0([[BLOCK:%.*]] : $@convention(block) (Optional<NSString>) -> @autoreleased Optional<NSString>, [[OPT_STRING:%.*]] : $Optional<NSString>, [[SELF:%.*]] : $Foo):
  // CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[BLOCK]]
  // CHECK:         [[OPT_STRING_COPY:%.*]] = copy_value [[OPT_STRING]]
  // CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:         [[THUNK:%.*]] = function_ref @_T0So8NSStringCSgACIyBya_SSSgADIxxo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[THUNK]]([[BLOCK_COPY]])
  // CHECK:         [[BORROWED_SELF_COPY:%.*]] = begin_borrow [[SELF_COPY]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC3bas{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned @callee_owned (@owned Optional<String>) -> @owned Optional<String>, @owned Optional<String>, @guaranteed Foo) -> @owned Optional<String>
  // CHECK:         apply [[NATIVE]]([[BRIDGED]], {{%.*}}, [[BORROWED_SELF_COPY]])
  // CHECK:         end_borrow [[BORROWED_SELF_COPY]] from [[SELF_COPY]]
  dynamic func bas(_ f: (String?) -> String?, x: String?) -> String? {
    return f(x)
  }

  // CHECK-LABEL: sil hidden [thunk] @_T020objc_blocks_bridging3FooC16cFunctionPointer{{[_0-9a-zA-Z]*}}FTo
  // CHECK:       bb0([[F:%.*]] : $@convention(c) (Int) -> Int, [[X:%.*]] : $Int, [[SELF:%.*]] : $Foo):
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
  // CHECK:         [[COPY:%.*]] = copy_block %0
  // CHECK:         [[BLOCK:%.*]] = unchecked_enum_data [[COPY]]
  // TODO: redundant reabstractions here
  // CHECK:         [[BLOCK_THUNK:%.*]] = function_ref @_T0So8NSStringCABIyBya_S2SIxxo_TR
  // CHECK:         [[BRIDGED:%.*]] = partial_apply [[BLOCK_THUNK]]([[BLOCK]])
  // CHECK:         enum $Optional<@callee_owned (@owned String) -> @owned String>, #Optional.some!enumelt.1, [[BRIDGED]]
  // CHECK:         [[NATIVE:%.*]] = function_ref @_T020objc_blocks_bridging3FooC7optFunc{{[_0-9a-zA-Z]*}}F : $@convention(method) (@owned Optional<@callee_owned (@owned String) -> @owned String>, @owned String, @guaranteed Foo) -> @owned Optional<String>
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
  // CHECK: bb0([[ARG0:%.*]] : $Foo, [[ARG1:%.*]] : $@callee_owned (Int) -> Int, [[ARG2:%.*]] : $@callee_owned (@owned String) -> @owned String, [[ARG3:%.*]] : $@callee_owned (@owned Optional<String>) -> @owned Optional<String>):
  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[FOO:%.*]] =  class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.foo!1.foreign
  // CHECK: [[BORROWED_ARG1:%.*]] = begin_borrow [[ARG1]]
  // CHECK: [[CLOSURE_COPY:%.*]] = copy_value [[BORROWED_ARG1]]
  // CHECK: [[F_BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
  // CHECK: [[F_BLOCK_CAPTURE:%.*]] = project_block_storage [[F_BLOCK_STORAGE]]
  // CHECK: store [[CLOSURE_COPY]] to [init] [[F_BLOCK_CAPTURE]]
  // CHECK: [[F_BLOCK_INVOKE:%.*]] = function_ref @_T0S2iIxyd_S2iIyByd_TR
  // CHECK: [[F_STACK_BLOCK:%.*]] = init_block_storage_header [[F_BLOCK_STORAGE]] : {{.*}}, invoke [[F_BLOCK_INVOKE]]
  // CHECK: [[F_BLOCK:%.*]] = copy_block [[F_STACK_BLOCK]]
  // CHECK: apply [[FOO]]([[F_BLOCK]]

  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[BAR:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.bar!1.foreign
  // CHECK: [[G_BLOCK_INVOKE:%.*]] = function_ref @_T0S2SIxxo_So8NSStringCABIyBya_TR
  // CHECK: [[G_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[G_BLOCK_INVOKE]]
  // CHECK: [[G_BLOCK:%.*]] = copy_block [[G_STACK_BLOCK]]
  // CHECK: apply [[BAR]]([[G_BLOCK]]

  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: [[BAS:%.*]] = class_method [volatile] [[BORROWED_ARG0]] : $Foo, #Foo.bas!1.foreign
  // CHECK: [[H_BLOCK_INVOKE:%.*]] = function_ref @_T0SSSgAAIxxo_So8NSStringCSgADIyBya_TR
  // CHECK: [[H_STACK_BLOCK:%.*]] = init_block_storage_header {{.*}}, invoke [[H_BLOCK_INVOKE]]
  // CHECK: [[H_BLOCK:%.*]] = copy_block [[H_STACK_BLOCK]]
  // CHECK: apply [[BAS]]([[H_BLOCK]]

  // CHECK: [[G_BLOCK:%.*]] = copy_block {{%.*}} : $@convention(block) (NSString) -> @autoreleased NSString
  // CHECK: enum $Optional<@convention(block) (NSString) -> @autoreleased NSString>, #Optional.some!enumelt.1, [[G_BLOCK]]

  return (x.foo(f, x: 0), x.bar(g, x: "one"), x.bas(h, x: "two"), x.optFunc(g, x: "three"))
}

class Test: NSObject {
  func blockTakesBlock() -> ((Int) -> Int) -> Int {}
}

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0S2iIxyd_SiIxxd_S2iIyByd_SiIyByd_TR
// CHECK:         [[BLOCK_COPY:%.*]] = copy_block [[ORIG_BLOCK:%.*]] :
// CHECK:         [[CLOSURE:%.*]] = partial_apply {{%.*}}([[BLOCK_COPY]])
// CHECK:         [[RESULT:%.*]] = apply {{%.*}}([[CLOSURE]])
// CHECK:         return [[RESULT]]

func clearDraggingItemImageComponentsProvider(_ x: NSDraggingItem) {
  x.imageComponentsProvider = {}
}
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SayypGIxo_So7NSArrayCSgIyBa_TR
// CHECK:         [[CONVERT:%.*]] = function_ref @_T0Sa10FoundationE19_bridgeToObjectiveCSo7NSArrayCyF
// CHECK:         [[CONVERTED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL:%.*]] = enum $Optional<NSArray>, #Optional.some!enumelt.1, [[CONVERTED]]
// CHECK:         return [[OPTIONAL]]

// CHECK-LABEL: sil hidden @{{.*}}bridgeNonnullBlockResult{{.*}}
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0SSIxo_So8NSStringCSgIyBa_TR
// CHECK:         [[CONVERT:%.*]] = function_ref @_T0SS10FoundationE19_bridgeToObjectiveCSo8NSStringCyF
// CHECK:         [[BRIDGED:%.*]] = apply [[CONVERT]]
// CHECK:         [[OPTIONAL_BRIDGED:%.*]] = enum $Optional<NSString>, #Optional.some!enumelt.1, [[BRIDGED]]
// CHECK:         return [[OPTIONAL_BRIDGED]]
func bridgeNonnullBlockResult() {
  nonnullStringBlockResult { return "test" }
}

// CHECK-LABEL: sil hidden @{{.*}}bridgeNoescapeBlock{{.*}}
func bridgeNoescapeBlock() {
  // CHECK: function_ref @_T0Ix_IyB_TR
  noescapeBlockAlias { }
  // CHECK: function_ref @_T0Ix_IyB_TR
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

// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0Ix_Ixx_IyB_IyBy_TR : $@convention(c) (@inout_aliasable @block_storage @callee_owned (@owned @callee_owned () -> ()) -> (), @convention(block) () -> ()) -> ()
// CHECK-LABEL: sil shared [transparent] [reabstraction_thunk] @_T0IyB_Ix_TR : $@convention(thin) (@owned @convention(block) () -> ()) -> ()

