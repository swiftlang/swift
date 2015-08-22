// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// Check SILGen against various FunctionConversionExprs emitted by Sema.

// CHECK-LABEL: sil hidden @_TF19function_conversion7cToFuncFcSiSiFSiSi : $@convention(thin) (@convention(c) (Int) -> Int) -> @owned @callee_owned (Int) -> Int
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFtCc_dSi_dSi_XFo_dSi_dSi_
// CHECK:         [[FUNC:%.*]] = partial_apply [[THUNK]](%0)
// CHECK:         return [[FUNC]]
func cToFunc(arg: @convention(c) Int -> Int) -> Int -> Int {
  return arg
}

// CHECK-LABEL: sil hidden @_TF19function_conversion8cToBlockFcSiSibSiSi : $@convention(thin) (@convention(c) (Int) -> Int) -> @owned @convention(block) (Int) -> Int
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) (Int) -> Int
// CHECK:         return [[COPY]]
func cToBlock(arg: @convention(c) Int -> Int) -> @convention(block) Int -> Int {
  return arg
}

// CHECK-LABEL: sil hidden @_TF19function_conversion11funcToBlockFFT_T_bT_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> @owned @convention(block) () -> ()
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) () -> ()
// CHECK:         return [[COPY]]
func funcToBlock(x: () -> ()) -> @convention(block) () -> () {
  return x
}

// CHECK-LABEL: sil hidden @_TF19function_conversion11blockToFuncFbT_T_FT_T_ : $@convention(thin) (@owned @convention(block) () -> ()) -> @owned @callee_owned () -> ()
// CHECK:         [[COPIED:%.*]] = copy_block %0
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb__dT__XFo__dT__
// CHECK:         [[FUNC:%.*]] = partial_apply [[THUNK]]([[COPIED]])
// CHECK:         return [[FUNC]]
func blockToFunc(x: @convention(block) () -> ()) -> () -> () {
  return x
}

// CHECK-LABEL: sil hidden @_TF19function_conversion12funcToThrowsFFT_T_FzT_T_ : $@convention(thin) (@owned @callee_owned () -> ()) -> @owned @callee_owned () -> @error ErrorType
// CHECK:         [[FUNC:%.*]] = convert_function %0 : $@callee_owned () -> () to $@callee_owned () -> @error ErrorType
// CHECK:         return [[FUNC]]
func funcToThrows(x: () -> ()) -> () throws -> () {
  return x
}

// CHECK-LABEL: sil hidden @_TF19function_conversion12thinToThrowsFXfT_T_XfzT_T_ : $@convention(thin) (@convention(thin) () -> ()) -> @convention(thin) () -> @error ErrorType
// CHECK:         [[FUNC:%.*]] = convert_function %0 : $@convention(thin) () -> () to $@convention(thin) () -> @error ErrorType
// CHECK:         return [[FUNC]] : $@convention(thin) () -> @error ErrorType
func thinToThrows(x: @convention(thin) () -> ()) -> @convention(thin) () throws -> () {
  return x
}

// FIXME: triggers an assert because we always do a thin to thick conversion on DeclRefExprs
/*
func thinFunc() {}

func thinToThrows() {
  let _: @convention(thin) () -> () = thinFunc
}
*/

class Feral {}
class Domesticated : Feral {}

// CHECK-LABEL: sil hidden @_TF19function_conversion12funcToUpcastFFT_CS_12DomesticatedFT_CS_5Feral : $@convention(thin) (@owned @callee_owned () -> @owned Domesticated) -> @owned @callee_owned () -> @owned Feral
// CHECK:         [[FUNC:%.*]] = convert_function %0 : $@callee_owned () -> @owned Domesticated to $@callee_owned () -> @owned Feral
// CHECK:         return [[FUNC]]
func funcToUpcast(x: () -> Domesticated) -> () -> Feral {
  return x
}

// CHECK-LABEL: sil hidden @_TF19function_conversion12funcToUpcastFFCS_5FeralT_FCS_12DomesticatedT_ : $@convention(thin) (@owned @callee_owned (@owned Feral) -> ()) -> @owned @callee_owned (@owned Domesticated) -> ()
// CHECK:         [[FUNC:%.*]] = convert_function %0 : $@callee_owned (@owned Feral) -> () to $@callee_owned (@owned Domesticated) -> () // user: %3
// CHECK:         return [[FUNC]]
func funcToUpcast(x: Feral -> ()) -> Domesticated -> () {
  return x
}
