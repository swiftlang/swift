// RUN: %target-swift-frontend -parse-as-library -emit-silgen -Xllvm -aarch64-use-tbi %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | %FileCheck -check-prefix=NO-TBI %s

// REQUIRES: concurrency
// REQUIRES: CODEGENERATOR=AArch64
// REQUIRES: PTRSIZE=64
// REQUIRES: OS=macosx || OS=ios
// REQUIRES: CPU=arm64


func useActor(iso: (any Actor)?) {}
func implicitParam(_ x: (any Actor)? = #isolation) {}

// CHECK-LABEL: sil hidden [ossa] @$s39isolated_nonsending_isolation_macro_sil46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
//
// #isolation without default arg
// CHECK:   hop_to_executor [[ACTOR]]
// CHECK:   [[CAST_TUPLE:%.*]] = unchecked_trivial_bit_cast [[ACTOR]] to $(Builtin.Word, Builtin.Word)
// CHECK:   ([[POINTER:%.*]], [[WITNESS:%.*]]) = destructure_tuple [[CAST_TUPLE]]

// MASK = 0 - 1 << ((sizeof(Word) - 1) << 3)
// CHECK:   [[WORD_SIZE:%.*]] = builtin "sizeof"<Builtin.Word>(
// CHECK:   [[INT_ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[INT_THREE:%.*]] = integer_literal $Builtin.Word, 3
// CHECK:   [[INT_ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[WORD_MINUS_ONE:%.*]] = builtin "sub_Word"([[WORD_SIZE]], [[INT_ONE]])
// CHECK:   [[INNER_SHIFT:%.*]] = builtin "shl_Word"([[WORD_MINUS_ONE]], [[INT_THREE]])
// CHECK:   [[OUTER_SHIFT:%.*]] = builtin "shl_Word"([[INT_ONE]], [[INNER_SHIFT]])
// CHECK:   [[MASK:%.*]] = builtin "sub_Word"([[INT_ZERO]], [[OUTER_SHIFT]])

// CHECK:   [[MASKED_WITNESS:%.*]] = builtin "and_Word"([[WITNESS]], [[MASK]])
// CHECK:   [[REFORMED_TUPLE:%.*]] = tuple ([[POINTER]], [[MASKED_WITNESS]])
// CHECK:   [[CAST_BACK:%.*]] = unchecked_bitwise_cast [[REFORMED_TUPLE]] to $Optional<any Actor>
// CHECK:   [[CAST_BACK_G:%.*]] = unchecked_ownership_conversion [[CAST_BACK]], @unowned to @guaranteed
// CHECK:   [[MARK_DEP:%.*]] = mark_dependence [nonescaping] [[CAST_BACK_G]] on [[ACTOR]]
// CHECK:   [[MARK_DEP_C:%.*]] = copy_value [[MARK_DEP]]
// CHECK:   [[ISO:%.*]] = move_value [lexical] [var_decl] [[MARK_DEP_C]]
// CHECK:   debug_value [[ISO]], let, name "iso"
// CHECK:   [[ISO_B:%.*]] = begin_borrow [[ISO]]
// CHECK:   [[FUNC:%.*]] = function_ref @$s39isolated_nonsending_isolation_macro_sil8useActor3isoyScA_pSg_tF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   apply [[FUNC]]([[ISO_B]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   end_borrow [[ISO_B]]
// CHECK:   destroy_value [[ISO]]
// CHECK: } // end sil function '$s39isolated_nonsending_isolation_macro_sil46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF'

// NO-TBI-LABEL: sil hidden [ossa] @$s39isolated_nonsending_isolation_macro_sil46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// NO-TBI: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
//
// #isolation without default arg
// NO-TBI:   hop_to_executor [[ACTOR]]
// NO-TBI:   [[ACTOR_COPY:%.*]] = copy_value [[ACTOR]]
// NO-TBI:   [[ISO:%.*]] = move_value [lexical] [var_decl] [[ACTOR_COPY]]
// NO-TBI:   [[ISO_B:%.*]] = begin_borrow [[ISO]]
// NO-TBI:   [[FUNC:%.*]] = function_ref @$s39isolated_nonsending_isolation_macro_sil8useActor3isoyScA_pSg_tF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// NO-TBI:   apply [[FUNC]]([[ISO_B]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// NO-TBI: } // end sil function '$s39isolated_nonsending_isolation_macro_sil46nonisolatedNonsendingUsePoundIsolationDirectlyyyYaF'
nonisolated(nonsending) func nonisolatedNonsendingUsePoundIsolationDirectly() async {
  let iso = #isolation
  useActor(iso: iso)
}

// #isolation via default arg
//
// CHECK-LABEL: sil hidden [ossa] @$s39isolated_nonsending_isolation_macro_sil45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// CHECK: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// CHECK:   [[CAST_PTR:%.*]] = unchecked_trivial_bit_cast [[ACTOR]] to $(Builtin.Word, Builtin.Word)
// CHECK:   ([[POINTER:%.*]], [[WITNESS:%.*]]) = destructure_tuple [[CAST_PTR]]
// MASK = 0 - 1 << ((sizeof(Word) - 1) << 3)
// CHECK:   [[WORD_SIZE:%.*]] = builtin "sizeof"<Builtin.Word>(
// CHECK:   [[INT_ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[INT_THREE:%.*]] = integer_literal $Builtin.Word, 3
// CHECK:   [[INT_ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[WORD_MINUS_ONE:%.*]] = builtin "sub_Word"([[WORD_SIZE]], [[INT_ONE]])
// CHECK:   [[INNER_SHIFT:%.*]] = builtin "shl_Word"([[WORD_MINUS_ONE]], [[INT_THREE]])
// CHECK:   [[OUTER_SHIFT:%.*]] = builtin "shl_Word"([[INT_ONE]], [[INNER_SHIFT]])
// CHECK:   [[MASK:%.*]] = builtin "sub_Word"([[INT_ZERO]], [[OUTER_SHIFT]])
// CHECK:   [[MASKED_WITNESS:%.*]] = builtin "and_Word"([[WITNESS]], [[MASK]])
// CHECK:   [[REFORMED_TUPLE:%.*]] = tuple ([[POINTER]], [[MASKED_WITNESS]])
// CHECK:   [[CAST_BACK:%.*]] = unchecked_bitwise_cast [[REFORMED_TUPLE]] to $Optional<any Actor>
// CHECK:   [[CAST_BACK_G:%.*]] = unchecked_ownership_conversion [[CAST_BACK]], @unowned to @guaranteed
// CHECK:   [[MARK_DEP:%.*]] = mark_dependence [nonescaping] [[CAST_BACK_G]] on [[ACTOR]]
// CHECK:   [[FUNC:%.*]] = function_ref @$s39isolated_nonsending_isolation_macro_sil13implicitParamyyScA_pSgF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK:   apply [[FUNC]]([[MARK_DEP]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// CHECK: } // end sil function '$s39isolated_nonsending_isolation_macro_sil45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF'
//
// NO-TBI-LABEL: sil hidden [ossa] @$s39isolated_nonsending_isolation_macro_sil45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF : $@convention(thin) @async (@sil_isolated @sil_implicit_leading_param @guaranteed Optional<any Actor>) -> () {
// NO-TBI: bb0([[ACTOR:%.*]] : @guaranteed $Optional<any Actor>):
// NO-TBI:   [[FUNC:%.*]] = function_ref @$s39isolated_nonsending_isolation_macro_sil13implicitParamyyScA_pSgF : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// NO-TBI:   apply [[FUNC]]([[ACTOR]]) : $@convention(thin) (@guaranteed Optional<any Actor>) -> ()
// NO-TBI: } // end sil function '$s39isolated_nonsending_isolation_macro_sil45nonisolatedNonsendingPoundIsolationDefaultArgyyYaF'
nonisolated(nonsending) func nonisolatedNonsendingPoundIsolationDefaultArg() async {
  implicitParam()
}
