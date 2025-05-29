
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name pack_iteration %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////
@_silgen_name("loopBodyEnd")
func loopBodyEnd() -> ()

@_silgen_name("funcEnd")
func funcEnd() -> ()

@_silgen_name("condition")
func condition() -> Bool

@_silgen_name("loopContinueEnd")
func loopContinueEnd() -> ()

@_silgen_name("loopBreakEnd")
func loopBreakEnd() -> ()

enum E<T> {
  case one(T)
  case two
}

//////////////
// Tests //
///////////

// CHECK-LABEL: sil hidden [ossa] @$s14pack_iteration14iterateTrivial4overyxxQp_tRvzlF : $@convention(thin) <each Element> (@pack_guaranteed Pack{repeat each Element}) -> () {
// CHECK: bb0([[PACK:%.*]] : $*Pack{repeat each Element}):
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IDX2:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[IDX1]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX3:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX3]] : $Builtin.Word, [[PACK_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[NONE_BB:bb[0-9]+]], [[SOME_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK:  [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK: apply [[FUNC_END_FUNC]]() : $@convention(thin) () -> ()
//
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat each Element}
// CHECK: open_pack_element [[DYN_PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "el"
// CHECK: [[PACK_ELT_GET:%.*]] = pack_element_get [[DYN_PACK_IDX]] of [[PACK]] : $*Pack{repeat each Element} as $*@pack_element("[[UUID]]") each Element
// CHECK: copy_addr [[PACK_ELT_GET]] to [init] [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[LOOP_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[IDX4:%.*]] = builtin "add_Word"([[IDX3]] : $Builtin.Word, [[IDX2]] : $Builtin.Word) : $Builtin.Word
// CHECK: br [[LOOP_DEST]]([[IDX4]] : $Builtin.Word)
//
// CHECK: } // end sil function '$s14pack_iteration14iterateTrivial4overyxxQp_tRvzlF'
func iterateTrivial<each Element>(over element: repeat each Element) {
  for el in repeat each element {
    loopBodyEnd()
  }
  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s14pack_iteration11equalTuples3lhs3rhsSbxxQp_t_xxQp_ttRvzSQRzlF : $@convention(thin) <each Element where repeat each Element : Equatable> (@pack_guaranteed Pack{repeat each Element}, @pack_guaranteed Pack{repeat each Element}) -> Bool {
// CHECK: bb6:
// CHECK: [[STACK1:%.*]] = alloc_stack $(repeat each Element)
// CHECK: [[STACK2:%.*]] = alloc_stack $(repeat each Element)
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IDX2:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[IDX1]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX3:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX3]] : $Builtin.Word, [[PACK_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[NONE_BB:bb[0-9]+]], [[SOME_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat (each Element, each Element)}
// CHECK: [[OPEN_PACK_ELT:%.*]] = open_pack_element [[DYN_PACK_IDX]] of <each Element where repeat each Element : Equatable> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK_LEFT:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "left"
// CHECK: [[STACK_RIGHT:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "right"
// CHECK: tuple_pack_element_addr [[DYN_PACK_IDX]] of [[STACK1]] : $*(repeat each Element) as $*@pack_element("[[UUID]]") each Element
// CHECK: tuple_pack_element_addr [[DYN_PACK_IDX]] of [[STACK2]] : $*(repeat each Element) as $*@pack_element("[[UUID]]") each Element
// CHECK: [[METATYPE:%.*]] = metatype $@thick (@pack_element("[[UUID]]") each Element).Type
// CHECK: [[WITNESS_METHOD:%.*]] = witness_method $@pack_element("[[UUID]]") each Element, #Equatable."==" : <Self where Self : Equatable> (Self.Type) -> (Self, Self) -> Bool, [[OPEN_PACK_ELT]] : $Builtin.SILToken : $@convention(witness_method: Equatable) <τ_0_0 where τ_0_0 : Equatable> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> Bool
// CHECK: apply [[WITNESS_METHOD]]<@pack_element("[[UUID]]") each Element>([[STACK_LEFT]], [[STACK_RIGHT]], [[METATYPE]]) : $@convention(witness_method: Equatable) <τ_0_0 where τ_0_0 : Equatable> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> Bool
//
// CHECK: } // end sil function '$s14pack_iteration11equalTuples3lhs3rhsSbxxQp_t_xxQp_ttRvzSQRzlF'
func equalTuples<each Element: Equatable>(lhs: (repeat each Element), rhs: (repeat each Element)) -> Bool {
  
  for (left, right) in repeat (each lhs, each rhs) {
    guard left == right else { return false }
  }
  
  return true
}

// CHECK-LABEL: sil hidden [ossa] @$s14pack_iteration19iteratePatternMatch4overyAA1EOyxGxQp_tRvzlF : $@convention(thin) <each Element> (@pack_guaranteed Pack{repeat E<each Element>}) -> () {
// CHECK: bb0([[PACK:%.*]] : $*Pack{repeat E<each Element>}):
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IDX2:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[IDX1]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX3:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX3]] : $Builtin.Word, [[PACK_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[NONE_BB:bb[0-9]+]], [[SOME_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat E<each Element>}
// CHECK: open_pack_element [[DYN_PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "value"
// CHECK: [[PACK_ELT_GET:%.*]] = pack_element_get [[DYN_PACK_IDX]] of [[PACK]] : $*Pack{repeat E<each Element>} as $*E<@pack_element("[[UUID]]") each Element>
// CHECK: [[ENUM_STACK:%.*]] = alloc_stack $E<@pack_element("[[UUID]]") each Element>
// CHECK: copy_addr [[PACK_ELT_GET]] to [init] [[ENUM_STACK]] : $*E<@pack_element("[[UUID]]") each Element>
// CHECK: switch_enum_addr [[ENUM_STACK]] : $*E<@pack_element("[[UUID]]") each Element>, case #E.one!enumelt: [[ENUM_MATCH_BB:bb[0-9]+]], case #E.two!enumelt: [[CONTINUE_BB:bb[0-9]+]]
//
// CHECK: [[CONTINUE_BB]]:
// CHECK: destroy_addr [[ENUM_STACK]]
// CHECK: dealloc_stack [[ENUM_STACK]]
// CHECK: dealloc_stack [[STACK]]
// CHECK:  br [[LATCH_BB:bb[0-9]+]]
//
// CHECK: [[ENUM_MATCH_BB]]:
// CHECK: [[ENUM_DATA_ADDR:%.*]] = unchecked_take_enum_data_addr %13 : $*E<@pack_element("[[UUID]]") each Element>, #E.one!enumelt
// CHECK: copy_addr [take] [[ENUM_DATA_ADDR]] to [init] [[STACK]]
// CHECK: [[LOOP_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: dealloc_stack [[ENUM_STACK]]
// CHECK: destroy_addr [[STACK]]
// CHECK: dealloc_stack [[STACK]]
// CHECK:  br [[LATCH:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK: [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK: apply [[FUNC_END_FUNC]]() : $@convention(thin) () -> ()
//
// CHECK: [[LATCH_BB]]:
// CHECK: [[ADD_WORD:%.*]] = builtin "add_Word"([[IDX3]] : $Builtin.Word, [[IDX2]] : $Builtin.Word) : $Builtin.Word
// CHECK: br [[LOOP_DEST]]([[ADD_WORD]] : $Builtin.Word)
// CHECK: } // end sil function '$s14pack_iteration19iteratePatternMatch4overyAA1EOyxGxQp_tRvzlF'
func iteratePatternMatch<each Element>(over element: repeat E<each Element>) {
  for case .one(let value) in repeat each element {
    loopBodyEnd()
  }
  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s14pack_iteration19iterateTrivialBreak4overyxxQp_tRvzlF : $@convention(thin) <each Element> (@pack_guaranteed Pack{repeat each Element}) -> () {
// CHECK: bb0([[PACK:%.*]] : $*Pack{repeat each Element}):
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IDX2:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[IDX1]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX3:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX3]] : $Builtin.Word, [[PACK_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[NONE_BB:bb[0-9]+]], [[SOME_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat each Element}
// CHECK: open_pack_element [[DYN_PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "el"
// CHECK: [[PACK_ELT_GET:%.*]] = pack_element_get [[DYN_PACK_IDX]] of [[PACK]] : $*Pack{repeat each Element} as $*@pack_element("[[UUID]]") each Element
// CHECK: copy_addr [[PACK_ELT_GET]] to [init] [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[LOOP_END_FUNC:%.*]] = function_ref @loopBreakEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: br [[FUNC_END_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK: br [[FUNC_END_BB]]
//
// CHECK: [[FUNC_END_BB]]
// CHECK: [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK: apply [[FUNC_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: } // end sil function '$s14pack_iteration19iterateTrivialBreak4overyxxQp_tRvzlF'
func iterateTrivialBreak<each Element>(over element: repeat each Element) {
  for el in repeat each element {
    loopBreakEnd()
    break
  }
  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s14pack_iteration20iterateContinueBreak4overyxxQp_tRvzlF : $@convention(thin) <each Element> (@pack_guaranteed Pack{repeat each Element}) -> () {
// CHECK: bb0([[PACK:%.*]] : $*Pack{repeat each Element}):
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IDX2:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[IDX1]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX3:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX3]] : $Builtin.Word, [[PACK_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[NONE_BB:bb[0-9]+]], [[SOME_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat each Element}
// CHECK: open_pack_element [[DYN_PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "el"
// CHECK: [[PACK_ELT_GET:%.*]] = pack_element_get [[DYN_PACK_IDX]] of [[PACK]] : $*Pack{repeat each Element} as $*@pack_element("[[UUID]]") each Element
// CHECK: copy_addr [[PACK_ELT_GET]] to [init] [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[COND_FUNC:%.*]] = function_ref @condition : $@convention(thin) () -> Bool
// CHECK: [[BOOL:%.*]] = apply [[COND_FUNC]]() : $@convention(thin) () -> Bool
// CHECK: [[IF:%.*]] = struct_extract [[BOOL]] : $Bool, #Bool._value
// CHECK: cond_br [[IF]], [[LOOP_BREAK:bb[0-9]+]], [[LOOP_CONDITION:bb[0-9]+]]
//
// CHECK: [[LOOP_BREAK]]:
// CHECK: [[LOOP_BREAK_FUNC:%.*]] = function_ref @loopBreakEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_BREAK_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// br [[FUNC_END:bb[0-9]+]]
//
// CHECK: [[LOOP_CONDITION]]:
// CHECK: [[LOOP_CONDITION_FUNC:%.*]] = function_ref @condition : $@convention(thin) () -> Bool
// CHECK: [[BOOL:%.*]] = apply [[LOOP_CONDITION_FUNC]]() : $@convention(thin) () -> Bool
// CHECK: [[IF:%.*]] = struct_extract [[BOOL]] : $Bool, #Bool._value
// CHECK: cond_br [[IF]], [[LOOP_CONTINUE:bb[0-9]+]], [[LOOP_BODY_END:bb[0-9]+]]
//
// CHECK: [[LOOP_CONTINUE]]:
// CHECK: [[LOOP_CONTINUE_FUNC:%.*]] = function_ref @loopContinueEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_CONTINUE_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: br [[LATCH:bb[0-9]+]]
//
// CHECK: [[LOOP_BODY_END]]:
// CHECK: [[LOOP_BODY_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_BODY_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: br [[LATCH]]
//
// CHECK: [[NONE_BB]]:
// CHECK: br [[FUNC_END_BB:bb[0-9]+]]
//
// CHECK: [[FUNC_END_BB]]
// CHECK: [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK: apply [[FUNC_END_FUNC]]() : $@convention(thin) () -> ()
//
// CHECK: [[LATCH]]:
// CHECK: [[ADD_WORD:%.*]] = builtin "add_Word"([[IDX3]] : $Builtin.Word, [[IDX2]] : $Builtin.Word) : $Builtin.Word
// CHECK: br [[LOOP_DEST]]([[ADD_WORD]] : $Builtin.Word)
// CHECK: } // end sil function '$s14pack_iteration20iterateContinueBreak4overyxxQp_tRvzlF'
func iterateContinueBreak<each Element>(over element: repeat each Element) {
  for el in repeat each element {
    if (condition()) {
      loopBreakEnd()
      break
    }

    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s14pack_iteration14iterateClosure4overyxxQp_tRvzlF : $@convention(thin) <each Element> (@pack_guaranteed Pack{repeat each Element}) -> () {
//
// CHECK-LABEL: sil private [ossa] @$s14pack_iteration14iterateClosure4overyxxQp_tRvzlFyycfU_ : $@convention(thin) <each Element> (@in_guaranteed (repeat each Element)) -> () {
//
// CHECK: bb0([[PACK:%.*]] :  @closureCapture $*(repeat each Element)):
// CHECK: [[ALLOC_PACK:%.*]] = alloc_pack $Pack{repeat each Element}
// CHECK: [[IDX1:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[IDX2:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[PACK_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[IDX1]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX3:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX3]] : $Builtin.Word, [[PACK_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[SETUP_BB:bb[0-9]+]], [[ITER_BB:bb[0-9]+]]
//
// CHECK: [[ITER_BB]]:
// CHECK: [[PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat each Element}
// CHECK: [[OPEN_ELT:%.*]] = open_pack_element [[PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[TUPLE_ADDR:%.*]] = tuple_pack_element_addr [[PACK_IDX]] of [[PACK]] : $*(repeat each Element) as $*@pack_element("[[UUID]]") each Element
// CHECK: pack_element_set [[TUPLE_ADDR]] : $*@pack_element("[[UUID]]") each Element into [[PACK_IDX]] of [[ALLOC_PACK]] : $*Pack{repeat each Element}
// CHECK: [[ADD_WORD:%.*]] = builtin "add_Word"([[IDX3]] : $Builtin.Word, [[IDX2]] : $Builtin.Word) : $Builtin.Word
// CHECK: br [[LOOP_DEST]]([[ADD_WORD]] : $Builtin.Word)
//
// CHECK: [[SETUP_BB]]
// CHECK: debug_value [[ALLOC_PACK]] : $*Pack{repeat each Element}, let, name "element", argno 1, expr op_deref
// CHECK: [[SETUP_0:%.*]] = integer_literal $Builtin.Word, 0
// CHECK: [[SETUP_1:%.*]] = integer_literal $Builtin.Word, 1
// CHECK: [[SETUP_LENGTH:%.*]] = pack_length $Pack{repeat each Element}
// CHECK: br [[LOOP_DEST:bb[0-9]+]]([[SETUP_0]] : $Builtin.Word)
//
// CHECK: [[LOOP_DEST]]([[IDX4:%.*]] : $Builtin.Word):
// CHECK: [[COND:%.*]] = builtin "cmp_eq_Word"([[IDX4]] : $Builtin.Word, [[SETUP_LENGTH]] : $Builtin.Word) : $Builtin.Int1
// CHECK: cond_br [[COND]], [[NONE_BB:bb[0-9]+]], [[SOME_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX4]] of $Pack{repeat each Element}
// CHECK: open_pack_element [[DYN_PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK:%.*]] = alloc_stack [lexical] [var_decl] $@pack_element("[[UUID]]") each Element, let, name "el"
// CHECK: [[PACK_ELT_GET:%.*]] = pack_element_get [[DYN_PACK_IDX]] of [[ALLOC_PACK]] : $*Pack{repeat each Element} as $*@pack_element("[[UUID]]") each Element
// CHECK: copy_addr [[PACK_ELT_GET]] to [init] [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[LOOP_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: br [[LATCH_BB:bb[0-9]+]]
//
// CHECK: [[NONE_BB]]:
// CHECK: dealloc_pack [[ALLOC_PACK]] : $*Pack{repeat each Element}
//
// CHECK: [[LATCH_BB]]:
// CHECK: [[ADD_WORD:%.*]] = builtin "add_Word"([[IDX4]] : $Builtin.Word, [[SETUP_1]] : $Builtin.Word) : $Builtin.Word
// CHECK: br [[LOOP_DEST]]([[ADD_WORD]] : $Builtin.Word)
func iterateClosure<each Element>(over element: repeat each Element) {
  let _ = { () -> Void in
    for el in repeat each element {
      loopBodyEnd()
    }
  }
}
