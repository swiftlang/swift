
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name foreach %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////

class C {}

@_silgen_name("loopBodyEnd")
func loopBodyEnd() -> ()

@_silgen_name("condition")
func condition() -> Bool

@_silgen_name("loopContinueEnd")
func loopContinueEnd() -> ()

@_silgen_name("loopBreakEnd")
func loopBreakEnd() -> ()

@_silgen_name("funcEnd")
func funcEnd() -> ()

struct TrivialStruct {
  var value: Int32
}

struct NonTrivialStruct {
  var value: C
}

struct GenericStruct<T> {
  var value: T
  var value2: C
}

protocol P {}
protocol ClassP : class {}

protocol GenericCollection : Collection {

}

///////////
// Tests //
///////////

//===----------------------------------------------------------------------===//
// Trivial Struct
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil hidden [ossa] @$s7foreach13trivialStructyySaySiGF : $@convention(thin) (@guaranteed Array<Int>) -> () {
// CHECK: bb0([[ARRAY:%.*]] : @guaranteed $Array<Int>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box ${ var IndexingIterator<Array<Int>> }, var, name "$x$generator"
// CHECK:   [[ITERATOR_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]
//
// CHECK: [[LOOP_DEST]]:
// CHECK:   switch_enum [[IND_VAR:%.*]] : $Optional<Int>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[VAR:%.*]] : $Int):
// CHECK:   [[LOOP_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_END_FUNC]]()
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   destroy_value [[ITERATOR_BOX]]
// CHECK:   [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK:   apply [[FUNC_END_FUNC]]()
// CHECK: } // end sil function '$s7foreach13trivialStructyySaySiGF'
func trivialStruct(_ xx: [Int]) {
  for x in xx {
    loopBodyEnd()
  }
  funcEnd()
}

// TODO: Write this test
func trivialStructContinue(_ xx: [Int]) {
  for x in xx {
    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

// TODO: Write this test
func trivialStructBreak(_ xx: [Int]) {
  for x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }
    loopBodyEnd()
  }

  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s7foreach26trivialStructContinueBreakyySaySiGF : $@convention(thin) (@guaranteed Array<Int>) -> () {
// CHECK: bb0([[ARRAY:%.*]] : @guaranteed $Array<Int>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box ${ var IndexingIterator<Array<Int>> }, var, name "$x$generator"
// CHECK:   [[ITERATOR_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   [[BORROWED_ARRAY_STACK:%.*]] = alloc_stack $Array<Int>
// CHECK:   store [[ARRAY_COPY:%.*]] to [init] [[BORROWED_ARRAY_STACK]]
// CHECK:   [[MAKE_ITERATOR_FUNC:%.*]] = function_ref @$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
// CHECK:   apply [[MAKE_ITERATOR_FUNC]]<[Int]>([[PROJECT_ITERATOR_BOX]], [[BORROWED_ARRAY_STACK]])
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]
//
// CHECK: [[LOOP_DEST]]:
// CHECK:   [[GET_ELT_STACK:%.*]] = alloc_stack $Optional<Int>
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PROJECT_ITERATOR_BOX]] : $*IndexingIterator<Array<Int>>
// CHECK:   [[FUNC_REF:%.*]] = function_ref @$ss16IndexingIteratorV4next7ElementQzSgyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@inout IndexingIterator<τ_0_0>) -> @out Optional<τ_0_0.Element>
// CHECK:   apply [[FUNC_REF]]<[Int]>([[GET_ELT_STACK]], [[WRITE]])
// CHECK:   [[IND_VAR:%.*]] = load [trivial] [[GET_ELT_STACK]]
// CHECK:   switch_enum [[IND_VAR]] : $Optional<Int>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[VAR:%.*]] : $Int):
// CHECK:   cond_br {{%.*}}, [[LOOP_BREAK_END_BLOCK:bb[0-9]+]], [[CONTINUE_CHECK_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_BREAK_END_BLOCK]]:
// CHECK:   [[LOOP_BREAK_FUNC:%.*]] = function_ref @loopBreakEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BREAK_FUNC]]()
// CHECK:   br [[CONT_BLOCK:bb[0-9]+]]
//
// CHECK: [[CONTINUE_CHECK_BLOCK]]:
// CHECK:   cond_br {{%.*}}, [[LOOP_CONTINUE_END:bb[0-9]+]], [[LOOP_END_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_CONTINUE_END]]:
// CHECK:   [[LOOP_CONTINUE_FUNC:%.*]] = function_ref @loopContinueEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_CONTINUE_FUNC]]() : $@convention(thin) () -> ()
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[LOOP_END_BLOCK]]:
// CHECK:   [[LOOP_BODY_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BODY_FUNC]]()
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   br [[CONT_BLOCK]]
//
// CHECK: [[CONT_BLOCK]]
// CHECK:   destroy_value [[ITERATOR_BOX]] : ${ var IndexingIterator<Array<Int>> }
// CHECK:   [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK:   apply [[FUNC_END_FUNC]]()
// CHECK: } // end sil function '$s7foreach26trivialStructContinueBreakyySaySiGF'
func trivialStructContinueBreak(_ xx: [Int]) {
  for x in xx {
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


//===----------------------------------------------------------------------===//
// Existential
//===----------------------------------------------------------------------===//

func existential(_ xx: [P]) {
  for x in xx {
    loopBodyEnd()
  }
  funcEnd()
}

func existentialContinue(_ xx: [P]) {
  for x in xx {
    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

func existentialBreak(_ xx: [P]) {
  for x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }
    loopBodyEnd()
  }

  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s7foreach24existentialContinueBreakyySayAA1P_pGF : $@convention(thin) (@guaranteed Array<any P>) -> () {
// CHECK: bb0([[ARRAY:%.*]] : @guaranteed $Array<any P>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box ${ var IndexingIterator<Array<any P>> }, var, name "$x$generator"
// CHECK:   [[ITERATOR_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   [[BORROWED_ARRAY_STACK:%.*]] = alloc_stack $Array<any P>
// CHECK:   store [[ARRAY_COPY:%.*]] to [init] [[BORROWED_ARRAY_STACK]]
// CHECK:   [[MAKE_ITERATOR_FUNC:%.*]] = function_ref @$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
// CHECK:   apply [[MAKE_ITERATOR_FUNC]]<[any P]>([[PROJECT_ITERATOR_BOX]], [[BORROWED_ARRAY_STACK]])
// CHECK:   [[ELT_STACK:%.*]] = alloc_stack $Optional<any P>
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]
//
// CHECK: [[LOOP_DEST]]:
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PROJECT_ITERATOR_BOX]] : $*IndexingIterator<Array<any P>>
// CHECK:   [[FUNC_REF:%.*]] = function_ref @$ss16IndexingIteratorV4next7ElementQzSgyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@inout IndexingIterator<τ_0_0>) -> @out Optional<τ_0_0.Element>
// CHECK:   apply [[FUNC_REF]]<[any P]>([[ELT_STACK]], [[WRITE]])
// CHECK:   switch_enum_addr [[ELT_STACK]] : $*Optional<any P>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK:   [[T0:%.*]] = alloc_stack [lexical] [var_decl] $any P, let, name "x"
// CHECK:   [[ELT_STACK_TAKE:%.*]] = unchecked_take_enum_data_addr [[ELT_STACK]] : $*Optional<any P>, #Optional.some!enumelt
// CHECK:   copy_addr [take] [[ELT_STACK_TAKE]] to [init] [[T0]]
// CHECK:   cond_br {{%.*}}, [[LOOP_BREAK_END_BLOCK:bb[0-9]+]], [[CONTINUE_CHECK_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_BREAK_END_BLOCK]]:
// CHECK:   [[LOOP_BREAK_FUNC:%.*]] = function_ref @loopBreakEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BREAK_FUNC]]()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[CONT_BLOCK:bb[0-9]+]]
//
// CHECK: [[CONTINUE_CHECK_BLOCK]]:
// CHECK:   cond_br {{%.*}}, [[LOOP_CONTINUE_END:bb[0-9]+]], [[LOOP_END_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_CONTINUE_END]]:
// CHECK:   [[LOOP_CONTINUE_FUNC:%.*]] = function_ref @loopContinueEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_CONTINUE_FUNC]]() : $@convention(thin) () -> ()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[LOOP_END_BLOCK]]:
// CHECK:   [[LOOP_BODY_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BODY_FUNC]]()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   br [[CONT_BLOCK]]
//
// CHECK: [[CONT_BLOCK]]
// CHECK:   dealloc_stack [[ELT_STACK]]
// CHECK:   destroy_value [[ITERATOR_BOX]] : ${ var IndexingIterator<Array<any P>> }
// CHECK:   [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK:   apply [[FUNC_END_FUNC]]()
// CHECK: } // end sil function '$s7foreach24existentialContinueBreakyySayAA1P_pGF'
func existentialContinueBreak(_ xx: [P]) {
  for x in xx {
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

//===----------------------------------------------------------------------===//
// Class Constrained Existential
//===----------------------------------------------------------------------===//

func existentialClass(_ xx: [ClassP]) {
  for x in xx {
    loopBodyEnd()
  }
  funcEnd()
}

func existentialClassContinue(_ xx: [ClassP]) {
  for x in xx {
    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

func existentialClassBreak(_ xx: [ClassP]) {
  for x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }
    loopBodyEnd()
  }

  funcEnd()
}

func existentialClassContinueBreak(_ xx: [ClassP]) {
  for x in xx {
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

//===----------------------------------------------------------------------===//
// Generic Struct
//===----------------------------------------------------------------------===//

func genericStruct<T>(_ xx: [GenericStruct<T>]) {
  for x in xx {
    loopBodyEnd()
  }
  funcEnd()
}

func genericStructContinue<T>(_ xx: [GenericStruct<T>]) {
  for x in xx {
    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

func genericStructBreak<T>(_ xx: [GenericStruct<T>]) {
  for x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }
    loopBodyEnd()
  }

  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s7foreach26genericStructContinueBreakyySayAA07GenericC0VyxGGlF : $@convention(thin) <T> (@guaranteed Array<GenericStruct<T>>) -> () {
// CHECK: bb0([[ARRAY:%.*]] : @guaranteed $Array<GenericStruct<T>>):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box $<τ_0_0> { var IndexingIterator<Array<GenericStruct<τ_0_0>>> } <T>, var, name "$x$generator"
// CHECK:   [[ITERATOR_LIFETIME:%.*]] = begin_borrow [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   [[BORROWED_ARRAY_STACK:%.*]] = alloc_stack $Array<GenericStruct<T>>
// CHECK:   store [[ARRAY_COPY:%.*]] to [init] [[BORROWED_ARRAY_STACK]]
// CHECK:   [[MAKE_ITERATOR_FUNC:%.*]] = function_ref @$sSlss16IndexingIteratorVyxG0B0RtzrlE04makeB0ACyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection, τ_0_0.Iterator == IndexingIterator<τ_0_0>> (@in τ_0_0) -> @out IndexingIterator<τ_0_0>
// CHECK:   apply [[MAKE_ITERATOR_FUNC]]<[GenericStruct<T>]>([[PROJECT_ITERATOR_BOX]], [[BORROWED_ARRAY_STACK]])
// CHECK:   [[ELT_STACK:%.*]] = alloc_stack $Optional<GenericStruct<T>>
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]
//
// CHECK: [[LOOP_DEST]]:
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PROJECT_ITERATOR_BOX]] : $*IndexingIterator<Array<GenericStruct<T>>>
// CHECK:   [[FUNC_REF:%.*]] = function_ref @$ss16IndexingIteratorV4next7ElementQzSgyF : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@inout IndexingIterator<τ_0_0>) -> @out Optional<τ_0_0.Element>
// CHECK:   apply [[FUNC_REF]]<[GenericStruct<T>]>([[ELT_STACK]], [[WRITE]])
// CHECK:   switch_enum_addr [[ELT_STACK]] : $*Optional<GenericStruct<T>>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK:   [[T0:%.*]] = alloc_stack [lexical] [var_decl] $GenericStruct<T>, let, name "x"
// CHECK:   [[ELT_STACK_TAKE:%.*]] = unchecked_take_enum_data_addr [[ELT_STACK]] : $*Optional<GenericStruct<T>>, #Optional.some!enumelt
// CHECK:   copy_addr [take] [[ELT_STACK_TAKE]] to [init] [[T0]]
// CHECK:   cond_br {{%.*}}, [[LOOP_BREAK_END_BLOCK:bb[0-9]+]], [[CONTINUE_CHECK_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_BREAK_END_BLOCK]]:
// CHECK:   [[LOOP_BREAK_FUNC:%.*]] = function_ref @loopBreakEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BREAK_FUNC]]()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[CONT_BLOCK:bb[0-9]+]]
//
// CHECK: [[CONTINUE_CHECK_BLOCK]]:
// CHECK:   cond_br {{%.*}}, [[LOOP_CONTINUE_END:bb[0-9]+]], [[LOOP_END_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_CONTINUE_END]]:
// CHECK:   [[LOOP_CONTINUE_FUNC:%.*]] = function_ref @loopContinueEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_CONTINUE_FUNC]]() : $@convention(thin) () -> ()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[LOOP_END_BLOCK]]:
// CHECK:   [[LOOP_BODY_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BODY_FUNC]]()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   br [[CONT_BLOCK]]
//
// CHECK: [[CONT_BLOCK]]
// CHECK:   dealloc_stack [[ELT_STACK]]
// CHECK:   destroy_value [[ITERATOR_BOX]]
// CHECK:   [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK:   apply [[FUNC_END_FUNC]]()
// CHECK: } // end sil function '$s7foreach26genericStructContinueBreakyySayAA07GenericC0VyxGGlF'
func genericStructContinueBreak<T>(_ xx: [GenericStruct<T>]) {
  for x in xx {
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

//===----------------------------------------------------------------------===//
// Fully Generic Collection
//===----------------------------------------------------------------------===//

func genericCollection<T : Collection>(_ xx: T) {
  for x in xx {
    loopBodyEnd()
  }
  funcEnd()
}

func genericCollectionContinue<T : Collection>(_ xx: T) {
  for x in xx {
    if (condition()) {
      loopContinueEnd()
      continue
    }
    loopBodyEnd()
  }

  funcEnd()
}

func genericCollectionBreak<T : Collection>(_ xx: T) {
  for x in xx {
    if (condition()) {
      loopBreakEnd()
      break
    }
    loopBodyEnd()
  }

  funcEnd()
}

// CHECK-LABEL: sil hidden [ossa] @$s7foreach30genericCollectionContinueBreakyyxSlRzlF : $@convention(thin) <T where T : Collection> (@in_guaranteed T) -> () {
// CHECK: bb0([[COLLECTION:%.*]] : $*T):
// CHECK:   [[ITERATOR_BOX:%.*]] = alloc_box $<τ_0_0 where τ_0_0 : Collection> { var τ_0_0.Iterator } <T>, var, name "$x$generator"
// CHECK:   [[ITERATOR_LIFETIME:%[^,]+]] = begin_borrow [lexical] [var_decl] [[ITERATOR_BOX]]
// CHECK:   [[PROJECT_ITERATOR_BOX:%.*]] = project_box [[ITERATOR_LIFETIME]]
// CHECK:   [[MAKE_ITERATOR_FUNC:%.*]] = witness_method $T, #Sequence.makeIterator :
// CHECK:   apply [[MAKE_ITERATOR_FUNC]]<T>([[PROJECT_ITERATOR_BOX]], [[COLLECTION_COPY:%.*]])
// CHECK:   [[ELT_STACK:%.*]] = alloc_stack $Optional<T.Element>
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]
//
// CHECK: [[LOOP_DEST]]:
// CHECK:   [[WRITE:%.*]] = begin_access [modify] [unknown] [[PROJECT_ITERATOR_BOX]] : $*T.Iterator
// CHECK:   [[GET_NEXT_FUNC:%.*]] = witness_method $T.Iterator, #IteratorProtocol.next : <Self where Self : IteratorProtocol> (inout Self) -> () -> Self.Element? : $@convention(witness_method: IteratorProtocol) <τ_0_0 where τ_0_0 : IteratorProtocol> (@inout τ_0_0) -> @out Optional<τ_0_0.Element>
// CHECK:   apply [[GET_NEXT_FUNC]]<T.Iterator>([[ELT_STACK]], [[WRITE]])
// CHECK:   switch_enum_addr [[ELT_STACK]] : $*Optional<T.Element>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]:
// CHECK:   [[T0:%.*]] = alloc_stack [lexical] [var_decl] $T.Element, let, name "x"
// CHECK:   [[ELT_STACK_TAKE:%.*]] = unchecked_take_enum_data_addr [[ELT_STACK]] : $*Optional<T.Element>, #Optional.some!enumelt
// CHECK:   copy_addr [take] [[ELT_STACK_TAKE]] to [init] [[T0]]
// CHECK:   cond_br {{%.*}}, [[LOOP_BREAK_END_BLOCK:bb[0-9]+]], [[CONTINUE_CHECK_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_BREAK_END_BLOCK]]:
// CHECK:   [[LOOP_BREAK_FUNC:%.*]] = function_ref @loopBreakEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BREAK_FUNC]]()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[CONT_BLOCK:bb[0-9]+]]
//
// CHECK: [[CONTINUE_CHECK_BLOCK]]:
// CHECK:   cond_br {{%.*}}, [[LOOP_CONTINUE_END:bb[0-9]+]], [[LOOP_END_BLOCK:bb[0-9]+]]
//
// CHECK: [[LOOP_CONTINUE_END]]:
// CHECK:   [[LOOP_CONTINUE_FUNC:%.*]] = function_ref @loopContinueEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_CONTINUE_FUNC]]() : $@convention(thin) () -> ()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[LOOP_END_BLOCK]]:
// CHECK:   [[LOOP_BODY_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_BODY_FUNC]]()
// CHECK:   destroy_addr [[T0]]
// CHECK:   dealloc_stack [[T0]]
// CHECK:   br [[LOOP_DEST]]
//
// CHECK: [[NONE_BB]]:
// CHECK:   br [[CONT_BLOCK]]
//
// CHECK: [[CONT_BLOCK]]
// CHECK:   dealloc_stack [[ELT_STACK]]
// CHECK:   end_borrow [[ITERATOR_LIFETIME]]
// CHECK:   destroy_value [[ITERATOR_BOX]]
// CHECK:   [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK:   apply [[FUNC_END_FUNC]]()
// CHECK: } // end sil function '$s7foreach30genericCollectionContinueBreakyyxSlRzlF'
func genericCollectionContinueBreak<T : Collection>(_ xx: T) {
  for x in xx {
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

//===----------------------------------------------------------------------===//
// Pattern Match Tests
//===----------------------------------------------------------------------===//

// CHECK-LABEL: sil hidden [ossa] @$s7foreach13tupleElementsyySayAA1CC_ADtGF
func tupleElements(_ xx: [(C, C)]) {
  // CHECK: bb{{.*}}([[PAYLOAD:%.*]] : @owned $(C, C)):
  // CHECK: ([[A:%.*]], [[B:%.*]]) = destructure_tuple [[PAYLOAD]]
  // CHECK: [[MOVED_A:%.*]] = move_value [lexical] [var_decl] [[A]]
  // CHECK: [[MOVED_B:%.*]] = move_value [lexical] [var_decl] [[B]]
  // CHECK: destroy_value [[MOVED_B]]
  // CHECK: destroy_value [[MOVED_A]]
  for (a, b) in xx {}
  // CHECK: bb{{.*}}([[PAYLOAD:%.*]] : @owned $(C, C)):
  // CHECK: ([[A:%.*]], [[B:%.*]]) = destructure_tuple [[PAYLOAD]]
  // CHECK: [[MOVED_A:%.*]] = move_value [lexical] [var_decl] [[A]]
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[MOVED_A]]
  for (a, _) in xx {}
  // CHECK: bb{{.*}}([[PAYLOAD:%.*]] : @owned $(C, C)):
  // CHECK: ([[A:%.*]], [[B:%.*]]) = destructure_tuple [[PAYLOAD]]
  // CHECK: [[MOVED_B:%.*]] = move_value [lexical] [var_decl] [[B]]
  // CHECK: destroy_value [[A]]
  // CHECK: destroy_value [[MOVED_B]]
  for (_, b) in xx {}
  // CHECK: bb{{.*}}([[PAYLOAD:%.*]] : @owned $(C, C)):
  // CHECK: ([[A:%.*]], [[B:%.*]]) = destructure_tuple [[PAYLOAD]]
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[A]]
  for (_, _) in xx {}
  // CHECK: bb{{.*}}([[PAYLOAD:%.*]] : @owned $(C, C)):
  // CHECK: ([[A:%.*]], [[B:%.*]]) = destructure_tuple [[PAYLOAD]]
  // CHECK: destroy_value [[B]]
  // CHECK: destroy_value [[A]]
  for  _     in xx {}
}

// Make sure that when we have an unused value, we properly iterate over the
// loop rather than run through the loop once.
//
// CHECK-LABEL: sil hidden [ossa] @$s7foreach16unusedArgPatternyySaySiGF : $@convention(thin) (@guaranteed Array<Int>) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $Array<Int>):
// CHECK:   br [[LOOP_DEST:bb[0-9]+]]
//
// CHECK: [[LOOP_DEST]]:
// CHECK:   switch_enum [[OPT_VAL:%.*]] : $Optional<Int>, case #Optional.some!enumelt: [[SOME_BB:bb[0-9]+]], case #Optional.none!enumelt: [[NONE_BB:bb[0-9]+]]
//
// CHECK: [[SOME_BB]]([[VAL:%.*]]  : $Int):
// CHECK:   [[LOOP_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK:   apply [[LOOP_END_FUNC]]
func unusedArgPattern(_ xx: [Int]) {
  for _ in xx {
    loopBodyEnd()
  }
}

// https://github.com/apple/swift/issues/53670
// Make sure that the SIL contains the needed upcast.
//
// CHECK-LABEL: sil hidden [ossa] @$s7foreach25genericFuncWithConversion4listySayxG_tAA1CCRbzlF
// CHECK: bb2([[ITER_VAL:%.*]] : @owned $T):
// CHECK:   [[ITER_VAL_UPCAST:%.*]] = upcast [[ITER_VAL]] : $T to $C
func genericFuncWithConversion<T: C>(list : [T]) {
  for item: C in list {
    print(item)
  }
}

// https://github.com/apple/swift/issues/51201
// Check that branch on result of 'next()' precedes optional injection.
// If we branch on the converted result of 'next()', the loop won't terminate.

// CHECK-LABEL: sil hidden [ossa] @$s7foreach32injectForEachElementIntoOptionalyySaySiGF
// CHECK: [[NEXT_RESULT:%.*]] = load [trivial] {{.*}} : $*Optional<Int>
// CHECK: switch_enum [[NEXT_RESULT]] : $Optional<Int>, case #Optional.some!enumelt: [[BB_SOME:bb.*]], case
// CHECK: [[BB_SOME]]([[X_PRE_BINDING:%.*]] : $Int):
// CHECK: [[X_BINDING:%.*]] = enum $Optional<Int>, #Optional.some!enumelt, [[X_PRE_BINDING]] : $Int
// CHECK: [[MVX_BINDING:%.*]] = move_value [var_decl] [[X_BINDING]] : $Optional<Int>
// CHECK: debug_value [[MVX_BINDING]] : $Optional<Int>, let, name "x"
func injectForEachElementIntoOptional(_ xs: [Int]) {
  for x : Int? in xs {}
}

// CHECK-LABEL: sil hidden [ossa] @$s7foreach32injectForEachElementIntoOptionalyySayxGlF
// CHECK: copy_addr [take] [[NEXT_RESULT:%.*]] to [init] [[NEXT_RESULT_COPY:%.*]] : $*Optional<T>
// CHECK: switch_enum_addr [[NEXT_RESULT_COPY]] : $*Optional<T>, case #Optional.some!enumelt: [[BB_SOME:bb.*]], case
// CHECK: [[BB_SOME]]:
// CHECK: [[X_BINDING:%.*]] = alloc_stack [lexical] [var_decl] $Optional<T>, let, name "x"
// CHECK: [[ADDR:%.*]] = unchecked_take_enum_data_addr [[NEXT_RESULT_COPY]] : $*Optional<T>, #Optional.some!enumelt
// CHECK: [[X_ADDR:%.*]] = init_enum_data_addr [[X_BINDING]] : $*Optional<T>, #Optional.some!enumelt
// CHECK: copy_addr [take] [[ADDR]] to [init] [[X_ADDR]] : $*T
// CHECK: inject_enum_addr [[X_BINDING]] : $*Optional<T>, #Optional.some!enumelt
func injectForEachElementIntoOptional<T>(_ xs: [T]) {
  for x : T? in xs {}
}
