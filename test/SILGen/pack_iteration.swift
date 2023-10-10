
// RUN: %target-swift-emit-silgen -module-name pack_iteration %s | %FileCheck %s

//////////////////
// Declarations //
//////////////////
@_silgen_name("loopBodyEnd")
func loopBodyEnd() -> ()

@_silgen_name("funcEnd")
func funcEnd() -> ()

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
// CHECK: [[SOME_BB]]:
// CHECK: [[DYN_PACK_IDX:%.*]] = dynamic_pack_index [[IDX3]] of $Pack{repeat each Element}
// CHECK: open_pack_element [[DYN_PACK_IDX]] of <each Element> at <Pack{repeat each Element}>, shape $each Element, uuid "[[UUID:.*]]"
// CHECK: [[STACK:%.*]] = alloc_stack [lexical] $@pack_element("[[UUID]]") each Element, let, name "element"
// CHECK: [[PACK_ELT_GET:%.*]] = pack_element_get [[DYN_PACK_IDX]] of [[PACK]] : $*Pack{repeat each Element} as $*@pack_element("[[UUID]]") each Element
// CHECK: copy_addr [[PACK_ELT_GET]] to [init] [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[LOOP_END_FUNC:%.*]] = function_ref @loopBodyEnd : $@convention(thin) () -> ()
// CHECK: apply [[LOOP_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: destroy_addr [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: dealloc_stack [[STACK]] : $*@pack_element("[[UUID]]") each Element
// CHECK: [[IDX4:%.*]] = builtin "add_Word"([[IDX3]] : $Builtin.Word, [[IDX2]] : $Builtin.Word) : $Builtin.Word
// CHECK: br [[LOOP_DEST]]([[IDX4]] : $Builtin.Word)
//
// CHECK: [[NONE_BB]]:
// CHECK:  [[FUNC_END_FUNC:%.*]] = function_ref @funcEnd : $@convention(thin) () -> ()
// CHECK: apply [[FUNC_END_FUNC]]() : $@convention(thin) () -> ()
// CHECK: } // end sil function '$s14pack_iteration14iterateTrivial4overyxxQp_tRvzlF'
func iterateTrivial<each Element>(over elements: repeat each Element) {
  for element in repeat each elements {
    loopBodyEnd()
  }
  funcEnd()
}

// TODO: Write this test
func equalTuples<each Element: Equatable>(lhs: (repeat each Element), rhs: (repeat each Element)) -> Bool {

//  %12 = dynamic_pack_index %9 of $Pack{repeat (each Element, each Element)} // users: %19, %17, %14, %13
//  %13 = open_pack_element %12 of <each Element where repeat each Element : Equatable> at <Pack{repeat each Element}>, shape $each Element, uuid "E53D635E-3D89-11EE-82A2-7AABAFDC7DCA" // users: %19, %17, %14
//  %14 = tuple_pack_element_addr %12 of %4 : $*(repeat (each Element, each Element)) as $*(@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element, @pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element) // users: %16, %15
//  %15 = tuple_element_addr %14 : $*(@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element, @pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element), 0 // user: %18
//  %16 = tuple_element_addr %14 : $*(@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element, @pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element), 1 // user: %20
//  %17 = pack_element_get %12 of %0 : $*Pack{repeat each Element} as $*@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element // user: %18
//  copy_addr %17 to [init] %15 : $*@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element // id: %18
//  %19 = pack_element_get %12 of %1 : $*Pack{repeat each Element} as $*@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element // user: %20
//  copy_addr %19 to [init] %16 : $*@pack_element("E53D635E-3D89-11EE-82A2-7AABAFDC7DCA") each Element // id: %20
//  %21 = builtin "add_Word"(%9 : $Builtin.Word, %6 : $Builtin.Word) : $Builtin.Word // user: %22
//  br bb1(%21 : $Builtin.Word)

  for (left, right) in repeat (each lhs, each rhs) {
    guard left == right else { return false }
  }
  return true
}

// TODO: Write this test
func iteratePatternMatch<each Element>(over element: repeat E<each Element>) {
  for case .one(let value) in repeat each element {
    print(value)
  }
}


