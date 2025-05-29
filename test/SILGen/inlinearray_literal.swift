// RUN: %target-swift-emit-silgen %s -disable-availability-checking | %FileCheck %s

import Synchronization

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal12emptyTrivials11InlineArrayVy$_SiGyF : $@convention(thin) () -> InlineArray<0, Int> {
// CHECK:         [[EMPTY_TUPLE:%.*]] = tuple ()
// CHECK-NEXT:    [[IA:%.*]] = unchecked_trivial_bit_cast [[EMPTY_TUPLE]] to $InlineArray<0, Int>
// CHECK-NEXT:    return [[IA]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal12emptyTrivials11InlineArrayVy$_SiGyF'
func emptyTrivial() -> InlineArray<0, Int> {
  []
}

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal15emptyNontrivials11InlineArrayVy$_SSGyF : $@convention(thin) () -> @owned InlineArray<0, String> {
// CHECK:         [[EMPTY_TUPLE:%.*]] = tuple ()
// CHECK-NEXT:    [[IA:%.*]] = unchecked_bitwise_cast [[EMPTY_TUPLE]] to $InlineArray<0, String>
// CHECK-NEXT:    [[COPY_IA:%.*]] = copy_value [[IA]]
// CHECK-NEXT:    return [[COPY_IA]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal15emptyNontrivials11InlineArrayVy$_SSGyF'
func emptyNontrivial() -> InlineArray<0, String> {
  []
}

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal16emptyNoncopyables11InlineArrayVy$_15Synchronization6AtomicVySiGGyF : $@convention(thin) () -> @out InlineArray<0, Atomic<Int>> {
// CHECK:       bb0([[IA_RETURN:%.*]] : $*InlineArray<0, Atomic<Int>>):
// CHECK-NEXT:    [[IA_ALLOC:%.*]] = alloc_stack $InlineArray<0, Atomic<Int>>
// CHECK:         [[IA_CAST:%.*]] = unchecked_addr_cast [[IA_ALLOC]] to $*InlineArray<0, Atomic<Int>>
// CHECK-NEXT:    [[IA_BOX:%.*]] = alloc_box
// CHECK-NEXT:    [[BORROW_BOX:%.*]] =  begin_borrow [lexical] [[IA_BOX]]
// CHECK-NEXT:    [[PROJECT_BOX:%.*]] = project_box [[BORROW_BOX]], 0
// CHECK-NEXT:    copy_addr [take] [[IA_CAST]] to [init] [[PROJECT_BOX]]
// CHECK-NEXT:    dealloc_stack [[IA_ALLOC]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal16emptyNoncopyables11InlineArrayVy$_15Synchronization6AtomicVySiGGyF'
func emptyNoncopyable() -> InlineArray<0, Atomic<Int>> {
  []
}

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal7trivials11InlineArrayVy$3_SiGyF : $@convention(thin) () -> InlineArray<4, Int> {
// CHECK:         [[SLAB_ALLOC:%.*]] = alloc_stack $InlineArray<4, Int>
// CHECK-NEXT:    [[SE:%.*]] = struct_element_addr [[SLAB_ALLOC]], #InlineArray._storage
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = vector_base_addr [[SE]]
// CHECK-NEXT:    [[ELT_0_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:         [[ELT_0:%.*]] = apply {{%.*}}([[ELT_0_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_0]] to [trivial] [[ELEMENT_PTR]]
// CHECK-NEXT:    [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]], [[ELT_1_OFFSET]]
// CHECK-NEXT:    [[ELT_1_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 2
// CHECK:         [[ELT_1:%.*]] = apply {{%.*}}([[ELT_1_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_1]] to [trivial] [[ELT_1_PTR]]
// CHECK-NEXT:    [[ELT_2_OFFSET:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-NEXT:    [[ELT_2_PTR:%.*]] = index_addr [[ELEMENT_PTR]], [[ELT_2_OFFSET]]
// CHECK-NEXT:    [[ELT_2_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 3
// CHECK:         [[ELT_2:%.*]] = apply {{%.*}}([[ELT_2_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_2]] to [trivial] [[ELT_2_PTR]]
// CHECK-NEXT:    [[ELT_3_OFFSET:%.*]] = integer_literal $Builtin.Word, 3
// CHECK-NEXT:    [[ELT_3_PTR:%.*]] = index_addr [[ELEMENT_PTR]], [[ELT_3_OFFSET]]
// CHECK-NEXT:    [[ELT_3_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 4
// CHECK:         [[ELT_3:%.*]] = apply {{%.*}}([[ELT_3_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_3]] to [trivial] [[ELT_3_PTR]]
// CHECK-NEXT:    [[SLAB:%.*]] = load [trivial] [[SLAB_ALLOC]]
// CHECK-NEXT:    dealloc_stack [[SLAB_ALLOC]]
// CHECK-NEXT:    return [[SLAB]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal7trivials11InlineArrayVy$3_SiGyF'
func trivial() -> InlineArray<4, Int> {
  [1, 2, 3, 4]
}

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal10nontrivials11InlineArrayVy$1_SSGyF : $@convention(thin) () -> @owned InlineArray<2, String> {
// CHECK:         [[SLAB_ALLOC:%.*]] = alloc_stack $InlineArray<2, String>
// CHECK-NEXT:    [[SE:%.*]] = struct_element_addr [[SLAB_ALLOC]], #InlineArray._storage
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = vector_base_addr [[SE]]
// CHECK-NEXT:    [[ELT_0_LITERAL:%.*]] = string_literal utf8 "hello"
// CHECK:         [[ELT_0:%.*]] = apply {{%.*}}([[ELT_0_LITERAL]], {{.*}}) : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK-NEXT:    store [[ELT_0]] to [init] [[ELEMENT_PTR]]
// CHECK-NEXT:    [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]], [[ELT_1_OFFSET]]
// CHECK-NEXT:    [[ELT_1_LITERAL:%.*]] = string_literal utf8 "world"
// CHECK:         [[ELT_1:%.*]] = apply {{%.*}}([[ELT_1_LITERAL]], {{.*}}) : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK-NEXT:    store [[ELT_1]] to [init] [[ELT_1_PTR]]
// CHECK-NEXT:    [[SLAB:%.*]] = load [take] [[SLAB_ALLOC]]
// CHECK-NEXT:    dealloc_stack [[SLAB_ALLOC]]
// CHECK-NEXT:    return [[SLAB]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal10nontrivials11InlineArrayVy$1_SSGyF'
func nontrivial() -> InlineArray<2, String> {
  ["hello", "world"]
}

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal11noncopyables11InlineArrayVy$1_15Synchronization6AtomicVySiGGyF : $@convention(thin) () -> @out InlineArray<2, Atomic<Int>> {
// CHECK:       bb0([[SLAB_RETURN:%.*]] : $*InlineArray<2, Atomic<Int>>):
// CHECK-NEXT:    [[SLAB_ALLOC:%.*]] = alloc_stack $InlineArray<2, Atomic<Int>>
// CHECK-NEXT:    [[SE:%.*]] = struct_element_addr [[SLAB_ALLOC]], #InlineArray._storage
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = vector_base_addr [[SE]]
// CHECK:         [[ATOMIC_INIT:%.*]] = function_ref @$s15Synchronization6AtomicVyACyxGxcfC
// CHECK-NEXT:    [[ELT_0:%.*]] = apply [[ATOMIC_INIT]]<Int>([[ELEMENT_PTR]], {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AtomicRepresentable> (@in τ_0_0, @thin Atomic<τ_0_0>.Type) -> @out Atomic<τ_0_0>
// CHECK:         [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]], [[ELT_1_OFFSET]]
// CHECK:         [[ATOMIC_INIT:%.*]] = function_ref @$s15Synchronization6AtomicVyACyxGxcfC
// CHECK-NEXT:    [[ELT_1:%.*]] = apply [[ATOMIC_INIT]]<Int>([[ELT_1_PTR]], {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AtomicRepresentable> (@in τ_0_0, @thin Atomic<τ_0_0>.Type) -> @out Atomic<τ_0_0>
// CHECK:         [[SLAB_ALLOC_AGAIN:%.*]] = unchecked_addr_cast [[SLAB_ALLOC]] to $*InlineArray<2, Atomic<Int>>
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box
// CHECK-NEXT:    [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK-NEXT:    [[BOX_PROJECT:%.*]] = project_box [[BOX_BORROW]]
// CHECK-NEXT:    copy_addr [take] [[SLAB_ALLOC_AGAIN]] to [init] [[BOX_PROJECT]]
// CHECK-NEXT:    dealloc_stack [[SLAB_ALLOC]]
// CHECK-NEXT:    copy_addr [take] [[BOX_PROJECT]] to [init] [[SLAB_RETURN]]
// CHECK-NEXT:    end_borrow [[BOX_BORROW]]
// CHECK-NEXT:    dealloc_box [[BOX]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal11noncopyables11InlineArrayVy$1_15Synchronization6AtomicVySiGGyF'
func noncopyable() -> InlineArray<2, Atomic<Int>> {
  [Atomic(0), Atomic(1)]
}

// CHECK-LABEL: sil{{.*}} @$s19inlinearray_literal7closures11InlineArrayVy$0_S2icGyF : $@convention(thin) () -> @owned InlineArray<1, (Int) -> Int> {
// CHECK:         [[IA_ALLOC:%.*]] = alloc_stack $InlineArray<1, (Int) -> Int>
// CHECK-NEXT:    [[SE:%.*]] = struct_element_addr [[IA_ALLOC]], #InlineArray._storage
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = vector_base_addr [[SE]]
// CHECK:         [[FN_REF:%.*]] = function_ref
// CHECK-NEXT:    [[THIN_TO_THICK_FN:%.*]] = thin_to_thick_function [[FN_REF]] to $@callee_guaranteed @substituted <τ_0_0, τ_0_1> (@in_guaranteed τ_0_0) -> @out τ_0_1 for <Int, Int>
// CHECK-NEXT:    store [[THIN_TO_THICK_FN]] to [init] [[ELEMENT_PTR]]
// CHECK-NEXT:    [[IA:%.*]] = load [take] [[IA_ALLOC]]
// CHECK-NEXT:    dealloc_stack [[IA_ALLOC]]
// CHECK-NEXT:    return [[IA]]
// CHECK-LABEL: } // end sil function '$s19inlinearray_literal7closures11InlineArrayVy$0_S2icGyF'
func closure() -> InlineArray<1, (Int) -> Int> {
  [{$0 * 2}]
}
