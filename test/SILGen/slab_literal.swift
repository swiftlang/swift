// RUN: %target-swift-emit-silgen %s -disable-availability-checking -enable-experimental-feature ValueGenerics | %FileCheck %s

// REQUIRES: swift_feature_ValueGenerics

import Synchronization

// CHECK-LABEL: sil{{.*}} @$s12slab_literal7trivials4SlabVy$3_SiGyF : $@convention(thin) () -> Slab<4, Int> {
// CHECK:         [[SLAB_ALLOC:%.*]] = alloc_stack $Slab<4, Int>
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = unchecked_addr_cast [[SLAB_ALLOC]] to $*Int
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
// CHECK-LABEL: } // end sil function '$s12slab_literal7trivials4SlabVy$3_SiGyF'
func trivial() -> Slab<4, Int> {
  [1, 2, 3, 4]
}

// CHECK-LABEL: sil{{.*}} @$s12slab_literal10nontrivials4SlabVy$1_SSGyF : $@convention(thin) () -> @owned Slab<2, String> {
// CHECK:         [[SLAB_ALLOC:%.*]] = alloc_stack $Slab<2, String>
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = unchecked_addr_cast [[SLAB_ALLOC]] to $*String
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
// CHECK-LABEL: } // end sil function '$s12slab_literal10nontrivials4SlabVy$1_SSGyF'
func nontrivial() -> Slab<2, String> {
  ["hello", "world"]
}

// CHECK-LABEL: sil{{.*}} @$s12slab_literal11noncopyables4SlabVy$1_15Synchronization6AtomicVySiGGyF : $@convention(thin) () -> @out Slab<2, Atomic<Int>> {
// CHECK:       bb0([[SLAB_RETURN:%.*]] : $*Slab<2, Atomic<Int>>):
// CHECK-NEXT:    [[SLAB_ALLOC:%.*]] = alloc_stack $Slab<2, Atomic<Int>>
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = unchecked_addr_cast [[SLAB_ALLOC]] to $*Atomic<Int>
// CHECK:         [[ATOMIC_INIT:%.*]] = function_ref @$s15Synchronization6AtomicVyACyxGxcfC
// CHECK-NEXT:    [[ELT_0:%.*]] = apply [[ATOMIC_INIT]]<Int>([[ELEMENT_PTR]], {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AtomicRepresentable> (@in τ_0_0, @thin Atomic<τ_0_0>.Type) -> @out Atomic<τ_0_0>
// CHECK:         [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]], [[ELT_1_OFFSET]]
// CHECK:         [[ATOMIC_INIT:%.*]] = function_ref @$s15Synchronization6AtomicVyACyxGxcfC
// CHECK-NEXT:    [[ELT_1:%.*]] = apply [[ATOMIC_INIT]]<Int>([[ELT_1_PTR]], {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AtomicRepresentable> (@in τ_0_0, @thin Atomic<τ_0_0>.Type) -> @out Atomic<τ_0_0>
// CHECK:         [[SLAB_ALLOC_AGAIN:%.*]] = unchecked_addr_cast [[SLAB_ALLOC]] to $*Slab<2, Atomic<Int>>
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box
// CHECK-NEXT:    [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK-NEXT:    [[BOX_PROJECT:%.*]] = project_box [[BOX_BORROW]]
// CHECK-NEXT:    copy_addr [take] [[SLAB_ALLOC_AGAIN]] to [init] [[BOX_PROJECT]]
// CHECK-NEXT:    dealloc_stack [[SLAB_ALLOC]]
// CHECK-NEXT:    copy_addr [take] [[BOX_PROJECT]] to [init] [[SLAB_RETURN]]
// CHECK-NEXT:    end_borrow [[BOX_BORROW]]
// CHECK-NEXT:    dealloc_box [[BOX]]
// CHECK-LABEL: } // end sil function '$s12slab_literal11noncopyables4SlabVy$1_15Synchronization6AtomicVySiGGyF'
func noncopyable() -> Slab<2, Atomic<Int>> {
  [Atomic(0), Atomic(1)]
}
