// RUN: %target-swift-emit-silgen %s -disable-availability-checking -enable-experimental-feature ValueGenerics | %FileCheck %s

// 123REQUIRES123: swift_feature_ValueGenerics

import Synchronization

// CHECK-LABEL: sil{{.*}} @$s14vector_literal7trivials6VectorVy$3_SiGyF : $@convention(thin) () -> Vector<4, Int> {
// CHECK:         [[VECTOR_ALLOC:%.*]] = alloc_stack $Vector<4, Int>
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = unchecked_addr_cast [[VECTOR_ALLOC]] : $*Vector<4, Int> to $*Int
// CHECK-NEXT:    [[ELT_0_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 1
// CHECK:         [[ELT_0:%.*]] = apply {{%.*}}([[ELT_0_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_0]] to [trivial] [[ELEMENT_PTR]] : $*Int
// CHECK-NEXT:    [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]] : $*Int, [[ELT_1_OFFSET]] : $Builtin.Word
// CHECK-NEXT:    [[ELT_1_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 2
// CHECK:         [[ELT_1:%.*]] = apply {{%.*}}([[ELT_1_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_1]] to [trivial] [[ELT_1_PTR]] : $*Int
// CHECK-NEXT:    [[ELT_2_OFFSET:%.*]] = integer_literal $Builtin.Word, 2
// CHECK-NEXT:    [[ELT_2_PTR:%.*]] = index_addr [[ELEMENT_PTR]] : $*Int, [[ELT_2_OFFSET]] : $Builtin.Word
// CHECK-NEXT:    [[ELT_2_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 3
// CHECK:         [[ELT_2:%.*]] = apply {{%.*}}([[ELT_2_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_2]] to [trivial] [[ELT_2_PTR]] : $*Int
// CHECK-NEXT:    [[ELT_3_OFFSET:%.*]] = integer_literal $Builtin.Word, 3
// CHECK-NEXT:    [[ELT_3_PTR:%.*]] = index_addr [[ELEMENT_PTR]] : $*Int, [[ELT_3_OFFSET]] : $Builtin.Word
// CHECK-NEXT:    [[ELT_3_LITERAL:%.*]] = integer_literal $Builtin.IntLiteral, 4
// CHECK:         [[ELT_3:%.*]] = apply {{%.*}}([[ELT_3_LITERAL]], {{%.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT:    store [[ELT_3]] to [trivial] [[ELT_3_PTR]] : $*Int
// CHECK-NEXT:    [[VECTOR:%.*]] = load [trivial] [[VECTOR_ALLOC]] : $*Vector<4, Int>
// CHECK-NEXT:    dealloc_stack [[VECTOR_ALLOC]] : $*Vector<4, Int>
// CHECK-NEXT:    return [[VECTOR]] : $Vector<4, Int>
// CHECK-LABEL: } // end sil function '$s14vector_literal7trivials6VectorVy$3_SiGyF'
func trivial() -> Vector<4, Int> {
  [1, 2, 3, 4]
}

// CHECK-LABEL: sil{{.*}} @$s14vector_literal10nontrivials6VectorVy$1_SSGyF : $@convention(thin) () -> @owned Vector<2, String> {
// CHECK:         [[VECTOR_ALLOC:%.*]] = alloc_stack $Vector<2, String>
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = unchecked_addr_cast [[VECTOR_ALLOC]] : $*Vector<2, String> to $*String
// CHECK-NEXT:    [[ELT_0_LITERAL:%.*]] = string_literal utf8 "hello"
// CHECK:         [[ELT_0:%.*]] = apply {{%.*}}([[ELT_0_LITERAL]], {{.*}}) : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK-NEXT:    store [[ELT_0]] to [init] [[ELEMENT_PTR]] : $*String
// CHECK-NEXT:    [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]] : $*String, [[ELT_1_OFFSET]] : $Builtin.Word
// CHECK-NEXT:    [[ELT_1_LITERAL:%.*]] = string_literal utf8 "world"
// CHECK:         [[ELT_1:%.*]] = apply {{%.*}}([[ELT_1_LITERAL]], {{.*}}) : $@convention(method) (Builtin.RawPointer, Builtin.Word, Builtin.Int1, @thin String.Type) -> @owned String
// CHECK-NEXT:    store [[ELT_1]] to [init] [[ELT_1_PTR]] : $*String
// CHECK-NEXT:    [[VECTOR:%.*]] = load [take] [[VECTOR_ALLOC]] : $*Vector<2, String>
// CHECK-NEXT:    dealloc_stack [[VECTOR_ALLOC]] : $*Vector<2, String>
// CHECK-NEXT:    return [[VECTOR]] : $Vector<2, String>
// CHECK-LABEL: } // end sil function '$s14vector_literal10nontrivials6VectorVy$1_SSGyF'
func nontrivial() -> Vector<2, String> {
  ["hello", "world"]
}

// CHECK-LABEL: sil{{.*}} @$s14vector_literal11noncopyables6VectorVy$1_15Synchronization6AtomicVySiGGyF : $@convention(thin) () -> @out Vector<2, Atomic<Int>> {
// CHECK:       bb0([[VECTOR_RETURN:%.*]] : $*Vector<2, Atomic<Int>>):
// CHECK-NEXT:    [[VECTOR_ALLOC:%.*]] = alloc_stack $Vector<2, Atomic<Int>>
// CHECK-NEXT:    [[ELEMENT_PTR:%.*]] = unchecked_addr_cast [[VECTOR_ALLOC]] : $*Vector<2, Atomic<Int>> to $*Atomic<Int>
// CHECK:         [[ATOMIC_INIT:%.*]] = function_ref @$s15Synchronization6AtomicVyACyxGxcfC
// CHECK-NEXT:    [[ELT_0:%.*]] = apply [[ATOMIC_INIT]]<Int>([[ELEMENT_PTR]], {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AtomicRepresentable> (@in τ_0_0, @thin Atomic<τ_0_0>.Type) -> @out Atomic<τ_0_0>
// CHECK:         [[ELT_1_OFFSET:%.*]] = integer_literal $Builtin.Word, 1
// CHECK-NEXT:    [[ELT_1_PTR:%.*]] = index_addr [[ELEMENT_PTR]] : $*Atomic<Int>, [[ELT_1_OFFSET]] : $Builtin.Word
// CHECK:         [[ATOMIC_INIT:%.*]] = function_ref @$s15Synchronization6AtomicVyACyxGxcfC
// CHECK-NEXT:    [[ELT_1:%.*]] = apply [[ATOMIC_INIT]]<Int>([[ELT_1_PTR]], {{.*}}) : $@convention(method) <τ_0_0 where τ_0_0 : AtomicRepresentable> (@in τ_0_0, @thin Atomic<τ_0_0>.Type) -> @out Atomic<τ_0_0>
// CHECK:         [[VECTOR_ALLOC_AGAIN:%.*]] = unchecked_addr_cast [[VECTOR_ALLOC]] : $*Vector<2, Atomic<Int>> to $*Vector<2, Atomic<Int>>
// CHECK-NEXT:    [[BOX:%.*]] = alloc_box
// CHECK-NEXT:    [[BOX_BORROW:%.*]] = begin_borrow [lexical] [[BOX]]
// CHECK-NEXT:    [[BOX_PROJECT:%.*]] = project_box [[BOX_BORROW]]
// CHECK-NEXT:    copy_addr [take] [[VECTOR_ALLOC_AGAIN]] to [init] [[BOX_PROJECT]] : $*Vector<2, Atomic<Int>>
// CHECK-NEXT:    dealloc_stack [[VECTOR_ALLOC]] : $*Vector<2, Atomic<Int>>
// CHECK-NEXT:    copy_addr [take] [[BOX_PROJECT]] to [init] [[VECTOR_RETURN]] : $*Vector<2, Atomic<Int>>
// CHECK-NEXT:    end_borrow [[BOX_BORROW]]
// CHECK-NEXT:    dealloc_box [[BOX]]
// CHECK-LABEL: } // end sil function '$s14vector_literal11noncopyables6VectorVy$1_15Synchronization6AtomicVySiGGyF'
func noncopyable() -> Vector<2, Atomic<Int>> {
  [Atomic(0), Atomic(1)]
}
