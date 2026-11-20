// RUN: %target-swift-emit-silgen -enable-experimental-feature PackExpansionInArrayLiterals -target %target-swift-5.9-abi-triple %s | %FileCheck %s

// REQUIRES: swift_feature_PackExpansionInArrayLiterals

// The array is allocated with the number of scalar elements plus the length
// of each pack expansion, and each expansion is emitted with a dynamic pack
// loop that stores into the elements following the preceding ones.

// CHECK-LABEL: sil hidden [ossa] @$s4main5mixed_1uSayypGxxQp_q_q_QptRvzRv_r0_lF :
// CHECK:       [[SCALARS:%.*]] = integer_literal $Builtin.Word, 2
// CHECK:       [[TLEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK:       [[COUNT1:%.*]] = builtin "add_Word"([[SCALARS]], [[TLEN]])
// CHECK:       [[ULEN:%.*]] = pack_length $Pack{repeat each U}
// CHECK:       [[COUNT:%.*]] = builtin "add_Word"([[COUNT1]], [[ULEN]])
// CHECK:       [[ALLOC:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK:       apply [[ALLOC]]<Any>([[COUNT]])
// CHECK:       [[BASE:%.*]] = ref_tail_addr {{%.*}}, $Any
//   First, the scalar element 0
// CHECK:       [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:       index_addr [projection] [[BASE]], [[ZERO]]
//   Then, the elements of `t`, starting at index 1
// CHECK:       [[TSTART:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:       br [[TCOND:bb[0-9]+]]
// CHECK:     [[TCOND]]([[TIDX:%.*]] : $Builtin.Word):
// CHECK:       cond_br {{%.*}}, [[TEND:bb[0-9]+]], [[TBODY:bb[0-9]+]]
// CHECK:     [[TBODY]]:
// CHECK:       [[TINDEX:%.*]] = builtin "add_Word"([[TSTART]], [[TIDX]])
// CHECK:       [[TADDR:%.*]] = index_addr [projection] [[BASE]], [[TINDEX]]
// CHECK:       init_existential_addr [[TADDR]], $@pack_element({{.*}}) each T
// CHECK:     [[TEND]]:
//   Then, the scalar element 1, right after `t`
// CHECK:       [[TNEXT:%.*]] = builtin "add_Word"([[TSTART]], [[TLEN]])
// CHECK:       index_addr [projection] [[BASE]], [[TNEXT]]
//   Then, the elements of `u`, starting after the scalar element
// CHECK:       [[ONE:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:       [[USTART:%.*]] = builtin "add_Word"([[TNEXT]], [[ONE]])
// CHECK:       br [[UCOND:bb[0-9]+]]
// CHECK:     [[UCOND]]([[UIDX:%.*]] : $Builtin.Word):
// CHECK:       cond_br {{%.*}}, [[UEND:bb[0-9]+]], [[UBODY:bb[0-9]+]]
// CHECK:     [[UBODY]]:
// CHECK:       [[UINDEX:%.*]] = builtin "add_Word"([[USTART]], [[UIDX]])
// CHECK:       [[UADDR:%.*]] = index_addr [projection] [[BASE]], [[UINDEX]]
// CHECK:       init_existential_addr [[UADDR]], $@pack_element({{.*}}) each U
// CHECK:     [[UEND]]:
// CHECK-NEXT:  end_borrow
//   There's at least one scalar element, so the array is always finalized.
// CHECK:       [[FINALIZE:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK:       apply [[FINALIZE]]<Any>
// CHECK:     } // end sil function '$s4main5mixed_1uSayypGxxQp_q_q_QptRvzRv_r0_lF'
func mixed<each T, each U>(_ t: repeat each T, u: repeat each U) -> [Any] {
  return [0, repeat each t, 1, repeat each u]
}

// If the array only contains pack expansions it might be empty, in which case
// it's the empty array singleton, which must not be finalized.

// CHECK-LABEL: sil hidden [ossa] @$s4main5toAnyySayypGxxQpRvzlF :
// CHECK:       [[COUNT:%.*]] = pack_length $Pack{repeat each T}
// CHECK:       [[ALLOC:%.*]] = function_ref @$ss27_allocateUninitializedArrayySayxG_BptBwlF
// CHECK:       [[TUPLE:%.*]] = apply [[ALLOC]]<Any>([[COUNT]])
// CHECK:       ([[ARRAY:%.*]], {{%.*}}) = destructure_tuple [[TUPLE]]
// CHECK:       end_borrow
// CHECK:       [[ZERO:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:       [[EMPTY:%.*]] = builtin "cmp_eq_Word"([[COUNT]], [[ZERO]])
// CHECK:       cond_br [[EMPTY]], [[EMPTYBB:bb[0-9]+]], [[NONEMPTYBB:bb[0-9]+]]
// CHECK:     [[CONT:bb[0-9]+]]([[RESULT:%.*]] : @owned $Array<Any>):
// CHECK:       return [[RESULT]]
// CHECK:     [[NONEMPTYBB]]:
// CHECK:       [[FINALIZE:%.*]] = function_ref @$ss27_finalizeUninitializedArrayySayxGABnlF
// CHECK:       [[FINALIZED:%.*]] = apply [[FINALIZE]]<Any>([[ARRAY]])
// CHECK:       br [[CONT]]([[FINALIZED]])
// CHECK:     [[EMPTYBB]]:
// CHECK:       br [[CONT]]([[ARRAY]])
// CHECK:     } // end sil function '$s4main5toAnyySayypGxxQpRvzlF'
func toAny<each T>(_ t: repeat each T) -> [Any] {
  return [repeat each t]
}

// Elements of the pack expansion are destroyed if a later element throws.

class C {}
func make<T>(_: T) throws -> C { C() }

// CHECK-LABEL: sil hidden [ossa] @$s4main8throwingySayAA1CCGxxQpKRvzlF :
// CHECK:       [[BASE:%.*]] = ref_tail_addr {{%.*}}, $C
// CHECK-NEXT:  [[START:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:       br [[COND:bb[0-9]+]]
// CHECK:     [[COND]]([[IDX:%.*]] : $Builtin.Word):
// CHECK:       cond_br {{%.*}}, {{bb[0-9]+}}, [[BODY:bb[0-9]+]]
// CHECK:     [[BODY]]:
// CHECK:       try_apply {{%.*}}<@pack_element({{.*}}) each T>({{%.*}}) : {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
//   On error, destroy the preceding elements of the expansion and deallocate
//   the array.
// CHECK:     [[ERROR]]([[E:%.*]] : @owned $any Error):
// CHECK:       br [[DCOND:bb[0-9]+]]
// CHECK:       [[DEALLOC:%.*]] = function_ref @$ss29_deallocateUninitializedArrayyySayxGnlF
// CHECK:       apply [[DEALLOC]]<C>
// CHECK:       throw [[E]]
// CHECK:       [[DINDEX:%.*]] = builtin "add_Word"([[START]], [[DIDX:%.*]] : $Builtin.Word)
// CHECK:       [[DADDR:%.*]] = index_addr [projection] [[BASE]], [[DINDEX]]
// CHECK:       destroy_addr [[DADDR]]
// CHECK:     [[DCOND]]([[DIDX]] : $Builtin.Word):
// CHECK:       builtin "cmp_eq_Word"([[DIDX]], [[IDX]])
// CHECK:     } // end sil function '$s4main8throwingySayAA1CCGxxQpKRvzlF'
func throwing<each T>(_ t: repeat each T) throws -> [C] {
  return [repeat try make(each t)]
}

// Once all elements of the expansion are initialized, they're destroyed if a
// later element throws.

// CHECK-LABEL: sil hidden [ossa] @$s4main13throwingAfterySayAA1CCGxxQpKRvzlF :
// CHECK:       [[TLEN:%.*]] = pack_length $Pack{repeat each T}
// CHECK:       [[BASE:%.*]] = ref_tail_addr {{%.*}}, $C
// CHECK-NEXT:  [[START:%.*]] = integer_literal $Builtin.Word, 0
//   The scalar element after the expansion...
// CHECK:       try_apply {{%.*}}<Int>({{%.*}}) : {{.*}}, normal {{bb[0-9]+}}, error [[ERROR:bb[0-9]+]]
//   ...destroys all of the expansion's elements if it throws.
// CHECK:     [[ERROR]]([[E:%.*]] : @owned $any Error):
// CHECK:       br [[DCOND:bb[0-9]+]]
// CHECK:       [[DEALLOC:%.*]] = function_ref @$ss29_deallocateUninitializedArrayyySayxGnlF
// CHECK:       apply [[DEALLOC]]<C>
// CHECK:       [[DINDEX:%.*]] = builtin "add_Word"([[START]], [[DIDX:%.*]] : $Builtin.Word)
// CHECK:       [[DADDR:%.*]] = index_addr [projection] [[BASE]], [[DINDEX]]
// CHECK:       destroy_addr [[DADDR]]
// CHECK:     [[DCOND]]([[DIDX]] : $Builtin.Word):
// CHECK:       builtin "cmp_eq_Word"([[DIDX]], [[TLEN]])
// CHECK:     } // end sil function '$s4main13throwingAfterySayAA1CCGxxQpKRvzlF'
func throwingAfter<each T>(_ t: repeat each T) throws -> [C] {
  return [repeat try make(each t), try make(0)]
}
