// RUN: %target-swift-frontend -O -emit-sil %s -Xllvm -debug-only=sil-inliner -Xllvm -debug-only=array-element-propagation -Xllvm -debug-only=cowarray-opts 2>&1 | %FileCheck %s

 // Test nested array semantic calls.
 //
 // The relevant sequence of passes is:
 //
 // - Early inlining does *not* inlinine Array.append(contentsOf:).
 //
 // - Early inlining inlines testInlineElts -> testInlineAppend.
 //
 // - ArrayElementPropagation of literal '[1, 2]' replaces
 // append(contentsOf:) with two calls to Array.append and removes the
 // temporary array literal.
 //
 // - Performance inlining does *not* initially inline any nested Array semantic
 // calls, like "_makeUniqueAndReserveCapacityIfNotUnique".
 //
 // - Performance inlining inlines Array.append(contentsOf:) and resets
 // the function pipeline.
 //
 // - COWArrayOpts hoists the call to _makeUniqueAndReserveCapacityIfNotUnique".

 // Here is the same sequence with interleaved CHECKs:

 // - Performance inlining does not initially any nested Array semantic
 // CHECK-NOT: inline [{{.*}}]] $sSa6append10contentsOfyqd__n_t7ElementQyd__RszSTRd__lF
 // CHECK-NOT: inline [{{.*}}] $sSa034_makeUniqueAndReserveCapacityIfNotB0yyFSi_Tg5

 // - Early inlining inlines testInlineElts -> testInlineAppend.
 // CHECK-LABEL: Inline into caller: $s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF
 // CHECK:       inline [{{.*}}] $s22array_semantics_nested14testInlineElts_4eltsySaySiGz_ADtF

 // CHECK-NOT: inline [{{.*}}]] $sSa6append10contentsOfyqd__n_t7ElementQyd__RszSTRd__lF
 // CHECK-NOT: inline [{{.*}}] $sSa034_makeUniqueAndReserveCapacityIfNotB0yyFSi_Tg5

 // - ArrayElementPropagation of literal '[1, 2]' replaces
 // append(contentsOf:) with two calls to Array.append and removes the
 // temporary array literal.
 // CHECK: Array append contentsOf calls replaced in $s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF (1)

 // - Performance inlining does *not* initially inline any nested Array semantic
 // calls, like "_makeUniqueAndReserveCapacityIfNotUnique".

 // CHECK-NOT: inline [{{.*}}]] $sSa6append10contentsOfyqd__n_t7ElementQyd__RszSTRd__lF
 // CHECK-NOT: inline [{{.*}}] $sSa034_makeUniqueAndReserveCapacityIfNotB0yyFSi_Tg5

 // - Performance inlining inlines Array.append(Element) and resets
 // the function pipeline.

 // CHECK: Inline into caller: $s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF
 // CHECK: inline [{{.*}}] $sSa6appendyyxnFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa6appendyyxnFSi_Tg5

 // CHECK: COW Array Opts in Func $s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF
 // CHECK:   Array Opts in Loop Loop at depth 1 containing:
 // CHECK:     Checking mutable array:   %{{.*}} = alloc_stack $Array<Int>, var, name "result"
 // CHECK: Hoisting make_mutable:
 // CHECK: Removing make_mutable call:

 // CHECK-NOT: Inline into caller

 // - The next round of inlinling in the same function pipeline.
 // CHECK: Inline into caller: $s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF
 // CHECK: inline [{{.*}}] $sSa034_makeUniqueAndReserveCapacityIfNotB0yyFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa9_getCountSiyFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa12_getCapacitySiyFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa15reserveCapacityyySiFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa9_getCountSiyFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa36_reserveCapacityAssumingUniqueBuffer8oldCountySi_tFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa37_appendElementAssumeUniqueAndCapacity_03newB0ySi_xntFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa9_getCountSiyFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa36_reserveCapacityAssumingUniqueBuffer8oldCountySi_tFSi_Tg5
 // CHECK: inline [{{.*}}] $sSa37_appendElementAssumeUniqueAndCapacity_03newB0ySi_xntFSi_Tg5

 // This helper ensures that at least one round of inlining is needed
 // *before* inlining Array.append.
 func testInlineElts(_ a: inout [Int], elts: [Int]) -> () {
   a.append(contentsOf: elts)
 }

  // CHECK-LABEL: sil @$s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF : $@convention(thin) (Int) -> @owned Array<Int> {
 // CHECK: bb0(%0 : $Int):
 // Initialize the array...
 // CHECK:   [[RESULTARRAY:%[0-9]+]] = alloc_stack $Array<Int>, var, name "result"
 // CHECK:   store %{{.*}} to [[RESULTARRAY]] : $*Array<Int>

 // Perform the uniqueness check... (FIXME: should be able to optimize this away since it hasn't been escaped)
 // CHECK:   [[BUFADR:%[0-9]+]] = struct_element_addr [[RESULTARRAY]] : $*Array<Int>, #Array._buffer
 // CHECK:   [[STORADR:%[0-9]+]] = struct_element_addr [[BUFADR]] : $*_ArrayBuffer<Int>, #_ArrayBuffer._storage
 // CHECK:   [[BRIDGE:%[0-9]+]] = struct_element_addr [[STORADR]] : $*_BridgeStorage<__ContiguousArrayStorageBase>, #_BridgeStorage.rawValue
 // CHECK:   [[NATIVE:%[0-9]+]] = unchecked_addr_cast [[BRIDGE]] : $*Builtin.BridgeObject to $*Builtin.NativeObject
 // CHECK:   [[UNIQ:%[0-9]+]] = is_unique [[NATIVE]] : $*Builtin.NativeObject
 // CHECK:   [[EXPECT:%[0-9]+]] = builtin "int_expect_Int1"([[UNIQ]] : $Builtin.Int1
 // CHECK:   cond_br [[EXPECT]]

 // Enter the loop...
 // CHECK: [[LOOPBB:bb[0-9]+]](%{{.*}} : $Builtin.Int64): // Preds: [[TAILBB:bb[0-9]+]] bb

 // CHECK-NOT: apply

 // Reserve capacity... FIXME: There is still an is_uniq inside the
 // loop because of reserveCapacity.  Either reserveCapacity should be
 // split into a hoistable uniqueness check, or we should be able to
 // prove that the uniqueness check is dominated with no escape.
 // CHECK:   %59 = load [[BRIDGE]] : $*Builtin.BridgeObject
 // CHECK:   %63 = struct_element_addr %62 : $*_SwiftArrayBodyStorage, #_SwiftArrayBodyStorage.count
 // CHECK:   %65 = load %64 : $*Builtin.Int64
 // CHECK:   %67 = struct_element_addr %62 : $*_SwiftArrayBodyStorage, #_SwiftArrayBodyStorage._capacityAndFlags
 // CHECK:   %69 = load %68 : $*Builtin.Int64
 // CHECK:   cond_br %76, bb8, bb9
 // CHECK-NOT: apply
 // CHECK: bb
 // CHECK:   is_unique %26 : $*Builtin.NativeObject
 // CHECK:   cond_br %90, bb14, bb13

 // CHECK-NOT: apply

 // CHECK:   [[SZF:%[0-9]+]] = function_ref @_swift_stdlib_malloc_size : $@convention(c) (UnsafeRawPointer) -> Int
 // CHECK:   apply [[SZF]](%{{.*}}) : $@convention(c) (UnsafeRawPointer) -> Int

 // CHECK-NOT: apply

 // CHECK: builtin "copyArray"<Int>(%153 : $@thick Int.Type, %150 : $Builtin.RawPointer, %152 : $Builtin.RawPointer, %154 : $Builtin.Word) : $()
 // CHECK: store %{{.*}} to [[RESULTARRAY]] : $*Array<Int>

 // CHECK-NOT: apply

 // CHECK:   %198 = function_ref @$sSa16_copyToNewBuffer8oldCountySi_tFSi_Tg5 : $@convention(method) (Int, @inout Array<Int>) -> ()
 // CHECK:   %199 = apply %198(%197, %2) : $@convention(method) (Int, @inout Array<Int>) -> ()

 // CHECK-NOT: apply

 // CHECK:   %233 = function_ref @$sSa16_copyToNewBuffer8oldCountySi_tFSi_Tg5 : $@convention(method) (Int, @inout Array<Int>) -> ()
 // CHECK:   %234 = apply %233(%232, %2) : $@convention(method) (Int, @inout Array<Int>) -> ()

 // CHECK-NOT: apply

 // CHECK: br [[LOOPBB]](%237 : $Builtin.Int64)
 // CHECK-LABEL: } // end sil function '$s22array_semantics_nested16testInlineAppend5countSaySiGSi_tF'

 public func testInlineAppend(count: Int) -> [Int] {
   var result = Array<Int>()
   for _ in 0..<count {
     testInlineElts(&result, elts: [1, 2])
   }
   return result
 }