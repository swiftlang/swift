
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -module-name function_conversion -primary-file %s | %FileCheck %s

// Check SILGen against various FunctionConversionExprs emitted by Sema.

// SE-0110 allows 'tuple splat' in a few narrow cases. Make sure SILGen supports them.

func takesTwo(_: ((Int, Int)) -> ()) {}

func givesTwo(_ fn: (Any, Any) -> ()) {
  takesTwo(fn)
}

// reabstraction thunk helper from @callee_guaranteed (@in_guaranteed Any, @in_guaranteed Any) -> () to @escaping @callee_guaranteed (@unowned Swift.Int, @unowned Swift.Int) -> ()
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sypypIgnn_S2iIegyy_TR : $@convention(thin) (Int, Int, @guaranteed @noescape @callee_guaranteed (@in_guaranteed Any, @in_guaranteed Any) -> ()) -> ()


func takesTwoGeneric<T>(_: (T) -> ()) -> T {}

func givesTwoGeneric(_ fn: (Int, Int) -> ()) {
  _ = takesTwoGeneric(fn)
}

// reabstraction thunk helper from @callee_guaranteed (@unowned Swift.Int, @unowned Swift.Int) -> () to @escaping @callee_guaranteed (@in_guaranteed (Swift.Int, Swift.Int)) -> ()
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sS2iIgyy_Si_SitIegn_TR : $@convention(thin) (@in_guaranteed (Int, Int), @guaranteed @noescape @callee_guaranteed (Int, Int) -> ()) -> ()


// Use a silly trick to bind T := (Int, Int) here
func givesTwoGenericAny(_ fn: (Any, Any) -> ()) -> (Int, Int) {
  return takesTwoGeneric(fn)
}

func givesNoneGeneric(_ fn: () -> ()) {
  _ = takesTwoGeneric(fn)
}

// reabstraction thunk helper from @callee_guaranteed () -> () to @escaping @callee_guaranteed (@in_guaranteed ()) -> ()
// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIg_ytIegn_TR : $@convention(thin) (@in_guaranteed (), @guaranteed @noescape @callee_guaranteed () -> ()) -> ()


// "Tuple splat" still works if there are __owned parameters.
// Make sure the memory management is correct also.

// CHECK-LABEL: sil hidden [ossa] @$s19function_conversion17takesTwoAnyObjectyyyyXl_yXlt_tXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (@guaranteed AnyObject, @guaranteed AnyObject) -> ()) -> ()
func takesTwoAnyObject(_: ((AnyObject, AnyObject)) -> ()) {}

// CHECK-LABEL: sil hidden [ossa] @$s19function_conversion22givesTwoAnyObjectOwnedyyyyXln_yXlntXEF : $@convention(thin) (@guaranteed @noescape @callee_guaranteed (@owned AnyObject, @owned AnyObject) -> ()) -> ()
func givesTwoAnyObjectOwned(_ fn: (__owned AnyObject, __owned AnyObject) -> ()) {
  takesTwoAnyObject(fn)
}

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$syXlyXlIgxx_yXlyXlIeggg_TR : $@convention(thin) (@guaranteed AnyObject, @guaranteed AnyObject, @guaranteed @noescape @callee_guaranteed (@owned AnyObject, @owned AnyObject) -> ()) -> () {
// CHECK: bb0(%0 : @guaranteed $AnyObject, %1 : @guaranteed $AnyObject, %2 : @guaranteed $@noescape @callee_guaranteed (@owned AnyObject, @owned AnyObject) -> ()):
// CHECK-NEXT: [[FIRST:%.*]] = copy_value %0
// CHECK-NEXT: [[SECOND:%.*]] = copy_value %1
// CHECK-NEXT: apply %2([[FIRST]], [[SECOND]]) : $@noescape @callee_guaranteed (@owned AnyObject, @owned AnyObject) -> ()
// CHECK-NEXT: [[RESULT:%.*]] = tuple ()
// CHECK-NEXT: return [[RESULT]] : $()
// CHECK-NEXT: }
