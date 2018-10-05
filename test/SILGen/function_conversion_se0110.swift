
// RUN: %target-swift-emit-silgen -module-name function_conversion -enable-sil-ownership -primary-file %s | %FileCheck %s

// Check SILGen against various FunctionConversionExprs emitted by Sema.

// SE-0110 allows 'tuple splat' in a few narrow cases. Make sure SILGen supports them.

func takesTwo(_: ((Int, Int)) -> ()) {}

func givesTwo(_ fn: (Any, Any) -> ()) {
  takesTwo(fn)
}

// reabstraction thunk helper from @callee_guaranteed (@in_guaranteed Any, @in_guaranteed Any) -> () to @escaping @callee_guaranteed (@unowned Swift.Int, @unowned Swift.Int) -> ()
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sypypIgnn_S2iIegyy_TR : $@convention(thin) (Int, Int, @noescape @callee_guaranteed (@in_guaranteed Any, @in_guaranteed Any) -> ()) -> ()


func takesTwoGeneric<T>(_: (T) -> ()) -> T {}

func givesTwoGeneric(_ fn: (Int, Int) -> ()) {
  _ = takesTwoGeneric(fn)
}

// reabstraction thunk helper from @callee_guaranteed (@unowned Swift.Int, @unowned Swift.Int) -> () to @escaping @callee_guaranteed (@in_guaranteed (Swift.Int, Swift.Int)) -> ()
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sS2iIgyy_Si_SitIegn_TR : $@convention(thin) (@in_guaranteed (Int, Int), @noescape @callee_guaranteed (Int, Int) -> ()) -> ()


// Use a silly trick to bind T := (Int, Int) here
func givesTwoGenericAny(_ fn: (Any, Any) -> ()) -> (Int, Int) {
  return takesTwoGeneric(fn)
}

func givesNoneGeneric(_ fn: () -> ()) {
  _ = takesTwoGeneric(fn)
}

// reabstraction thunk helper from @callee_guaranteed () -> () to @escaping @callee_guaranteed (@in_guaranteed ()) -> ()
// CHECK-LABEL: sil shared [transparent] [serializable] [reabstraction_thunk] @$sIg_ytIegn_TR : $@convention(thin) (@in_guaranteed (), @noescape @callee_guaranteed () -> ()) -> ()
