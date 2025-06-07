// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx10.50 | %FileCheck %s

// REQUIRES: OS=macosx

// -- Fallback definition of trivialFunc()
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy11trivialFuncyyFTwB : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Back deployment thunk for trivialFunc()
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy11trivialFuncyyFTwb : $@convention(thin) () -> ()
// CHECK: bb0:
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy11trivialFuncyyFTwB : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[FALLBACKFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB:bb[0-9]+]]
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy11trivialFuncyyF : $@convention(thin) () -> ()
// CHECK:   {{%.*}} = apply [[ORIGFN]]() : $@convention(thin) () -> ()
// CHECK:   br [[RETURN_BB]]
//
// CHECK: [[RETURN_BB]]
// CHECK:   [[RESULT:%.*]] = tuple ()
// CHECK:   return [[RESULT]] : $()

// -- Original definition of trivialFunc()
// CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy11trivialFuncyyF : $@convention(thin) () -> ()
@backDeployed(before: macOS 52.1)
public func trivialFunc() {}

// -- Fallback definition of isNumber(_:)
// CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy8isNumberySbSiFTwB : $@convention(thin) (Int) -> Bool
// CHECK: bb0([[ARG_X:%.*]] : $Int):
// ...
// CHECK:   return {{%.*}} : $Bool

// -- Back deployment thunk for isNumber(_:)
// CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy8isNumberySbSiFTwb : $@convention(thin) (Int) -> Bool
// CHECK: bb0([[ARG_X:%.*]] : $Int):
// CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
// CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
// CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
// CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
// CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
//
// CHECK: [[UNAVAIL_BB]]:
// CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy8isNumberySbSiFTwB : $@convention(thin) (Int) -> Bool
// CHECK:   [[RESULT:%.*]] = apply [[FALLBACKFN]]([[ARG_X]]) : $@convention(thin) (Int) -> Bool
// CHECK:   br [[RETURN_BB:bb[0-9]+]]([[RESULT]] : $Bool)
//
// CHECK: [[AVAIL_BB]]:
// CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy8isNumberySbSiF : $@convention(thin) (Int) -> Bool
// CHECK:   [[RESULT:%.*]] = apply [[ORIGFN]]([[ARG_X]]) : $@convention(thin) (Int) -> Bool
// CHECK:   br [[RETURN_BB]]([[RESULT]] : $Bool)
//
// CHECK: [[RETURN_BB]]([[RETURN_BB_ARG:%.*]] : $Bool)
// CHECK:   return [[RETURN_BB_ARG]] : $Bool

// -- Original definition of isNumber(_:)
// CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy8isNumberySbSiF : $@convention(thin) (Int) -> Bool
@backDeployed(before: macOS 52.1)
public func isNumber(_ x: Int) -> Bool {
  return true
}

// CHECK-LABEL: sil hidden [ossa] @$s11back_deploy6calleryyF : $@convention(thin) () -> ()
func caller() {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy11trivialFuncyyFTwb : $@convention(thin) () -> ()
  trivialFunc()
  // CHECK: {{%.*}} = function_ref @$s11back_deploy8isNumberySbSiFTwb : $@convention(thin) (Int) -> Bool
  _ = isNumber(6)
}
