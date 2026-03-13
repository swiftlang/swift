// RUN: %target-swift-emit-sil -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx50.3 -verify
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s | %FileCheck %s
// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -parse-as-library -module-name back_deploy %s -target %target-cpu-apple-macosx50.3 | %FileCheck %s

// REQUIRES: OS=macosx

@available(macOS 50.3, *)
public struct TopLevelStruct<T> {
  @usableFromInline var t: T

  // -- Fallback definition for TopLevelStruct.init(_:)
  // CHECK-LABEL: sil non_abi [serialized] [ossa] @$s11back_deploy14TopLevelStructVyACyxGxcfCTwB : $@convention(method) <T> (@in T, @thin TopLevelStruct<T>.Type) -> @out TopLevelStruct<T>
  // CHECK: bb0([[SELF_OUT:%.*]] : $*TopLevelStruct<T>, [[T_ARG:%.*]] : $*T, [[METATYPE_ARG:%.*]] : $@thin TopLevelStruct<T>.Type):
  // CHECK:   [[SELF:%.*]] = alloc_box $<τ_0_0> { var TopLevelStruct<τ_0_0> } <T>, var, name "self"
  // CHECK:   [[MARKED_SELF:%.*]] = mark_uninitialized [rootself] [[SELF]] : $<τ_0_0> { var TopLevelStruct<τ_0_0> } <T>
  // CHECK:   [[BORROWED_SELF:%.*]] = begin_borrow [lexical] [var_decl] [[MARKED_SELF]] : $<τ_0_0> { var TopLevelStruct<τ_0_0> } <T>
  // CHECK:   [[BOX:%.*]] = project_box [[BORROWED_SELF]] : $<τ_0_0> { var TopLevelStruct<τ_0_0> } <T>, 0
  // CHECK:   [[T_STACK:%.*]] = alloc_stack $T
  // CHECK:   copy_addr [[T_ARG]] to [init] [[T_STACK]] : $*T
  // CHECK:   [[ACCESS:%.*]] = begin_access [modify] [unknown] [[BOX]] : $*TopLevelStruct<T>
  // CHECK:   [[ELEM_ADDR:%.*]] = struct_element_addr [[ACCESS]] : $*TopLevelStruct<T>, #TopLevelStruct.t
  // CHECK:   copy_addr [take] [[T_STACK]] to [[ELEM_ADDR]] : $*T
  // CHECK:   end_access [[ACCESS]] : $*TopLevelStruct<T>
  // CHECK:   dealloc_stack [[T_STACK]] : $*T
  // CHECK:   copy_addr [[BOX]] to [init] [[SELF_OUT]] : $*TopLevelStruct<T>
  // CHECK:   destroy_addr [[T_ARG]] : $*T
  // CHECK:   end_borrow [[BORROWED_SELF]] : $<τ_0_0> { var TopLevelStruct<τ_0_0> } <T>
  // CHECK:   destroy_value [[MARKED_SELF]] : $<τ_0_0> { var TopLevelStruct<τ_0_0> } <T>
  // CHECK:   [[RESULT:%.*]] = tuple ()
  // CHECK:   return [[RESULT]] : $()

  // -- Back deployment thunk for TopLevelStruct.init(_:)
  // CHECK-LABEL: sil non_abi [serialized] [back_deployed_thunk] [ossa] @$s11back_deploy14TopLevelStructVyACyxGxcfCTwb : $@convention(method) <T> (@in T, @thin TopLevelStruct<T>.Type) -> @out TopLevelStruct<T>
  // CHECK: bb0([[SELF_OUT:%.*]] : $*TopLevelStruct<T>, [[T_ARG:%.*]] : $*T, [[METATYPE_ARG:%.*]] : $@thin TopLevelStruct<T>.Type):
  // CHECK:   [[MAJOR:%.*]] = integer_literal $Builtin.Word, 52
  // CHECK:   [[MINOR:%.*]] = integer_literal $Builtin.Word, 1
  // CHECK:   [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK:   [[OSVFN:%.*]] = function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK:   [[AVAIL:%.*]] = apply [[OSVFN]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@convention(thin) (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK:   cond_br [[AVAIL]], [[AVAIL_BB:bb[0-9]+]], [[UNAVAIL_BB:bb[0-9]+]]
  //
  // CHECK: [[UNAVAIL_BB]]:
  // CHECK:   [[FALLBACKFN:%.*]] = function_ref @$s11back_deploy14TopLevelStructVyACyxGxcfCTwB : $@convention(method) <τ_0_0> (@in τ_0_0, @thin TopLevelStruct<τ_0_0>.Type) -> @out TopLevelStruct<τ_0_0>
  // CHECK:   {{%.*}} = apply [[FALLBACKFN]]<T>([[SELF_OUT]], [[T_ARG]], [[METATYPE_ARG]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @thin TopLevelStruct<τ_0_0>.Type) -> @out TopLevelStruct<τ_0_0>
  // CHECK:   br [[RETURN_BB:bb[0-9]+]]
  //
  // CHECK: [[AVAIL_BB]]:
  // CHECK:   [[ORIGFN:%.*]] = function_ref @$s11back_deploy14TopLevelStructVyACyxGxcfC : $@convention(method) <τ_0_0> (@in τ_0_0, @thin TopLevelStruct<τ_0_0>.Type) -> @out TopLevelStruct<τ_0_0>
  // CHECK:   {{%.*}} = apply [[ORIGFN]]<T>([[SELF_OUT]], [[T_ARG]], [[METATYPE_ARG]]) : $@convention(method) <τ_0_0> (@in τ_0_0, @thin TopLevelStruct<τ_0_0>.Type) -> @out TopLevelStruct<τ_0_0>
  // CHECK:   br [[RETURN_BB]]
  //
  // CHECK: [[RETURN_BB]]
  // CHECK:   [[RESULT:%.*]] = tuple ()
  // CHECK:   return [[RESULT]] : $()

  // -- Original definition of TopLevelStruct.init(_:)
  // CHECK-LABEL: sil [available 52.1] [ossa] @$s11back_deploy14TopLevelStructVyACyxGxcfC : $@convention(method) <T> (@in T, @thin TopLevelStruct<T>.Type) -> @out TopLevelStruct<T>
  @available(macOS 51.0, *)
  @backDeployed(before: macOS 52.1)
  public init(_ t: T) {
    self.t = t
  }
}

struct S {}

// CHECK-LABEL: sil hidden [available 51.0] [ossa] @$s11back_deploy6calleryyAA1SVF : $@convention(thin) (S) -> ()
// CHECK: bb0([[STRUCT_ARG:%.*]] : $S):
@available(macOS 51.0, *)
func caller(_ s: S) {
  // -- Verify the thunk is called
  // CHECK: {{%.*}} = function_ref @$s11back_deploy14TopLevelStructVyACyxGxcfCTwb : $@convention(method) <τ_0_0> (@in τ_0_0, @thin TopLevelStruct<τ_0_0>.Type) -> @out TopLevelStruct<τ_0_0>
  _ = TopLevelStruct(s)
}

