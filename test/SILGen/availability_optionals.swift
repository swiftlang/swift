// RUN: %target-swift-frontend -emit-silgen -enable-experimental-availability-checking -enable-experimental-unavailable-as-optional -primary-file %s %S/Inputs/availability_optionals_other.swift | FileCheck %s

// REQUIRES: OS=macosx

// CHECK-LABEL: sil hidden @_TF22availability_optionals34referenceToAvailableGlobalVariableFT_T_
func referenceToAvailableGlobalVariable() {
  // A definitely available global variable gets loaded as usual.
  let _ = globalAvailableOn10_9

  // CHECK: [[GLOBAL_ADDRESSOR:%.*]] = function_ref @_TF22availability_optionalsau21globalAvailableOn10_9Si : $@thin () -> Builtin.RawPointer
  // CHECK: [[GLOBAL_POINTER:%.*]] = apply [[GLOBAL_ADDRESSOR]]() : $@thin () -> Builtin.RawPointer
  // CHECK: [[GLOBAL_ADDR:%.*]] = pointer_to_address [[GLOBAL_POINTER]] : $Builtin.RawPointer to $*Int
  // CHECK: [[LOADED_VAL:%.*]] = load [[GLOBAL_ADDR]] : $*Int
}

// CHECK-LABEL: sil hidden @_TF22availability_optionals47referenceToPotentiallyUnavailableGlobalVariableFT_T_
func referenceToPotentiallyUnavailableGlobalVariable() {
  // A potentially unavailable global variable gets injected into an optional.
  let _ = globalAvailableOn10_10
 
  // Check the required availability.
  // CHECK: [[OPT_ADDR:%.*]] = alloc_stack $Optional<Int> 
  // CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@thin (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@thin (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: cond_br [[QUERY_RESULT]], [[AVAILABLE:bb[0-9]+]], [[UNAVAILABLE:bb[0-9]+]]
  
  // Load and inject the value when available.
  // CHECK: [[AVAILABLE]]:
  // CHECK: [[DATA_ADDR:%.*]] = init_enum_data_addr [[OPT_ADDR]]#1 : $*Optional<Int>, #Optional.Some!enumelt.1
  // CHECK: [[GLOBAL_ADDRESSOR:%.*]] = function_ref @_TF22availability_optionalsau22globalAvailableOn10_10Si : $@thin () -> Builtin.RawPointer
  // CHECK: [[GLOBAL_POINTER:%.*]] = apply [[GLOBAL_ADDRESSOR]]() : $@thin () -> Builtin.RawPointer
  // CHECK: [[GLOBAL_ADDR:%.*]] = pointer_to_address [[GLOBAL_POINTER]] : $Builtin.RawPointer to $*Int
  // CHECK: [[GLOBAL_VAL:%.*]] = load [[GLOBAL_ADDR]] : $*Int
  // CHECK: store [[GLOBAL_VAL]] to [[DATA_ADDR]] : $*Int
  // CHECK: inject_enum_addr [[OPT_ADDR]]#1 : $*Optional<Int>, #Optional.Some!enumelt.1
  // CHECK: br [[CONT:bb[0-9]+]]

  // Inject Nothing when not available.
  // CHECK: [[UNAVAILABLE]]:
  // CHECK: inject_enum_addr [[OPT_ADDR]]#1 : $*Optional<Int>, #Optional.None!enumelt
  // CHECK: br [[CONT]] 

  // CHECK: [[CONT]]:
  // [[IGNORED:%.*]] = load [[OPT_ADDR]]#1 : $*Optional<Int>
  // dealloc_stack [[OPT_ADDR]]#0 : $*@local_storage Optional<Int>
}

@availability(OSX, introduced=10.9)
func funcAvailableOn10_9() -> Int { return 9 }

@availability(OSX, introduced=10.10)
func funcAvailableOn10_10() -> Int { return 10 }

// CHECK-LABEL: sil hidden @_TF22availability_optionals34referenceToAvailableGlobalFunctionFT_T_
func referenceToAvailableGlobalFunction() {
  // A definitely available global function gets treated as usual.
  let _ = funcAvailableOn10_9()
  
  // CHECK: [[FUNC_REF:%.*]] = function_ref @_TF22availability_optionals19funcAvailableOn10_9FT_Si : $@thin () -> Int // user: %1
  // [[IGNORED:%.*]] = apply [[FUNC_REF]]() : $@thin () -> Int
}

// CHECK-LABEL: sil hidden @_TF22availability_optionals47referenceToPotentiallyUnavailableGlobalFunctionFT_T_
func referenceToPotentiallyUnavailableGlobalFunction() {
  // A potentially unavailable global function gets treated as having optional type.
  let _ = funcAvailableOn10_10
  
  // Check the required availability.
  // CHECK: [[OPT_ADDR:%.*]] = alloc_stack $Optional<() -> Int> 
  // CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@thin (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@thin (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: cond_br [[QUERY_RESULT]], [[AVAILABLE:bb[0-9]+]], [[UNAVAILABLE:bb[0-9]+]]
  
  // Load and inject the value when available.
  // CHECK: [[AVAILABLE]]:
  // CHECK: [[DATA_ADDR:%.*]] = init_enum_data_addr [[OPT_ADDR]]#1 : $*Optional<() -> Int>, #Optional.Some!enumelt.1
  // CHECK: [[FUNC_VAL:%.*]] = function_ref @_TF22availability_optionals20funcAvailableOn10_10FT_Si : $@thin () -> Int
  // CHECK: [[THICK_FUNC_VAL:%.*]] = thin_to_thick_function [[FUNC_VAL]] : $@thin () -> Int to $@callee_owned () -> Int
  // CHECK: [[REABSTRACTION_THUNK_HELPER:%.*]] = function_ref @[[HELPER_NAME:.*]] : $@thin (@out Int, @in (), @owned @callee_owned () -> Int) -> ()
  // CHECK: [[STORED_VAL:%.*]] = partial_apply [[REABSTRACTION_THUNK_HELPER]]([[THICK_FUNC_VAL]]) : $@thin (@out Int, @in (), @owned @callee_owned () -> Int) -> ()
  // CHECK: store [[STORED_VAL]] to [[DATA_ADDR]] : $*@callee_owned (@out Int, @in ()) -> ()
  // CHECK: inject_enum_addr [[OPT_ADDR]]#1 : $*Optional<() -> Int>, #Optional.Some!enumelt.1
  // CHECK: br [[CONT:bb[0-9]+]]

  // Inject Nothing when not available.
  // CHECK: [[UNAVAILABLE]]:
  // CHECK: inject_enum_addr [[OPT_ADDR]]#1 : $*Optional<() -> Int>, #Optional.None!enumelt
  // CHECK: br [[CONT]] 

  // CHECK: [[CONT]]:
  // [[IGNORED:%.*]] = load [[OPT_ADDR]]#1 : $*Optional<() -> Int>
  // dealloc_stack [[OPT_ADDR]]#0 : $*@local_storage Optional<() -> Int>
}
