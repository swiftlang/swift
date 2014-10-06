// RUN: %swift -emit-silgen -target x86_64-apple-macosx10.9 -enable-experimental-availability-checking -enable-experimental-unavailable-as-optional %s | FileCheck %s

@availability(OSX, introduced=10.9)
var globalAvailableOn10_9: Int = 9

@availability(OSX, introduced=10.10)
var globalAvailableOn10_10: Int = 10

// CHECK-LABEL: sil hidden @_TF22availability_optionals34referenceToAvailableGlobalVariableFT_T_
func referenceToAvailableGlobalVariable() {
  // A definitely available global variable gets loaded as usual.
  let _ = globalAvailableOn10_9
  
  // CHECK: [[GLOBAL_ADDR:%.*]] = sil_global_addr @_Tv22availability_optionals21globalAvailableOn10_9Si : $*Int
  // CHECK: [[LOADED_VAL:%.*]] = load [[GLOBAL_ADDR]] : $*Int
}

// CHECK-LABEL: sil hidden @_TF22availability_optionals47referenceToPotentiallyUnavailableGlobalVariableFT_T_
func referenceToPotentiallyUnavailableGlobalVariable() {
  // A potentially unavailable global variable gets injected into an optional.
  let _ = globalAvailableOn10_10
 
  // Check the required availability.
  // CHECK: [[GLOBAL_ADDR:%.*]] = sil_global_addr @_Tv22availability_optionals22globalAvailableOn10_10Si : $*Int
  // CHECK: [[OPT_ADDR:%.*]] = alloc_stack $Optional<Int> 
  // CHECK: [[MAJOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[MINOR:%.*]] = integer_literal $Builtin.Word, 10
  // CHECK: [[PATCH:%.*]] = integer_literal $Builtin.Word, 0
  // CHECK: [[QUERY_FUNC:%.*]] = function_ref @_TFSs26_stdlib_isOSVersionAtLeastFTBwBwBw_Bi1_ : $@thin (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: [[QUERY_RESULT:%.*]] = apply [[QUERY_FUNC]]([[MAJOR]], [[MINOR]], [[PATCH]]) : $@thin (Builtin.Word, Builtin.Word, Builtin.Word) -> Builtin.Int1
  // CHECK: cond_br [[QUERY_RESULT]], [[AVAILABLE:bb[0-9]+]], [[UNAVAILABLE:bb[0-9]+]]
  
  // Load and inject the value when available.
  // CHECK: [[AVAILABLE]]:
  // CHECK: [[GLOBAL_VAL:%.*]] = load [[GLOBAL_ADDR]] : $*Int
  // CHECK: [[DATA_ADDR:%.*]] = init_enum_data_addr [[OPT_ADDR]]#1 : $*Optional<Int>, #Optional.Some!enumelt.1
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
