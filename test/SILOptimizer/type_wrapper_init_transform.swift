// RUN: %target-swift-frontend -enable-experimental-feature TypeWrappers -module-name test -disable-availability-checking -Xllvm -sil-print-after=definite-init -emit-sil %s -o /dev/null 2>&1 | %FileCheck %s

// REQUIRES: asserts

@typeWrapper
public struct Wrapper<W, S> {
  var underlying: S

  public init(for: W.Type, storage: S) {
    self.underlying = storage
  }

  public subscript<V>(propertyKeyPath _: KeyPath<W, V>,
                      storageKeyPath path: KeyPath<S, V>) -> V {
    get { underlying[keyPath: path] }
  }

  public subscript<V>(propertyKeyPath _: KeyPath<W, V>,
                      storageKeyPath path: WritableKeyPath<S, V>) -> V {
    get { underlying[keyPath: path] }
    set { underlying[keyPath: path] = newValue }
  }
}

@Wrapper
struct TrivialStruct {
  // CHECK-LABEL: sil hidden [ossa] @$s4test13TrivialStructVACycfC
  // CHECK: [[SELF:%.*]] = alloc_stack $TrivialStruct, var, name "self", implicit
  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack $(), var, name "_storage", implicit
  // CHECK: [[STORAGE_INST:%.*]] = alloc_stack $TrivialStruct.$Storage
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test13TrivialStructV8$StorageVAEycfC : $@convention(method) (@thin TrivialStruct.$Storage.Type) -> TrivialStruct.$Storage
  // CHECK: [[LOCAL_STORAGE_REF:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE]] : $*()
  // CHECK: end_access [[LOCAL_STORAGE_REF]] : $*()
  // CHECK: [[STORAGE_METATYPE:%.*]] = metatype $@thin TrivialStruct.$Storage.Type
  // CHECK: [[STORAGE_INIT_RES:%.*]] = apply [[STORAGE_INIT_REF]]([[STORAGE_METATYPE]]) : $@convention(method) (@thin TrivialStruct.$Storage.Type) -> TrivialStruct.$Storage
  // CHECK: store [[STORAGE_INIT_RES]] to [trivial] [[STORAGE_INST]] : $*TrivialStruct.$Storage
  // CHECK: [[WRAPPER_INIT_REF:%.*]] = function_ref @$s4test7WrapperV3for7storageACyxq_Gxm_q_tcfC
  // CHECK: [[SELF_ACCESS:%.*]] = begin_access [modify] [static] %1 : $*TrivialStruct
  // CHECK: [[STORAGE_VAR_REF:%.*]] = struct_element_addr [[SELF_ACCESS]] : $*TrivialStruct, #TrivialStruct.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TrivialStruct.Type
  // CHECK: [[WRAPPER_METATYPE:%.*]] = metatype $@thin Wrapper<TrivialStruct, TrivialStruct.$Storage>.Type
  // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TrivialStruct, TrivialStruct.$Storage>
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF]]<TrivialStruct, TrivialStruct.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE_INST]], [[WRAPPER_METATYPE]])
  // CHECK: [[STORAGE_VAR_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_VAR_REF]] : $*Wrapper<TrivialStruct, TrivialStruct.$Storage>
  // CHECK: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_VAR_ACCESS]] : $*Wrapper<TrivialStruct, TrivialStruct.$Storage>
  // CHECK: end_access [[STORAGE_VAR_ACCESS]] : $*Wrapper<TrivialStruct, TrivialStruct.$Storage>
  // CHECK: end_access [[SELF_ACCESS]] : $*TrivialStruct
  // CHECK: dealloc_stack [[WRAPPER_INST]] : $*Wrapper<TrivialStruct, TrivialStruct.$Storage>
  // CHECK: dealloc_stack [[STORAGE_INST]] : $*TrivialStruct.$Storage
  // CHECK: dealloc_stack [[LOCAL_STORAGE]] : $*()
  init() {}
}

@Wrapper
struct GenericStruct<T: Collection> {
  var test: T

  // CHECK-LABEL: sil hidden [ossa] @$s4test13GenericStructV1vACyxGx_tcfC : $@convention(method) <T where T : Collection> (@in T, @thin GenericStruct<T>.Type) -> @out GenericStruct<T>
  // CHECK: [[SELF:%.*]] = alloc_stack [lexical] $GenericStruct<T>, var, name "self", implicit
  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack [lexical] $(test: T), var, name "_storage", implicit
  // CHECK: [[TEST_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(test: T), 0
  // CHECK: assign_by_wrapper origin type_wrapper, [[FIELD_VALUE:%.*]] : $*T to [init] [[TEST_PROP]] : $*T, set {{.*}}
  // CHECK-NEXT: [[STORAGE_INST:%.*]] = alloc_stack $GenericStruct<T>.$Storage
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test13GenericStructV8$StorageVAaEyx_Gx_tcfC : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@in τ_0_0, @thin GenericStruct<τ_0_0>.$Storage.Type) -> @out GenericStruct<τ_0_0>.$Storage
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE]] : $*(test: T)
  // CHECK: [[T_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(test: T), 0
  // CHECK: [[T_VAL:%.*]] = alloc_stack $T
  // CHECK: copy_addr [[T_REF]] to [init] [[T_VAL]] : $*T
  // CHECK: end_access [[LOCAL_STORAGE_ACCESS]] : $*(test: T)
  // CHECK: [[STORAGE_METATYPE:%.*]] = metatype $@thin GenericStruct<T>.$Storage.Type
  // CHECK: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE_INST]], [[T_VAL]], [[STORAGE_METATYPE]]) : $@convention(method) <τ_0_0 where τ_0_0 : Collection> (@in τ_0_0, @thin GenericStruct<τ_0_0>.$Storage.Type) -> @out GenericStruct<τ_0_0>.$Storage
  // CHECK: [[WRAPPER_INIT_REF:%.*]] = function_ref @$s4test7WrapperV3for7storageACyxq_Gxm_q_tcfC
  // CHECK: [[SELF_ACCESS:%.*]] = begin_access [modify] [static] [[SELF]] : $*GenericStruct<T>
  // CHECK: [[STORAGE_VAR:%.*]] = struct_element_addr [[SELF_ACCESS]] : $*GenericStruct<T>, #GenericStruct.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick GenericStruct<T>.Type
  // CHECK: [[WRAPPER_METATYPE:%.*]] = metatype $@thin Wrapper<GenericStruct<T>, GenericStruct<T>.$Storage>.Type
  // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<GenericStruct<T>, GenericStruct<T>.$Storage>
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF]]<GenericStruct<T>, GenericStruct<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE_INST]], [[WRAPPER_METATYPE]])
  // CHECK: [[STORAGE_VAR_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_VAR]] : $*Wrapper<GenericStruct<T>, GenericStruct<T>.$Storage>
  // CHECK: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_VAR_ACCESS]] : $*Wrapper<GenericStruct<T>, GenericStruct<T>.$Storage>
  // CHECK: end_access [[STORAGE_VAR_ACCESS]] : $*Wrapper<GenericStruct<T>, GenericStruct<T>.$Storage>
  init(v: T) {
    self.test = v
  }
}

// The only difference between wrapped class and struct is how `self.$storage` is referenced.
@Wrapper
class GenericClass<T: Collection> {
  var test: T

  // CHECK-LABEL: sil hidden [ossa] @$s4test12GenericClassC1vACyxGx_tcfc : $@convention(method) <T where T : Collection> (@in T, @owned GenericClass<T>) -> @owned GenericClass<T>
  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack [lexical] $(test: T), var, name "_storage", implicit
  // CHECK: [[TEST_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(test: T), 0
  // CHECK: assign_by_wrapper origin type_wrapper, [[TEST_VAL:%.*]] : $*T to [init] [[TEST_PROP]] : $*T, set {{.*}}
  // CHECK: [[SELF_ACCESS:%.*]] = begin_borrow [[SELF:%.*]] : $GenericClass<T>
  // CHECK-NEXT: [[STORAGE_VAR_REF:%.*]] = ref_element_addr [[SELF_ACCESS]] : $GenericClass<T>, #GenericClass.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick GenericClass<T>.Type
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF:%.*]]<GenericClass<T>, GenericClass<T>.$Storage>([[WRAPPER_INST:%.*]], [[WRAPPED_METATYPE]], [[STORAGE_INST:%.*]], {{.*}})
  // CHECK: [[STORAGE_VAR_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_VAR_REF]] : $*Wrapper<GenericClass<T>, GenericClass<T>.$Storage>
  // CHECK-NEXT: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_VAR_ACCESS]] : $*Wrapper<GenericClass<T>, GenericClass<T>.$Storage>
  // CHECK-NEXT: end_access [[STORAGE_VAR_ACCESS]]
  // CHECK-NEXT: end_borrow [[SELF_ACCESS]]
  init(v: T) {
    self.test = v
  }
}

@Wrapper
struct TupleFlatteningTest<K: Collection & Hashable, V> {
  var a: Int
  var b: (String, (Int, K))
  var c: [K: V]
  var d: V?

  // Make sure that `_storage` is correctly flattened before its elements
  // are passed as arguments to $Storage.init(a:b:c:d:)

  // CHECK-LABEL: sil hidden [ossa] @$s4test19TupleFlatteningTestV1a1b1c1dACyxq_GSi_SS_Si_xttSDyxq_Gq_SgtcfC
  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack [lexical] $(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), var, name "_storage", implicit
  // CHECK: [[A_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 0
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $Int to [init] [[A_PROP]] : $*Int, set {{.*}} : $@callee_guaranteed (Int) -> ()
  // CHECK: [[B_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 1
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $*(String, (Int, K)) to [init] [[B_PROP]] : $*(String, (Int, K)), set {{.*}} : $@callee_guaranteed (@owned String, Int, @in K) -> ()
  // CHECK: [[C_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 2
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $Dictionary<K, V> to [init] [[C_PROP]] : $*Dictionary<K, V>, set {{.*}} : $@callee_guaranteed (@owned Dictionary<K, V>) -> ()
  // CHECK: [[D_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 3
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $*Optional<V> to [init] [[D_PROP]] : $*Optional<V>, set {{.*}} : $@callee_guaranteed (@in Optional<V>) -> ()
  // CHECK: [[STORAGE_VAR:%.*]] = alloc_stack $TupleFlatteningTest<K, V>.$Storage
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test19TupleFlatteningTestV8$StorageV1a1b1c1dAEyxq__GSi_SS_Si_xttSDyxq_Gq_SgtcfC
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE:%.*]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>)
  // CHECK: [[A_REF:%.*]] = tuple_element_addr %73 : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 0
  // CHECK: [[A:%.*]] = load [trivial] [[A_REF]] : $*Int
  // CHECK: [[B_TUPLE:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 1
  // CHECK: [[B_ELT_0_REF:%.*]] = tuple_element_addr [[B_TUPLE]] : $*(String, (Int, K)), 0
  // CHECK: [[B_ELT_0_VAL:%.*]] = load [copy] [[B_ELT_0_REF]] : $*String
  // CHECK: [[B_ELT_1_REF:%.*]] = tuple_element_addr [[B_TUPLE]] : $*(String, (Int, K)), 1
  // CHECK: [[B_ELT_1_0_REF:%.*]] = tuple_element_addr [[B_ELT_1_REF]] : $*(Int, K), 0
  // CHECK: [[B_ELT_1_0_VAL:%.*]] = load [trivial] [[B_ELT_1_0_REF]] : $*Int
  // CHECK: [[B_ELT_1_1_REF:%.*]] = tuple_element_addr [[B_ELT_1_REF]] : $*(Int, K), 1
  // CHECK: [[K:%.*]] = alloc_stack $K
  // CHECK: copy_addr [[B_ELT_1_1_REF]] to [init] [[K]] : $*K
  // CHECK: [[C_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 2
  // CHECK: [[C_VAL:%.*]] = load [copy] [[C_REF]] : $*Dictionary<K, V>
  // CHECK: [[D_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>), 3
  // CHECK: [[D_VAL:%.*]] = alloc_stack $Optional<V>
  // CHECK: copy_addr [[D_REF]] to [init] [[D_VAL]] : $*Optional<V>
  // CHECK: end_access [[LOCAL_STORAGE_ACCESS]] : $*(a: Int, b: (String, (Int, K)), c: Dictionary<K, V>, d: Optional<V>)
  // CHECK: [[STORAGE_METATYPE:%.*]] = metatype $@thin TupleFlatteningTest<K, V>.$Storage.Type
  // CHECK: {{.*}} = apply [[STORAGE_INIT_REF]]<K, V>([[STORAGE_INST:%.*]], [[A]], [[B_ELT_0_VAL]], [[B_ELT_1_0_VAL]], [[K]], [[C_VAL]], [[D_VAL]], [[STORAGE_METATYPE]]) : $@convention(method) <τ_0_0, τ_0_1 where τ_0_0 : Collection, τ_0_0 : Hashable> (Int, @owned String, Int, @in τ_0_0, @owned Dictionary<τ_0_0, τ_0_1>, @in Optional<τ_0_1>, @thin TupleFlatteningTest<τ_0_0, τ_0_1>.$Storage.Type) -> @out TupleFlatteningTest<τ_0_0, τ_0_1>.$Storage
  // CHECK: dealloc_stack [[D_VAL]] : $*Optional<V>
  // CHECK: dealloc_stack [[K]] : $*K
  init(a: Int, b: (String, (Int, K)), c: [K: V], d: V?) {
    self.a = a
    self.b = b
    self.c = c
    self.d = d
  }
}

// Make sure that all the branches have `self.$storage` initialized.

@Wrapper
class TestConditionalInjection<T> {
  var b: T?

  // CHECK-LABEL: sil hidden [ossa] @$s4test24TestConditionalInjectionC4cond1vACyxGSb_xSgtcfc

  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack [lexical] $(b: Optional<T>), var, name "_storage", implicit

  // CHECK: [[COND:%.*]] = struct_extract [[COND_ARG:%.*]] : $Bool, #Bool._value
  // CHECK-NEXT: cond_br [[COND]], [[COND_TRUE:bb[0-9]+]], [[COND_FALSE:bb[0-9]+]]

  // CHECK: [[COND_TRUE]]:
  // CHECK: copy_addr [[V_ARG:%.*]] to [init] [[V:%.*]] : $*Optional<T>
  // CHECK: switch_enum_addr [[V]] : $*Optional<T>, case #Optional.some!enumelt: [[V_SOME:bb[0-9]+]], case #Optional.none!enumelt: [[V_NONE:bb[0-9]+]]

  // CHECK: [[V_NONE]]:
  // CHECK-NEXT: destroy_addr [[V]] : $*Optional<T>
  // CHECK-NEXT: dealloc_stack [[V]] : $*Optional<T>
  // CHECK: br [[STORAGE_INIT_NIL:bb[0-9]+]]


  // CHECK: [[V_SOME]]:
  // CHECK: [[V_VAL:%.*]] = unchecked_take_enum_data_addr [[V]] : $*Optional<T>, #Optional.some!enumelt
  // CHECK: copy_addr [take] [[V_VAL:%.*]] to [init] [[T:%.*]] : $*T
  // CHECK: br [[STORAGE_INIT_SOME:bb[0-9]+]]

  // CHECK: [[STORAGE_INIT_SOME]]:
  // CHECK: [[B_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(b: Optional<T>), 0
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $*Optional<T> to [init] [[B_PROP]] : $*Optional<T>, set {{.*}} : $@callee_guaranteed (@in Optional<T>) -> ()
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test24TestConditionalInjectionC8$StorageV1bAEyx_GxSg_tcfC
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE:%.*]] : $*(b: Optional<T>)
  // CHECK: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>), 0
  // CHECK: [[B_VAL:%.*]] = alloc_stack $Optional<T>
  // CHECK: copy_addr [[B_REF]] to [init] [[B_VAL]] : $*Optional<T>
  // CHECK: end_access [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>)
  // CHECK: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE_INST:%.*]], [[B_VAL]], {{.*}}) : $@convention(method) <τ_0_0> (@in Optional<τ_0_0>, @thin TestConditionalInjection<τ_0_0>.$Storage.Type) -> @out TestConditionalInjection<τ_0_0>.$Storage
  // CHECK: [[SELF_ACCESS:%.*]] = begin_borrow [[SELF:%.*]] : $TestConditionalInjection<T>
  // CHECK: [[STORAGE_PROP:%.*]] = ref_element_addr [[SELF_ACCESS]] : $TestConditionalInjection<T>, #TestConditionalInjection.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TestConditionalInjection<T>.Type
  // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF:%.*]]<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE_INST]], {{.*}})
  // CHECK: [[STORAGE_PROP_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_PROP]]
  // CHECK: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_PROP_ACCESS]]
  // CHECK: end_access [[STORAGE_PROP_ACCESS]]
  // CHECK: end_borrow [[SELF_ACCESS]]
  // CHECK: dealloc_stack [[B_VAL]]
  // CHECK: dealloc_stack [[STORAGE_INST]]

  // CHECK: [[STORAGE_INIT_NIL]]:
  // CHECK: [[B_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(b: Optional<T>), 0
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $*Optional<T> to [init] [[B_PROP]] : $*Optional<T>, set {{.*}} : $@callee_guaranteed (@in Optional<T>) -> ()
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test24TestConditionalInjectionC8$StorageV1bAEyx_GxSg_tcfC
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE:%.*]] : $*(b: Optional<T>)
  // CHECK: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>), 0
  // CHECK: [[B_VAL:%.*]] = alloc_stack $Optional<T>
  // CHECK: copy_addr [[B_REF]] to [init] [[B_VAL]] : $*Optional<T>
  // CHECK: end_access [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>)
  // CHECK: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE_INST:%.*]], [[B_VAL]], {{.*}}) : $@convention(method) <τ_0_0> (@in Optional<τ_0_0>, @thin TestConditionalInjection<τ_0_0>.$Storage.Type) -> @out TestConditionalInjection<τ_0_0>.$Storage
  // CHECK: [[SELF_ACCESS:%.*]] = begin_borrow [[SELF:%.*]] : $TestConditionalInjection<T>
  // CHECK: [[STORAGE_PROP:%.*]] = ref_element_addr [[SELF_ACCESS]] : $TestConditionalInjection<T>, #TestConditionalInjection.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TestConditionalInjection<T>.Type
  // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF:%.*]]<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE_INST]], {{.*}})
  // CHECK: [[STORAGE_PROP_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_PROP]]
  // CHECK: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_PROP_ACCESS]]
  // CHECK: end_access [[STORAGE_PROP_ACCESS]]
  // CHECK: end_borrow [[SELF_ACCESS]]
  // CHECK: dealloc_stack [[B_VAL]]
  // CHECK: dealloc_stack [[STORAGE_INST]]

  // CHECK: [[COND_FALSE]]:
  // CHECK-NEXT: br [[V_CHECK:bb[0-9]+]]

  // CHECK: [[V_CHECK]]:
  // CHECK: switch_enum_addr [[V:%.*]] : $*Optional<T>, case #Optional.some!enumelt: [[V_SOME:bb[0-9]+]], case #Optional.none!enumelt: [[V_NONE:bb[0-9]+]]

  // CHECK: [[V_NONE]]:
  // CHECK-NEXT: destroy_addr [[V]] : $*Optional<T>
  // CHECK-NEXT: dealloc_stack [[V]] : $*Optional<T>
  // CHECK: br [[STORAGE_INIT_NIL:bb[0-9]+]]


  // CHECK: [[V_SOME]]:
  // CHECK: [[V_VAL:%.*]] = unchecked_take_enum_data_addr [[V]] : $*Optional<T>, #Optional.some!enumelt
  // CHECK: copy_addr [take] [[V_VAL:%.*]] to [init] [[T:%.*]] : $*T
  // CHECK: br [[STORAGE_INIT_SOME:bb[0-9]+]]

  // CHECK: [[STORAGE_INIT_SOME]]:
  // CHECK: [[B_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(b: Optional<T>), 0
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $*Optional<T> to [init] [[B_PROP]] : $*Optional<T>, set {{.*}} : $@callee_guaranteed (@in Optional<T>) -> ()
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test24TestConditionalInjectionC8$StorageV1bAEyx_GxSg_tcfC
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE:%.*]] : $*(b: Optional<T>)
  // CHECK: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>), 0
  // CHECK: [[B_VAL:%.*]] = alloc_stack $Optional<T>
  // CHECK: copy_addr [[B_REF]] to [init] [[B_VAL]] : $*Optional<T>
  // CHECK: end_access [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>)
  // CHECK: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE_INST:%.*]], [[B_VAL]], {{.*}}) : $@convention(method) <τ_0_0> (@in Optional<τ_0_0>, @thin TestConditionalInjection<τ_0_0>.$Storage.Type) -> @out TestConditionalInjection<τ_0_0>.$Storage
  // CHECK: [[SELF_ACCESS:%.*]] = begin_borrow [[SELF:%.*]] : $TestConditionalInjection<T>
  // CHECK: [[STORAGE_PROP:%.*]] = ref_element_addr [[SELF_ACCESS]] : $TestConditionalInjection<T>, #TestConditionalInjection.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TestConditionalInjection<T>.Type
  // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF:%.*]]<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE_INST]], {{.*}})
  // CHECK: [[STORAGE_PROP_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_PROP]]
  // CHECK: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_PROP_ACCESS]]
  // CHECK: end_access [[STORAGE_PROP_ACCESS]]
  // CHECK: end_borrow [[SELF_ACCESS]]
  // CHECK: dealloc_stack [[B_VAL]]
  // CHECK: dealloc_stack [[STORAGE_INST]]

  // CHECK: [[STORAGE_INIT_NIL]]:
  // CHECK: [[B_PROP:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(b: Optional<T>), 0
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $*Optional<T> to [init] [[B_PROP]] : $*Optional<T>, set {{.*}} : $@callee_guaranteed (@in Optional<T>) -> ()
  // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test24TestConditionalInjectionC8$StorageV1bAEyx_GxSg_tcfC
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE:%.*]] : $*(b: Optional<T>)
  // CHECK: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>), 0
  // CHECK: [[B_VAL:%.*]] = alloc_stack $Optional<T>
  // CHECK: copy_addr [[B_REF]] to [init] [[B_VAL]] : $*Optional<T>
  // CHECK: end_access [[LOCAL_STORAGE_ACCESS]] : $*(b: Optional<T>)
  // CHECK: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE_INST:%.*]], [[B_VAL]], {{.*}}) : $@convention(method) <τ_0_0> (@in Optional<τ_0_0>, @thin TestConditionalInjection<τ_0_0>.$Storage.Type) -> @out TestConditionalInjection<τ_0_0>.$Storage
  // CHECK: [[SELF_ACCESS:%.*]] = begin_borrow [[SELF:%.*]] : $TestConditionalInjection<T>
  // CHECK: [[STORAGE_PROP:%.*]] = ref_element_addr [[SELF_ACCESS]] : $TestConditionalInjection<T>, #TestConditionalInjection.$storage
  // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TestConditionalInjection<T>.Type
  // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>
  // CHECK: {{.*}} = apply [[WRAPPER_INIT_REF:%.*]]<TestConditionalInjection<T>, TestConditionalInjection<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE_INST]], {{.*}})
  // CHECK: [[STORAGE_PROP_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_PROP]]
  // CHECK: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_PROP_ACCESS]]
  // CHECK: end_access [[STORAGE_PROP_ACCESS]]
  // CHECK: dealloc_stack [[B_VAL]]
  // CHECK: dealloc_stack [[STORAGE_INST]]

  public init(cond: Bool = false, v: T? = nil) {
    if cond {
      if let v {
        self.b = v
      } else {
        self.b = nil
      }
    } else {
      if let v {
        self.b = v
      } else {
        self.b = nil
      }
    }
  }
}

/// Make sure that convenience initializers don't get _storage variable injected and don't produce any assign_by_wrapper instructions
@Wrapper
class ClassWithConvenienceInit<T> {
  var a: T?
  var b: String

  init(a: T?, b: String) {
    self.a = a
    self.b = b
  }

  // CHECK-LABEL: sil hidden [ossa] @$s4test24ClassWithConvenienceInitC1aACyxGxSg_tcfC
  // CHECK-NOT: {{.*}} alloc_stack [lexical] $(b: Optional<T>), var, name "_storage", implicit
  // CHECK-NOT: assign_by_wrapper {{.*}}

  convenience init(a: T? = nil) {
    self.init(a: a, b: "<placeholder>")

    // CHECK: [[A_GETTER:%.*]] = class_method [[SELF:%.*]] : $ClassWithConvenienceInit<T>, #ClassWithConvenienceInit.a!getter
    // CHECK-NEXT: {{.*}} = apply [[A_GETTER]]<T>({{.*}})
    _ = self.a
    // CHECK: [[B_GETTER:%.*]] = class_method [[SELF:%.*]] : $ClassWithConvenienceInit<T>, #ClassWithConvenienceInit.b!getter
    // CHECK-NEXT: {{.*}} = apply [[B_GETTER]]<T>({{.*}})
    _ = self.b

    // CHECK: [[A_SETTER:%.*]] = class_method [[SELF:%.*]] : $ClassWithConvenienceInit<T>, #ClassWithConvenienceInit.a!setter
    // CHECK-NEXT: {{.*}} = apply [[A_SETTER]]<T>({{.*}})
    self.a = a

    if let a {
      // CHECK: [[B_SETTER:%.*]] = class_method [[SELF:%.*]] : $ClassWithConvenienceInit<T>, #ClassWithConvenienceInit.b!setter
      // CHECK-NEXT: {{.*}} = apply [[B_SETTER]]<T>({{.*}})
      self.b = "ultimate question"
    }
  }
}

@Wrapper
struct TypeWithLetProperties<T> {
  let a: T
  let b: Int

  // CHECK-LABEL: sil hidden [ossa] @$s4test21TypeWithLetPropertiesV1a1b5onSetACyxGx_SiSgyycSgtcfC
  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack [lexical] $(a: T, b: Int), var, name "_storage", implicit
  public init(a: T, b: Int? = nil, onSet: (() -> Void)? = nil) {
    // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [modify] [static] [[LOCAL_STORAGE]] : $*(a: T, b: Int)
    // CHECK-NEXT: [[A_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 0
    // CHECK-NEXT: copy_addr [take] %11 to [init] [[A_REF]] : $*T
    // CHECK-NOT: {{.*}} = assign_by_wrapper {{.*}}
    // CHECK-NEXT: end_access [[LOCAL_STORAGE_ACCESS]]
    self.a = a
    if let b {
      // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [modify] [static] [[LOCAL_STORAGE]] : $*(a: T, b: Int)
      // CHECK-NEXT: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 1
      // CHECK-NEXT: assign {{.*}} to [init] [[B_REF]] : $*Int
      // CHECK-NOT: {{.*}} = assign_by_wrapper {{.*}}
      // CHECK-NEXT: end_access [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int)

      // CHECK: [[STORAGE:%.*]] = alloc_stack $TypeWithLetProperties<T>.$Storage
      // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test21TypeWithLetPropertiesV8$StorageV1a1bAEyx_Gx_SitcfC

      // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE]] : $*(a: T, b: Int)
      // CHECK-NEXT: [[A_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 0
      // CHECK-NEXT: [[T:%.*]] = alloc_stack $T
      // CHECK-NEXT: copy_addr [[A_REF]] to [init] [[T]] : $*T
      // CHECK-NEXT: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 1
      // CHECK-NEXT: [[B_VAL:%.*]] = load [trivial] [[B_REF]] : $*Int
      // CHECK-NEXT: end_access [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int)

      // CHECK-NEXT: [[STORAGE_METATYPE:%.*]] = metatype $@thin TypeWithLetProperties<T>.$Storage.Type
      // CHECK-NEXT: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE]], [[T]], [[B_VAL]], [[STORAGE_METATYPE]])

      // CHECK: [[WRAPPER_INIT_REF:%.*]] = function_ref @$s4test7WrapperV3for7storageACyxq_Gxm_q_tcfC
      // CHECK: [[STORAGE_PROP:%.*]] = struct_element_addr [[SELF_ACCESS:%.*]] : $*TypeWithLetProperties<T>, #TypeWithLetProperties.$storage
      // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TypeWithLetProperties<T>.Type
      // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TypeWithLetProperties<T>, TypeWithLetProperties<T>.$Storage>
      // CHECK-NEXT: {{.*}} = apply [[WRAPPER_INIT_REF]]<TypeWithLetProperties<T>, TypeWithLetProperties<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE]], [[WRAPPER_METATYPE:%.*]])

      // CHECK: [[STORAGE_PROP_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_PROP]]
      // CHECK-NEXT: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_PROP_ACCESS]]
      // CHECK-NEXT: end_access [[STORAGE_PROP_ACCESS]]
      self.b = b
    } else {
      // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [modify] [static] [[LOCAL_STORAGE]] : $*(a: T, b: Int)
      // CHECK-NEXT: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 1
      // CHECK-NEXT: assign {{.*}} to [init] [[B_REF]] : $*Int
      // CHECK-NOT: {{.*}} = assign_by_wrapper {{.*}}
      // CHECK-NEXT: end_access [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int)

      // CHECK: [[STORAGE:%.*]] = alloc_stack $TypeWithLetProperties<T>.$Storage
      // CHECK: [[STORAGE_INIT_REF:%.*]] = function_ref @$s4test21TypeWithLetPropertiesV8$StorageV1a1bAEyx_Gx_SitcfC

      // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [read] [unsafe] [[LOCAL_STORAGE]] : $*(a: T, b: Int)
      // CHECK-NEXT: [[A_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 0
      // CHECK-NEXT: [[T:%.*]] = alloc_stack $T
      // CHECK-NEXT: copy_addr [[A_REF]] to [init] [[T]] : $*T
      // CHECK-NEXT: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int), 1
      // CHECK-NEXT: [[B_VAL:%.*]] = load [trivial] [[B_REF]] : $*Int
      // CHECK-NEXT: end_access [[LOCAL_STORAGE_ACCESS]] : $*(a: T, b: Int)

      // CHECK-NEXT: [[STORAGE_METATYPE:%.*]] = metatype $@thin TypeWithLetProperties<T>.$Storage.Type
      // CHECK-NEXT: {{.*}} = apply [[STORAGE_INIT_REF]]<T>([[STORAGE]], [[T]], [[B_VAL]], [[STORAGE_METATYPE]])

      // CHECK: [[WRAPPER_INIT_REF:%.*]] = function_ref @$s4test7WrapperV3for7storageACyxq_Gxm_q_tcfC
      // CHECK: [[STORAGE_PROP:%.*]] = struct_element_addr [[SELF_ACCESS:%.*]] : $*TypeWithLetProperties<T>, #TypeWithLetProperties.$storage
      // CHECK: [[WRAPPED_METATYPE:%.*]] = metatype $@thick TypeWithLetProperties<T>.Type
      // CHECK: [[WRAPPER_INST:%.*]] = alloc_stack $Wrapper<TypeWithLetProperties<T>, TypeWithLetProperties<T>.$Storage>
      // CHECK-NEXT: {{.*}} = apply [[WRAPPER_INIT_REF]]<TypeWithLetProperties<T>, TypeWithLetProperties<T>.$Storage>([[WRAPPER_INST]], [[WRAPPED_METATYPE]], [[STORAGE]], [[WRAPPER_METATYPE:%.*]])

      // CHECK: [[STORAGE_PROP_ACCESS:%.*]] = begin_access [modify] [dynamic] [[STORAGE_PROP]]
      // CHECK-NEXT: copy_addr [take] [[WRAPPER_INST]] to [init] [[STORAGE_PROP_ACCESS]]
      // CHECK-NEXT: end_access [[STORAGE_PROP_ACCESS]]
      self.b = 0
    }
  }
}

@Wrapper
struct TypeWithDefaultedProperties<T> {
  let a: [String]
  let b: T? = nil
  var c: Int = 42

  // CHECK-LABEL: sil hidden [ossa] @$s4test27TypeWithDefaultedPropertiesV1aACyxGSaySSG_tcfC
  // --> Defaults handling
  // CHECK: [[LOCAL_STORAGE:%.*]] = alloc_stack [lexical] $(a: Array<String>, b: Optional<T>, c: Int), var, name "_storage", implicit
  // CHECK-NEXT: [[A_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Array<String>, b: Optional<T>, c: Int), 0
  // CHECK-NEXT: [[B_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Array<String>, b: Optional<T>, c: Int), 1
  // CHECK-NEXT: [[C_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Array<String>, b: Optional<T>, c: Int), 2
  // CHECK-NEXT: inject_enum_addr [[B_REF]] : $*Optional<T>, #Optional.none!enumelt
  // CHECK-NEXT: [[C_DEFAULT_VAL:%.*]] = integer_literal $Builtin.IntLiteral, 42
  // CHECK: [[INT_INIT_REF:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK-NEXT: [[C_VAL:%.*]] = apply [[INT_INIT_REF]]([[C_DEFAULT_VAL]], {{.*}}) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
  // CHECK-NEXT: store [[C_VAL:%.*]] to [trivial] [[C_REF]] : $*Int
  // --> Assignment to `let a`
  // CHECK: [[LOCAL_STORAGE_ACCESS:%.*]] = begin_access [modify] [static] [[LOCAL_STORAGE]] : $*(a: Array<String>, b: Optional<T>, c: Int)
  // CHECK-NEXT: [[A_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE_ACCESS]] : $*(a: Array<String>, b: Optional<T>, c: Int), 0
  // CHECK-NEXT: assign [[A_ARG:%.*]] to [init] %18 : $*Array<String>
  // CHECK-NEXT: end_access [[LOCAL_STORAGE_ACCESS]]
  // --> Assignment to `var c`, note that the assignment is done to a wrapped value
  // CHECK: [[C_REF:%.*]] = tuple_element_addr [[LOCAL_STORAGE]] : $*(a: Array<String>, b: Optional<T>, c: Int), 2
  // CHECK: assign_by_wrapper origin type_wrapper, {{.*}} : $Int to [assign_wrapped_value] [[C_REF]] : $*Int, set {{.*}}
  init(a: [String]) {
    self.a = a
    self.c = 0
  }
}
