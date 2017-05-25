// RUN: %target-swift-frontend -enable-experimental-keypath-components -emit-silgen %s | %FileCheck %s

class A {}
class B {}
protocol P {}
protocol Q {}

// CHECK-LABEL: sil hidden @{{.*}}loadable
func loadable(readonly: A, writable: inout A,
              value: B,
              kp: KeyPath<A, B>,
              wkp: WritableKeyPath<A, B>,
              rkp: ReferenceWritableKeyPath<A, B>) {
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $A
  // CHECK: [[ROOT_BORROW:%.*]] = begin_borrow %0
  // CHECK: [[ROOT_COPY:%.*]] = copy_value [[ROOT_BORROW]]
  // CHECK: store [[ROOT_COPY]] to [init] [[ROOT_TMP]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %3
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  // CHECK: [[RESULT_TMP:%.*]] = alloc_stack $B
  // CHECK: apply [[PROJECT]]<A, B>([[RESULT_TMP]], [[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[RESULT:%.*]] = load [take] [[RESULT_TMP]]
  // CHECK: destroy_value [[RESULT]]
  _ = readonly[keyPath: kp]
  _ = writable[keyPath: kp]
  _ = readonly[keyPath: wkp]

  // CHECK: [[TEMP:%.*]] = mark_uninitialized
  // CHECK: [[ROOT_ACCESS:%.*]] = begin_access [read] [unknown] %1 : $*A
  // CHECK: [[ROOT_RAW_PTR:%.*]] = address_to_pointer [[ROOT_ACCESS]]
  // CHECK: [[ROOT_PTR:%.*]] = struct $UnsafeMutablePointer<A> ([[ROOT_RAW_PTR]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %4
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathWritable
  // CHECK: [[PROJECTED:%.*]] = apply [[PROJECT]]<A, B>([[ROOT_PTR]], [[KP_COPY]])
  // CHECK: [[PROJECTED_BORROW:%.*]] = begin_borrow [[PROJECTED]]
  // CHECK: [[PROJECTED_PTR:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER_B:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER:%.*]] = copy_value [[PROJECTED_OWNER_B]]
  // CHECK: destroy_value [[PROJECTED]]
  // CHECK: [[PROJECTED_RAW_PTR:%.*]] = struct_extract [[PROJECTED_PTR]]
  // CHECK: [[PROJECTED_ADDR:%.*]] = pointer_to_address [[PROJECTED_RAW_PTR]]
  // CHECK: [[PROJECTED_DEP:%.*]] = mark_dependence [[PROJECTED_ADDR]] : $*B on [[PROJECTED_OWNER]]
  // CHECK: [[COPY_TMP:%.*]] = load [copy] [[PROJECTED_DEP]] : $*B
  // CHECK: assign [[COPY_TMP]] to [[TEMP]] : $*B
  // CHECK: destroy_value [[PROJECTED_OWNER]]
  _ = writable[keyPath: wkp]

  // CHECK: [[TEMP:%.*]] = mark_uninitialized
  // CHECK: [[ROOT_BORROW:%.*]] = begin_borrow %0
  // CHECK: [[ROOT_COPY:%.*]] = copy_value [[ROOT_BORROW]]
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $A
  // CHECK: store [[ROOT_COPY]] to [init] [[ROOT_TMP]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %5
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReferenceWritable
  // CHECK: [[PROJECTED:%.*]] = apply [[PROJECT]]<A, B>([[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[PROJECTED_BORROW:%.*]] = begin_borrow [[PROJECTED]]
  // CHECK: [[PROJECTED_PTR:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER_B:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER:%.*]] = copy_value [[PROJECTED_OWNER_B]]
  // CHECK: destroy_value [[PROJECTED]]
  // CHECK: [[PROJECTED_RAW_PTR:%.*]] = struct_extract [[PROJECTED_PTR]]
  // CHECK: [[PROJECTED_ADDR:%.*]] = pointer_to_address [[PROJECTED_RAW_PTR]]
  // CHECK: [[PROJECTED_DEP:%.*]] = mark_dependence [[PROJECTED_ADDR]] : $*B on [[PROJECTED_OWNER]]
  // CHECK: [[COPY_TMP:%.*]] = load [copy] [[PROJECTED_DEP]] : $*B
  // CHECK: assign [[COPY_TMP]] to [[TEMP]] : $*B
  // CHECK: destroy_value [[PROJECTED_OWNER]]
  _ = readonly[keyPath: rkp]
  _ = writable[keyPath: rkp]

  writable[keyPath: wkp] = value
  readonly[keyPath: rkp] = value
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}addressOnly
func addressOnly(readonly: P, writable: inout P,
                 value: Q,
                 kp: KeyPath<P, Q>,
                 wkp: WritableKeyPath<P, Q>,
                 rkp: ReferenceWritableKeyPath<P, Q>) {
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $P
  // CHECK: copy_addr %0 to [initialization] [[ROOT_TMP]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %3
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  // CHECK: [[RESULT:%.*]] = alloc_stack $Q
  // CHECK: apply [[PROJECT]]<P, Q>([[RESULT]], [[ROOT_TMP]], [[KP_COPY]])
  // CHECK: destroy_addr [[RESULT]]
  _ = readonly[keyPath: kp]
  _ = writable[keyPath: kp]
  _ = readonly[keyPath: wkp]

  // CHECK: [[TEMP:%.*]] = mark_uninitialized
  // CHECK: [[ROOT_ACCESS:%.*]] = begin_access [read] [unknown] %1 : $*P
  // CHECK: [[ROOT_RAW_PTR:%.*]] = address_to_pointer [[ROOT_ACCESS]]
  // CHECK: [[ROOT_PTR:%.*]] = struct $UnsafeMutablePointer<P> ([[ROOT_RAW_PTR]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %4
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathWritable
  // CHECK: [[PROJECTED:%.*]] = apply [[PROJECT]]<P, Q>([[ROOT_PTR]], [[KP_COPY]])
  // CHECK: [[PROJECTED_BORROW:%.*]] = begin_borrow [[PROJECTED]]
  // CHECK: [[PROJECTED_PTR:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER_B:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER:%.*]] = copy_value [[PROJECTED_OWNER_B]]
  // CHECK: destroy_value [[PROJECTED]]
  // CHECK: [[PROJECTED_RAW_PTR:%.*]] = struct_extract [[PROJECTED_PTR]]
  // CHECK: [[PROJECTED_ADDR:%.*]] = pointer_to_address [[PROJECTED_RAW_PTR]]
  // CHECK: [[PROJECTED_DEP:%.*]] = mark_dependence [[PROJECTED_ADDR]] : $*Q on [[PROJECTED_OWNER]]
  // CHECK: copy_addr [[PROJECTED_DEP]] to [initialization] [[CPY_TMP:%.*]] : $*Q
  // CHECK: copy_addr [take] [[CPY_TMP]] to [[TEMP]] : $*Q
  // CHECK: destroy_value [[PROJECTED_OWNER]]
  _ = writable[keyPath: wkp]

  // CHECK: [[TEMP:%.*]] = mark_uninitialized
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $P
  // CHECK: copy_addr %0 to [initialization] [[ROOT_TMP]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %5
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReferenceWritable
  // CHECK: [[PROJECTED:%.*]] = apply [[PROJECT]]<P, Q>([[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[PROJECTED_BORROW:%.*]] = begin_borrow [[PROJECTED]]
  // CHECK: [[PROJECTED_PTR:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER_B:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER:%.*]] = copy_value [[PROJECTED_OWNER_B]]
  // CHECK: destroy_value [[PROJECTED]]
  // CHECK: [[PROJECTED_RAW_PTR:%.*]] = struct_extract [[PROJECTED_PTR]]
  // CHECK: [[PROJECTED_ADDR:%.*]] = pointer_to_address [[PROJECTED_RAW_PTR]]
  // CHECK: [[PROJECTED_DEP:%.*]] = mark_dependence [[PROJECTED_ADDR]] : $*Q on [[PROJECTED_OWNER]]
  // CHECK: copy_addr [[PROJECTED_DEP]] to [initialization] [[CPY_TMP:%.*]] : $*Q
  // CHECK: copy_addr [take] [[CPY_TMP]] to [[TEMP]] : $*Q
  // CHECK: destroy_value [[PROJECTED_OWNER]]
  _ = readonly[keyPath: rkp]
  _ = writable[keyPath: rkp]

  writable[keyPath: wkp] = value
  readonly[keyPath: rkp] = value
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}reabstracted
func reabstracted(readonly: @escaping () -> (),
                  writable: inout () -> (),
                  value: @escaping (A) -> B,
                  kp: KeyPath<() -> (), (A) -> B>,
                  wkp: WritableKeyPath<() -> (), (A) -> B>,
                  rkp: ReferenceWritableKeyPath<() -> (), (A) -> B>) {
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $@callee_owned (@in ()) -> @out ()
  // CHECK: [[ROOT_BORROW:%.*]] = begin_borrow %0
  // CHECK: [[ROOT_COPY:%.*]] = copy_value [[ROOT_BORROW]]
  // CHECK: [[ROOT_ORIG:%.*]] = partial_apply {{.*}}([[ROOT_COPY]])
  // CHECK:  store [[ROOT_ORIG]] to [init] [[ROOT_TMP]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %3
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  // CHECK: [[RESULT_TMP:%.*]] = alloc_stack $@callee_owned (@in A) -> @out B
  // CHECK: apply [[PROJECT]]<() -> (), (A) -> B>([[RESULT_TMP]], [[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[RESULT_ORIG:%.*]] = load [take] [[RESULT_TMP]]
  // CHECK: [[RESULT_SUBST:%.*]] = partial_apply {{.*}}([[RESULT_ORIG]])
  // CHECK: destroy_value [[RESULT_SUBST]]
  _ = readonly[keyPath: kp]
  _ = writable[keyPath: kp]
  _ = readonly[keyPath: wkp]

  // CHECK: [[TEMP_SUBST:%.*]] = mark_uninitialized {{.*}} : $*@callee_owned (@owned A) -> @owned B
  // CHECK: [[ROOT_ACCESS:%.*]] = begin_access [read] [unknown] %1 : $*@callee_owned () -> ()
  // CHECK: [[ROOT_ORIG_TMP:%.*]] = alloc_stack $@callee_owned (@in ()) -> @out ()
  // CHECK: [[ROOT_SUBST:%.*]] = load [copy] [[ROOT_ACCESS]]
  // CHECK: [[ROOT_ORIG:%.*]] = partial_apply {{.*}}([[ROOT_SUBST]])
  // CHECK: store [[ROOT_ORIG]] to [init] [[ROOT_ORIG_TMP]]
  // CHECK: [[ROOT_RAW_PTR:%.*]] = address_to_pointer [[ROOT_ORIG_TMP]]
  // CHECK: [[ROOT_PTR:%.*]] = struct $UnsafeMutablePointer<() -> ()> ([[ROOT_RAW_PTR]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %4
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathWritable
  // CHECK: [[PROJECTED:%.*]] = apply [[PROJECT]]<() -> (), (A) -> B>([[ROOT_PTR]], [[KP_COPY]])
  // CHECK: [[PROJECTED_BORROW:%.*]] = begin_borrow [[PROJECTED]]
  // CHECK: [[PROJECTED_PTR:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER_B:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER:%.*]] = copy_value [[PROJECTED_OWNER_B]]
  // CHECK: destroy_value [[PROJECTED]]
  // CHECK: [[PROJECTED_RAW_PTR:%.*]] = struct_extract [[PROJECTED_PTR]]
  // CHECK: [[PROJECTED_ORIG_ADDR:%.*]] = pointer_to_address [[PROJECTED_RAW_PTR]] {{.*}} to [strict] $*@callee_owned (@in A) -> @out B
  // CHECK: [[PROJECTED_DEP:%.*]] = mark_dependence [[PROJECTED_ORIG_ADDR]] : $*@callee_owned (@in A) -> @out B on [[PROJECTED_OWNER]]
  // CHECK: [[PROJECTED_ORIG:%.*]] = load [copy] [[PROJECTED_DEP]]
  // CHECK: [[PROJECTED_SUBST:%.*]] = partial_apply {{.*}}([[PROJECTED_ORIG]])
  // CHECK: destroy_addr [[ROOT_ORIG_TMP]]
  // CHECK: assign [[PROJECTED_SUBST]] to [[TEMP_SUBST]]
  // CHECK: destroy_value [[PROJECTED_OWNER]]
  _ = writable[keyPath: wkp]

  // CHECK: [[TEMP:%.*]] = mark_uninitialized
  // CHECK: [[ROOT_BORROW:%.*]] = begin_borrow %0
  // CHECK: [[ROOT_COPY_SUBST:%.*]] = copy_value [[ROOT_BORROW]]
  // CHECK: [[ROOT_COPY_ORIG:%.*]] = partial_apply {{.*}}([[ROOT_COPY_SUBST]])
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $@callee_owned (@in ()) -> @out ()
  // CHECK: store [[ROOT_COPY_ORIG]] to [init] [[ROOT_TMP]]
  // CHECK: [[KP_BORROW:%.*]] = begin_borrow %5
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP_BORROW]]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReferenceWritable
  // CHECK: [[PROJECTED:%.*]] = apply [[PROJECT]]<() -> (), (A) -> B>([[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[PROJECTED_BORROW:%.*]] = begin_borrow [[PROJECTED]]
  // CHECK: [[PROJECTED_PTR:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER_B:%.*]] = tuple_extract [[PROJECTED_BORROW]]
  // CHECK: [[PROJECTED_OWNER:%.*]] = copy_value [[PROJECTED_OWNER_B]]
  // CHECK: destroy_value [[PROJECTED]]
  // CHECK: [[PROJECTED_RAW_PTR:%.*]] = struct_extract [[PROJECTED_PTR]]
  // CHECK: [[PROJECTED_ADDR_ORIG:%.*]] = pointer_to_address [[PROJECTED_RAW_PTR]] {{.*}} to [strict] $*@callee_owned (@in A) -> @out B
  // CHECK: [[PROJECTED_DEP:%.*]] = mark_dependence [[PROJECTED_ADDR_ORIG]] : $*@callee_owned (@in A) -> @out B on [[PROJECTED_OWNER]]
  // CHECK: [[PROJECTED_ORIG:%.*]] = load [copy] [[PROJECTED_DEP]]
  // CHECK: [[PROJECTED_SUBST:%.*]] = partial_apply {{.*}}([[PROJECTED_ORIG]])
  // CHECK: assign [[PROJECTED_SUBST]] to [[TEMP]]
  // CHECK: destroy_value [[PROJECTED_OWNER]]
  _ = readonly[keyPath: rkp]
  _ = writable[keyPath: rkp]

  writable[keyPath: wkp] = value
  readonly[keyPath: rkp] = value
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}partial
func partial<A>(valueA: A,
                valueB: Int,
                pkpA: PartialKeyPath<A>,
                pkpB: PartialKeyPath<Int>,
                akp: AnyKeyPath) {
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}projectKeyPathAny
  // CHECK: apply [[PROJECT]]<A>
  _ = valueA[keyPath: akp]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}projectKeyPathPartial
  // CHECK: apply [[PROJECT]]<A>
  _ = valueA[keyPath: pkpA]

  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}projectKeyPathAny
  // CHECK: apply [[PROJECT]]<Int>
  _ = valueB[keyPath: akp]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}projectKeyPathPartial
  // CHECK: apply [[PROJECT]]<Int>
  _ = valueB[keyPath: pkpB]
}
