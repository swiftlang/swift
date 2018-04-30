
// RUN: %target-swift-emit-silgen -enable-sil-ownership %s | %FileCheck %s

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
  // CHECK: [[ROOT_COPY:%.*]] = copy_value %0
  // CHECK: store [[ROOT_COPY]] to [init] [[ROOT_TMP]]
  // CHECK: [[KP_COPY:%.*]] = copy_value %3
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  // CHECK: [[BORROWED_KP_COPY:%.*]] = begin_borrow [[KP_COPY]]
  // CHECK: [[RESULT_TMP:%.*]] = alloc_stack $B
  // CHECK: apply [[PROJECT]]<A, B>([[RESULT_TMP]], [[ROOT_TMP]], [[BORROWED_KP_COPY]])
  // CHECK: [[RESULT:%.*]] = load [take] [[RESULT_TMP]]
  // CHECK: destroy_value [[RESULT]]
  _ = readonly[keyPath: kp]
  _ = writable[keyPath: kp]
  _ = readonly[keyPath: wkp]

  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: wkp]

  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: rkp]
  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: rkp]

  // CHECK: function_ref @{{.*}}_projectKeyPathWritable
  writable[keyPath: wkp] = value
  // CHECK: function_ref @{{.*}}_projectKeyPathReferenceWritable
  readonly[keyPath: rkp] = value
  // CHECK: function_ref @{{.*}}_projectKeyPathReferenceWritable
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}addressOnly
func addressOnly(readonly: P, writable: inout P,
                 value: Q,
                 kp: KeyPath<P, Q>,
                 wkp: WritableKeyPath<P, Q>,
                 rkp: ReferenceWritableKeyPath<P, Q>) {
  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: kp]
  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: kp]
  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: wkp]

  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: wkp]

  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: rkp]
  // CHECK: function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: rkp]

  // CHECK: function_ref @{{.*}}_projectKeyPathWritable
  writable[keyPath: wkp] = value
  // CHECK: function_ref @{{.*}}_projectKeyPathReferenceWritable
  readonly[keyPath: rkp] = value
  // CHECK: function_ref @{{.*}}_projectKeyPathReferenceWritable
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}reabstracted
func reabstracted(readonly: @escaping () -> (),
                  writable: inout () -> (),
                  value: @escaping (A) -> B,
                  kp: KeyPath<() -> (), (A) -> B>,
                  wkp: WritableKeyPath<() -> (), (A) -> B>,
                  rkp: ReferenceWritableKeyPath<() -> (), (A) -> B>) {
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: kp]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: kp]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: wkp]

  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: wkp]

  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  _ = readonly[keyPath: rkp]
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReadOnly
  _ = writable[keyPath: rkp]

  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathWritable
  writable[keyPath: wkp] = value
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReferenceWritable
  readonly[keyPath: rkp] = value
  // CHECK: [[PROJECT:%.*]] = function_ref @{{.*}}_projectKeyPathReferenceWritable
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

