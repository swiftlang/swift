
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
  // CHECK: [[ROOT_COPY:%.*]] = copy_value [[READONLY:%0]] :
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP:%3]]
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $A
  // CHECK: store [[ROOT_COPY]] to [init] [[ROOT_TMP]]
  // CHECK: [[GET:%.*]] = function_ref @swift_getAtKeyPath :
  // CHECK: [[RESULT_TMP:%.*]] = alloc_stack $B
  // CHECK: apply [[GET]]<A, B>([[RESULT_TMP]], [[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[RESULT:%.*]] = load [take] [[RESULT_TMP]]
  // CHECK: destroy_value [[RESULT]]
  _ = readonly[keyPath: kp]

  // CHECK: [[ACCESS:%.*]] = begin_access [read] [unknown] [[WRITABLE:%1]] :
  // CHECK: [[ROOT_COPY:%.*]] = load [copy] [[ACCESS]]
  // CHECK: end_access [[ACCESS]]
  // CHECK: [[KP_COPY:%.*]] = copy_value [[KP]]
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $A
  // CHECK: store [[ROOT_COPY]] to [init] [[ROOT_TMP]]
  // CHECK: [[GET:%.*]] = function_ref @swift_getAtKeyPath :
  // CHECK: [[RESULT_TMP:%.*]] = alloc_stack $B
  // CHECK: apply [[GET]]<A, B>([[RESULT_TMP]], [[ROOT_TMP]], [[KP_COPY]])
  // CHECK: [[RESULT:%.*]] = load [take] [[RESULT_TMP]]
  // CHECK: destroy_value [[RESULT]]
  _ = writable[keyPath: kp]

  // CHECK: [[ROOT_COPY:%.*]] = copy_value [[READONLY]] :
  // CHECK: [[KP_COPY:%.*]] = copy_value [[WKP:%4]]
  // CHECK: [[KP_UPCAST:%.*]] = upcast [[KP_COPY]] : $WritableKeyPath<A, B> to $KeyPath<A, B>
  // CHECK: [[ROOT_TMP:%.*]] = alloc_stack $A
  // CHECK: store [[ROOT_COPY]] to [init] [[ROOT_TMP]]
  // CHECK: [[GET:%.*]] = function_ref @swift_getAtKeyPath :
  // CHECK: [[RESULT_TMP:%.*]] = alloc_stack $B
  // CHECK: apply [[GET]]<A, B>([[RESULT_TMP]], [[ROOT_TMP]], [[KP_UPCAST]])
  // CHECK: [[RESULT:%.*]] = load [take] [[RESULT_TMP]]
  // CHECK: destroy_value [[RESULT]]
  _ = readonly[keyPath: wkp]

  // CHECK: function_ref @swift_getAtKeyPath
  _ = writable[keyPath: wkp]

  // CHECK: function_ref @swift_getAtKeyPath
  _ = readonly[keyPath: rkp]

  // CHECK: function_ref @swift_getAtKeyPath
  _ = writable[keyPath: rkp]


  // CHECK:      [[KP_COPY:%.*]] = copy_value [[WKP]]
  // CHECK-NEXT: [[VALUE_COPY:%.*]] = copy_value [[VALUE:%2]] : $B
  // CHECK-NEXT: [[ACCESS:%.*]] = begin_access [modify] [unknown] [[WRITABLE]] :
  // CHECK-NEXT: [[VALUE_TEMP:%.*]] = alloc_stack $B
  // CHECK-NEXT: store [[VALUE_COPY]] to [init] [[VALUE_TEMP]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[SET:%.*]] = function_ref @swift_setAtWritableKeyPath :
  // CHECK-NEXT: apply [[SET]]<A, B>([[ACCESS]], [[KP_COPY]], [[VALUE_TEMP]])
  // CHECK-NEXT: end_access [[ACCESS]]
  // CHECK-NEXT: dealloc_stack [[VALUE_TEMP]]
  // CHECK-NEXT: destroy_value [[KP_COPY]]
  writable[keyPath: wkp] = value

  // CHECK-NEXT: [[ROOT_COPY:%.*]] = copy_value [[READONLY]] :
  // CHECK-NEXT: [[KP_COPY:%.*]] = copy_value [[RKP:%5]]
  // CHECK-NEXT: [[VALUE_COPY:%.*]] = copy_value [[VALUE]] : $B
  // CHECK-NEXT: [[ROOT_TEMP:%.*]] = alloc_stack $A
  // CHECK-NEXT: store [[ROOT_COPY]] to [init] [[ROOT_TEMP]]
  // CHECK-NEXT: [[VALUE_TEMP:%.*]] = alloc_stack $B
  // CHECK-NEXT: store [[VALUE_COPY]] to [init] [[VALUE_TEMP]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[SET:%.*]] = function_ref @swift_setAtReferenceWritableKeyPath :
  // CHECK-NEXT: apply [[SET]]<A, B>([[ROOT_TEMP]], [[KP_COPY]], [[VALUE_TEMP]])
  // CHECK-NEXT: dealloc_stack [[VALUE_TEMP]]
  // CHECK-NEXT: destroy_addr [[ROOT_TEMP]]
  // CHECK-NEXT: dealloc_stack [[ROOT_TEMP]]
  // CHECK-NEXT: destroy_value [[KP_COPY]]
  readonly[keyPath: rkp] = value

  // CHECK-NEXT: [[ACCESS:%.*]] = begin_access [read] [unknown] [[WRITABLE]] :
  // CHECK-NEXT: [[ROOT_COPY:%.*]] = load [copy] [[ACCESS]] :
  // CHECK-NEXT: end_access [[ACCESS]]
  // CHECK-NEXT: [[KP_COPY:%.*]] = copy_value [[RKP:%5]]
  // CHECK-NEXT: [[VALUE_COPY:%.*]] = copy_value [[VALUE]] : $B
  // CHECK-NEXT: [[ROOT_TEMP:%.*]] = alloc_stack $A
  // CHECK-NEXT: store [[ROOT_COPY]] to [init] [[ROOT_TEMP]]
  // CHECK-NEXT: [[VALUE_TEMP:%.*]] = alloc_stack $B
  // CHECK-NEXT: store [[VALUE_COPY]] to [init] [[VALUE_TEMP]]
  // CHECK-NEXT: // function_ref
  // CHECK-NEXT: [[SET:%.*]] = function_ref @swift_setAtReferenceWritableKeyPath :
  // CHECK-NEXT: apply [[SET]]<A, B>([[ROOT_TEMP]], [[KP_COPY]], [[VALUE_TEMP]])
  // CHECK-NEXT: dealloc_stack [[VALUE_TEMP]]
  // CHECK-NEXT: destroy_addr [[ROOT_TEMP]]
  // CHECK-NEXT: dealloc_stack [[ROOT_TEMP]]
  // CHECK-NEXT: destroy_value [[KP_COPY]]
  writable[keyPath: rkp] = value
} // CHECK-LABEL: } // end sil function '{{.*}}loadable

// CHECK-LABEL: sil hidden @{{.*}}addressOnly
func addressOnly(readonly: P, writable: inout P,
                 value: Q,
                 kp: KeyPath<P, Q>,
                 wkp: WritableKeyPath<P, Q>,
                 rkp: ReferenceWritableKeyPath<P, Q>) {
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = readonly[keyPath: kp]
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = writable[keyPath: kp]
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = readonly[keyPath: wkp]

  // CHECK: function_ref @swift_getAtKeyPath :
  _ = writable[keyPath: wkp]

  // CHECK: function_ref @swift_getAtKeyPath :
  _ = readonly[keyPath: rkp]
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = writable[keyPath: rkp]

  // CHECK: function_ref @swift_setAtWritableKeyPath :
  writable[keyPath: wkp] = value
  // CHECK: function_ref @swift_setAtReferenceWritableKeyPath :
  readonly[keyPath: rkp] = value
  // CHECK: function_ref @swift_setAtReferenceWritableKeyPath :
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}reabstracted
func reabstracted(readonly: @escaping () -> (),
                  writable: inout () -> (),
                  value: @escaping (A) -> B,
                  kp: KeyPath<() -> (), (A) -> B>,
                  wkp: WritableKeyPath<() -> (), (A) -> B>,
                  rkp: ReferenceWritableKeyPath<() -> (), (A) -> B>) {
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = readonly[keyPath: kp]
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = writable[keyPath: kp]
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = readonly[keyPath: wkp]

  // CHECK: function_ref @swift_getAtKeyPath :
  _ = writable[keyPath: wkp]

  // CHECK: function_ref @swift_getAtKeyPath :
  _ = readonly[keyPath: rkp]
  // CHECK: function_ref @swift_getAtKeyPath :
  _ = writable[keyPath: rkp]

  // CHECK: function_ref @swift_setAtWritableKeyPath :
  writable[keyPath: wkp] = value
  // CHECK: function_ref @swift_setAtReferenceWritableKeyPath :
  readonly[keyPath: rkp] = value
  // CHECK: function_ref @swift_setAtReferenceWritableKeyPath :
  writable[keyPath: rkp] = value
}

// CHECK-LABEL: sil hidden @{{.*}}partial
func partial<A>(valueA: A,
                valueB: Int,
                pkpA: PartialKeyPath<A>,
                pkpB: PartialKeyPath<Int>,
                akp: AnyKeyPath) {
  // CHECK: [[PROJECT:%.*]] = function_ref @swift_getAtAnyKeyPath :
  // CHECK: apply [[PROJECT]]<A>
  _ = valueA[keyPath: akp]
  // CHECK: [[PROJECT:%.*]] = function_ref @swift_getAtPartialKeyPath :
  // CHECK: apply [[PROJECT]]<A>
  _ = valueA[keyPath: pkpA]

  // CHECK: [[PROJECT:%.*]] = function_ref @swift_getAtAnyKeyPath :
  // CHECK: apply [[PROJECT]]<Int>
  _ = valueB[keyPath: akp]
  // CHECK: [[PROJECT:%.*]] = function_ref @swift_getAtPartialKeyPath :
  // CHECK: apply [[PROJECT]]<Int>
  _ = valueB[keyPath: pkpB]
}

extension Int {
  var b: Int { get { return 0 } set { } }
  var u: Int { get { return 0 } set { } }
  var tt: Int { get { return 0 } set { } }
}

// CHECK-LABEL: sil hidden @{{.*}}writebackNesting
func writebackNesting(x: inout Int,
                      y: WritableKeyPath<Int, Int>,
                      z: WritableKeyPath<Int, Int>,
                      w: Int) -> Int {
  // -- get 'b'
  // CHECK: function_ref @$sSi19keypath_applicationE1bSivg
  // -- apply keypath y
  // CHECK: [[PROJECT_FN:%.*]] = function_ref @swift_modifyAtWritableKeyPath :
  // CHECK: ([[Y_ADDR:%.*]], [[Y_TOKEN:%.*]]) = begin_apply [[PROJECT_FN]]<Int, Int>
  // -- get 'u'
  // CHECK: function_ref @$sSi19keypath_applicationE1uSivg
  // -- apply keypath z
  // CHECK: [[PROJECT_FN:%.*]] = function_ref @swift_modifyAtWritableKeyPath :
  // CHECK: ([[Z_ADDR:%.*]], [[Z_TOKEN:%.*]]) = begin_apply [[PROJECT_FN]]<Int, Int>

  // -- set 'tt'
  // CHECK: function_ref @$sSi19keypath_applicationE2ttSivs
  // -- destroy owner for keypath projection z
  // CHECK: end_apply [[Z_TOKEN]]
  // -- set 'u'
  // CHECK: function_ref @$sSi19keypath_applicationE1uSivs
  // -- destroy owner for keypath projection y
  // CHECK: end_apply [[Y_TOKEN]]
  // -- set 'b'
  // CHECK: function_ref @$sSi19keypath_applicationE1bSivs

  x.b[keyPath: y].u[keyPath: z].tt = w
}
