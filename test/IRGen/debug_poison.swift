// RUN: %target-swift-frontend -primary-file %s -emit-ir -Onone -enable-copy-propagation | %FileCheck %s -DINT=i%target-ptrsize
//
// This test is currently disabled because mandatory copy propagation
// is not part of the pipeline. It may be re-added to the pipeline,
// but it isn't clear if we'll still need to emit poison references by
// that time.
//
// REQUIRES: mandatory_copy_propagation

// Test debug_value [poison] emission

class K {
  init() {}
}

protocol P : AnyObject {}

class D : P {}

protocol Q {}

class E : Q {}

func useInt(_ i: Int) {}
func useAny(_: Any) {}
func useOptionalAny(_: Any?) {}
func useNone() {}
func getK() -> K { return K() }
func getOptionalK() -> K? { return K() }

private func useK(_: K) -> Int {
  return 2
}
private func useOptionalK(_: K?) -> Int {
  return 2
}

// Hoist and poison 'b' above useInt(y).
//
// CHECK-LABEL: define {{.*}}swiftcc void @"$s12debug_poison13testPoisonRefyyF"
// CHECK: %b.debug = alloca %T12debug_poison1KC*
// CHECK: %y.debug = alloca [[INT]]
// CHECK: [[REF:%.*]] = call {{.*}} %T12debug_poison1KC* @"$s12debug_poison4getKAA1KCyF"()
// CHECK: store %T12debug_poison1KC* [[REF]], %T12debug_poison1KC** %b.debug
// CHECK: [[Y:%.*]] = call {{.*}} [[INT]] @"$s12debug_poison4use{{.*}}"(%T12debug_poison1KC* [[REF]])
// CHECK: call void {{.*}} @swift_release {{.*}} [[REF]]
// CHECK: store %T12debug_poison1KC* inttoptr ([[INT]] 1088 to %T12debug_poison1KC*), %T12debug_poison1KC** %b.debug
// CHECK: store [[INT]] [[Y]], [[INT]]* %y.debug
// CHECK: call {{.*}} void @"$s12debug_poison6useIntyySiF"([[INT]] [[Y]])
public func testPoisonRef() {
  let b = getK()
  let y = useK(b)
  useInt(y)
}

// Hoist and poison 'b' above useInt(y).
//
// CHECK-LABEL: define {{.*}}swiftcc void @"$s12debug_poison21testPoisonOptionalRefyyF"
// CHECK: %b.debug = alloca [[INT]]
// CHECK: %y.debug = alloca [[INT]]
// CHECK: [[REF:%.*]] = call {{.*}} [[INT]] @"$s12debug_poison12getOptionalKAA1KCSgyF"()
// CHECK: store [[INT]] [[REF]], [[INT]]* %b.debug
// CHECK: [[Y:%.*]] = call {{.*}} [[INT]] @"$s12debug_poison12useOptionalK{{.*}}"([[INT]] [[REF]])
// CHECK: call void @swift_release
// CHECK: [[NIL:%.*]] = icmp eq [[INT]] [[REF]], 0
// CHECK: [[POISON:%.*]] = select i1 [[NIL]], [[INT]] [[REF]], [[INT]] 1088
// CHECK: store [[INT]] [[POISON]], [[INT]]* %b.debug
// CHECK: store [[INT]] [[Y]], [[INT]]* %y.debug
// CHECK: call {{.*}} void @"$s12debug_poison6useIntyySiF"([[INT]] [[Y]])
public func testPoisonOptionalRef() {
  let b: K? = getOptionalK()
  let y = useOptionalK(b)
  useInt(y)
}

// Hoist and poison 'b' above useNone.
//
// CHECK-LABEL: define {{.*}}swiftcc void @"$s12debug_poison21testPoisonExistentialyyF"
// CHECK: %b.debug = alloca %T12debug_poison1PP
// CHECK: [[INIT:%.*]] = call {{.*}} %T12debug_poison1DC* @"$s12debug_poison1DCACycfC"(
// CHECK: [[REF:%.*]] = bitcast %T12debug_poison1DC* [[INIT]] to %[[REFTY:.*]]*
// CHECK: [[GEP0:%.*]] = getelementptr inbounds %T12debug_poison1PP, %T12debug_poison1PP* %b.debug, i32 0, i32 0
// CHECK: store %[[REFTY]]* [[REF]], %[[REFTY]]** [[GEP0]]
// CHECK: store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s12debug_poison1DCAA1PAAWP", i32 0, i32 0), i8***
// CHECK: call %[[REFTY]]* @swift_{{unknownObjectRetain|retain}}(%[[REFTY]]* returned [[REF]])
// CHECK: store %[[REFTY]]* [[REF]], %[[REFTY]]**
// CHECK: call {{.*}} void @"$s12debug_poison6useAnyyyypF"(
// CHECK: call void @swift_{{unknownObjectRelease|release}}(%[[REFTY]]* [[REF]])
// CHECK: [[GEP1:%.*]] = getelementptr inbounds %T12debug_poison1PP, %T12debug_poison1PP* %b.debug, i32 0, i32 0
// CHECK: store %[[REFTY]]* inttoptr ([[INT]] 1088 to %[[REFTY]]*), %[[REFTY]]** [[GEP1]]
// CHECK: call {{.*}} void @"$s12debug_poison7useNoneyyF"()
public func testPoisonExistential() {
  let b: P = D()
  useAny(b)
  useNone()
}

// Hoist and poison 'b' above useNone.
//
// CHECK-LABEL: define {{.*}}swiftcc void @"$s12debug_poison19testPoisonCompositeyyF"()
// CHECK: %b.debug = alloca %T12debug_poison1Q_Xl
// CHECK: [[INIT:%.*]] = call {{.*}} %T12debug_poison1EC* @"$s12debug_poison1ECACycfC"(
// CHECK: [[REF:%.*]] = bitcast %T12debug_poison1EC* [[INIT]] to %[[REFTY]]*
// CHECK: [[GEP0:%.*]] = getelementptr inbounds %T12debug_poison1Q_Xl, %T12debug_poison1Q_Xl* %b.debug, i32 0, i32 0
// CHECK: store %[[REFTY]]* [[REF]], %[[REFTY]]** [[GEP0]]
// CHECK: store i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @"$s12debug_poison1ECAA1QAAWP", i32 0, i32 0), i8***
// CHECK: call %[[REFTY]]* @swift_{{unknownObjectRetain|retain}}(%[[REFTY]]* returned [[REF]])
// CHECK: store %[[REFTY]]* [[REF]], %[[REFTY]]**
// CHECK: call {{.*}} void @"$s12debug_poison6useAnyyyypF"(
// CHECK: call void @swift_{{unknownObjectRelease|release}}(%[[REFTY]]* [[REF]])
// CHECK: [[GEP1:%.*]] = getelementptr inbounds %T12debug_poison1Q_Xl, %T12debug_poison1Q_Xl* %b.debug, i32 0, i32 0
// CHECK: store %[[REFTY]]* inttoptr ([[INT]] 1088 to %[[REFTY]]*), %[[REFTY]]** [[GEP1]]
// CHECK: call {{.*}} void @"$s12debug_poison7useNoneyyF"()
public func testPoisonComposite() {
  let b: Q & AnyObject = E()
  useAny(b)
  useNone()
}

// CopyPropagation hoists 'b' above useNone, but IRGen currently bails on emitting poison.
//
// CHECK-LABEL: define {{.*}}swiftcc void @"$s12debug_poison27testPoisonOptionalCompositeyyF"()
// CHECK: %b.debug = alloca %T12debug_poison1Q_XlSg
// CHECK: [[INIT:%.*]] = call {{.*}} %T12debug_poison1EC* @"$s12debug_poison1ECACycfC"(
// CHECK: [[REF0:%.*]] = bitcast %T12debug_poison1EC* [[INIT]] to %[[REFTY]]*
// CHECK: [[REFINT0:%.*]] = ptrtoint %[[REFTY]]* [[REF0]] to [[INT]]
// CHECK: [[SHADOW0:%.*]] = bitcast %T12debug_poison1Q_XlSg* %b.debug to { [[INT]], [[INT]] }*
// CHECK: [[GEP0:%.*]] = getelementptr inbounds {{.*}} [[SHADOW0]], i32 0, i32 0
// CHECK: store [[INT]] [[REFINT0]], [[INT]]* [[GEP0]]
// CHECK: store [[INT]] ptrtoint ([1 x i8*]* @"$s12debug_poison1ECAA1QAAWP" to [[INT]]),
// CHECK: [[REF1:%.*]] = inttoptr [[INT]] [[REFINT0]] to %[[REFTY]]*
// CHECK: call %[[REFTY]]* @swift_{{unknownObjectRetain|retain}}(%[[REFTY]]* returned [[REF1]])
// CHECK: icmp eq [[INT]] [[REFINT0]], 0
// CHECK: [[PHI:%.*]] = phi %[[REFTY]]*
// CHECK: call void @swift_{{unknownObjectRelease|release}}(%[[REFTY]]*
// CHECK: store %[[REFTY]]* [[PHI]], %[[REFTY]]**
// CHECK: call {{.*}} void @"$s12debug_poison14useOptionalAnyyyypSgF"(
//
// Currently no poison store here.
// CHECK-NOT: store
// CHECK: call {{.*}} void @"$s12debug_poison7useNoneyyF"()
public func testPoisonOptionalComposite() {
  let b: Optional<Q & AnyObject> = E()
  useOptionalAny(b)
  useNone()
}
