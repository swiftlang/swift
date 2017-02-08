// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

// REQUIRES: CPU=x86_64
// XFAIL: linux

protocol ClassBound : class {
  func classBoundMethod()
}
protocol ClassBound2 : class {
  func classBoundMethod2()
}
protocol ClassBoundBinary : class, ClassBound {
  func classBoundBinaryMethod(_ x: Self)
}
@objc protocol ObjCClassBound {
  func objCClassBoundMethod()
}
@objc protocol ObjCClassBound2 {
  func objCClassBoundMethod2()
}
protocol NotClassBound {
  func notClassBoundMethod()
  func notClassBoundBinaryMethod(_ x: Self)
}

struct ClassGenericFieldStruct<T:ClassBound> {
  var x : Int
  var y : T
  var z : Int
}

struct ClassProtocolFieldStruct {
  var x : Int
  var y : ClassBound
  var z : Int
}

class ClassGenericFieldClass<T:ClassBound> {
  final var x : Int = 0
  final var y : T
  final var z : Int = 0

  init(t: T) {
    y = t
  }
}

class ClassProtocolFieldClass {
  var x : Int = 0
  var y : ClassBound
  var z : Int = 0

  init(classBound cb: ClassBound) {
    y = cb
  }
}

// CHECK: %C22class_bounded_generics22ClassGenericFieldClass = type <{ %swift.refcounted, %Si, %objc_object*, %Si }>
// CHECK: %V22class_bounded_generics23ClassGenericFieldStruct = type <{ %Si, %objc_object*, %Si }>
// CHECK: %V22class_bounded_generics24ClassProtocolFieldStruct = type <{ %Si, %P22class_bounded_generics10ClassBound_, %Si }>

// CHECK-LABEL: define hidden %objc_object* @_T022class_bounded_generics0a1_B10_archetype{{[_0-9a-zA-Z]*}}F(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype<T : ClassBound>(_ x: T) -> T {
  return x
}

class SomeClass {}
class SomeSubclass : SomeClass {}

// CHECK-LABEL: define hidden %C22class_bounded_generics9SomeClass* @_T022class_bounded_generics011superclass_B10_archetype{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics9SomeClass*, %swift.type* %T)
func superclass_bounded_archetype<T : SomeClass>(_ x: T) -> T {
  return x
}

// CHECK-LABEL: define hidden { %C22class_bounded_generics9SomeClass*, %C22class_bounded_generics12SomeSubclass* } @_T022class_bounded_generics011superclass_B15_archetype_call{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics9SomeClass*, %C22class_bounded_generics12SomeSubclass*)
func superclass_bounded_archetype_call(_ x: SomeClass, y: SomeSubclass) -> (SomeClass, SomeSubclass) {
  return (superclass_bounded_archetype(x),
          superclass_bounded_archetype(y));
  // CHECK: [[SOMECLASS_RESULT:%.*]] = call %C22class_bounded_generics9SomeClass* @_T022class_bounded_generics011superclass_B10_archetype{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics9SomeClass* {{%.*}}, {{.*}})
  // CHECK: [[SOMESUPERCLASS_IN:%.*]] = bitcast %C22class_bounded_generics12SomeSubclass* {{%.*}} to %C22class_bounded_generics9SomeClass*
  // CHECK: [[SOMESUPERCLASS_RESULT:%.*]] = call %C22class_bounded_generics9SomeClass* @_T022class_bounded_generics011superclass_B10_archetype{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics9SomeClass* [[SOMESUPERCLASS_IN]], {{.*}})
  // CHECK: bitcast %C22class_bounded_generics9SomeClass* [[SOMESUPERCLASS_RESULT]] to %C22class_bounded_generics12SomeSubclass*
}

// CHECK-LABEL: define hidden void @_T022class_bounded_generics0a1_B17_archetype_method{{[_0-9a-zA-Z]*}}F(%objc_object*, %objc_object*, %swift.type* %T, i8** %T.ClassBoundBinary)
func class_bounded_archetype_method<T : ClassBoundBinary>(_ x: T, y: T) {
  x.classBoundMethod()
  // CHECK: [[INHERITED:%.*]] = load i8*, i8** %T.ClassBoundBinary, align 8
  // CHECK: [[INHERITED_WTBL:%.*]] = bitcast i8* [[INHERITED]] to i8**
  // CHECK: [[WITNESS:%.*]] = load i8*, i8** [[INHERITED_WTBL]], align 8
  // CHECK: [[WITNESS_FUNC:%.*]] = bitcast i8* [[WITNESS]] to void (%objc_object*, %swift.type*, i8**)
  // CHECK: call void [[WITNESS_FUNC]](%objc_object* %0, %swift.type* {{.*}}, i8** [[INHERITED_WTBL]])
  x.classBoundBinaryMethod(y)
  // CHECK: [[WITNESS_ENTRY:%.*]] = getelementptr inbounds i8*, i8** %T.ClassBoundBinary, i32 1
  // CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_ENTRY]], align 8
  // CHECK: call void @swift_unknownRetain(%objc_object* [[Y:%.*]])
  // CHECK: [[WITNESS_FUNC:%.*]] = bitcast i8* [[WITNESS]] to void (%objc_object*, %objc_object*, %swift.type*, i8**)
  // CHECK: call void [[WITNESS_FUNC]](%objc_object* [[Y]], %objc_object* %0, %swift.type* %T, i8** %T.ClassBoundBinary)
}

// CHECK-LABEL: define hidden { %objc_object*, %objc_object* } @_T022class_bounded_generics0a1_B16_archetype_tuple{{[_0-9a-zA-Z]*}}F(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype_tuple<T : ClassBound>(_ x: T) -> (T, T) {
  return (x, x)
}

class ConcreteClass : ClassBoundBinary, NotClassBound {
  func classBoundMethod() {}
  func classBoundBinaryMethod(_ x: ConcreteClass) {}
  func notClassBoundMethod() {}
  func notClassBoundBinaryMethod(_ x: ConcreteClass) {}
}

// CHECK-LABEL: define hidden %C22class_bounded_generics13ConcreteClass* @_T022class_bounded_generics05call_a1_B10_archetype{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics13ConcreteClass*) {{.*}} {
func call_class_bounded_archetype(_ x: ConcreteClass) -> ConcreteClass {
  return class_bounded_archetype(x)
  // CHECK: [[IN:%.*]] = bitcast %C22class_bounded_generics13ConcreteClass* {{%.*}} to %objc_object*
  // CHECK: [[OUT_ORIG:%.*]] = call %objc_object* @_T022class_bounded_generics0a1_B10_archetype{{[_0-9a-zA-Z]*}}F(%objc_object* [[IN]], {{.*}})
  // CHECK: [[OUT:%.*]] = bitcast %objc_object* [[OUT_ORIG]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK: define hidden void @_T022class_bounded_generics04not_a1_B10_archetype{{[_0-9a-zA-Z]*}}F(%swift.opaque* noalias nocapture sret, %swift.opaque* noalias nocapture, %swift.type* %T, i8** %T.NotClassBound)
func not_class_bounded_archetype<T : NotClassBound>(_ x: T) -> T {
  return x
}

// CHECK-LABEL: define hidden %objc_object* @_T022class_bounded_generics0a1_b18_archetype_to_not_a1_B0{{[_0-9a-zA-Z]*}}F(%objc_object*, %swift.type* %T, i8** %T.ClassBound, i8** %T.NotClassBound) {{.*}} {
func class_bounded_archetype_to_not_class_bounded
<T: ClassBound & NotClassBound>(_ x:T) -> T {
  // CHECK: alloca %objc_object*, align 8
  return not_class_bounded_archetype(x)
}

/* TODO Abstraction remapping to non-class-bounded witnesses
func class_and_not_class_bounded_archetype_methods
<T: ClassBound & NotClassBound>(_ x:T, y:T) {
  x.classBoundMethod()
  x.classBoundBinaryMethod(y)
  x.notClassBoundMethod()
  x.notClassBoundBinaryMethod(y)
}
*/

// CHECK-LABEL: define hidden { %objc_object*, i8** } @_T022class_bounded_generics0a1_B8_erasure{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics13ConcreteClass*) {{.*}} {
func class_bounded_erasure(_ x: ConcreteClass) -> ClassBound {
  return x
  // CHECK: [[INSTANCE_OPAQUE:%.*]] = bitcast %C22class_bounded_generics13ConcreteClass* [[INSTANCE:%.*]] to %objc_object*
  // CHECK: [[T0:%.*]] = insertvalue { %objc_object*, i8** } undef, %objc_object* [[INSTANCE_OPAQUE]], 0
  // CHECK: [[T1:%.*]] = insertvalue { %objc_object*, i8** } [[T0]], i8** getelementptr inbounds ([1 x i8*], [1 x i8*]* @_T022class_bounded_generics13ConcreteClassCAA0E5BoundAAWP, i32 0, i32 0), 1
  // CHECK: ret { %objc_object*, i8** } [[T1]]
}

// CHECK-LABEL: define hidden void @_T022class_bounded_generics0a1_B16_protocol_method{{[_0-9a-zA-Z]*}}F(%objc_object*, i8**) {{.*}} {
func class_bounded_protocol_method(_ x: ClassBound) {
  x.classBoundMethod()
  // CHECK: [[METADATA:%.*]] = call %swift.type* @swift_getObjectType(%objc_object* %0)
  // CHECK: [[WITNESS:%.*]] = load i8*, i8** [[WITNESS_TABLE:%.*]], align 8
  // CHECK: [[WITNESS_FN:%.*]] = bitcast i8* [[WITNESS]] to void (%objc_object*, %swift.type*, i8**)
  // CHECK: call void [[WITNESS_FN]](%objc_object* %0, %swift.type* [[METADATA]], i8** [[WITNESS_TABLE]])
}

// CHECK-LABEL: define hidden %C22class_bounded_generics13ConcreteClass* @_T022class_bounded_generics0a1_B15_archetype_cast{{[_0-9a-zA-Z]*}}F(%objc_object*, %swift.type* %T, i8** %T.ClassBound)
func class_bounded_archetype_cast<T : ClassBound>(_ x: T) -> ConcreteClass {
  return x as! ConcreteClass
  // CHECK: [[IN_PTR:%.*]] = bitcast %objc_object* {{%.*}} to i8*
  // CHECK: [[T0:%.*]] = call %swift.type* @_T022class_bounded_generics13ConcreteClassCMa()
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to i8*
  // CHECK: [[OUT_PTR:%.*]] = call i8* @swift_dynamicCastClassUnconditional(i8* [[IN_PTR]], i8* [[T1]])
  // CHECK: [[OUT:%.*]] = bitcast i8* [[OUT_PTR]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK-LABEL: define hidden %C22class_bounded_generics13ConcreteClass* @_T022class_bounded_generics0a1_B14_protocol_cast{{[_0-9a-zA-Z]*}}F(%objc_object*, i8**)
func class_bounded_protocol_cast(_ x: ClassBound) -> ConcreteClass {
  return x as! ConcreteClass
  // CHECK: [[IN_PTR:%.*]] = bitcast %objc_object* {{%.*}} to i8*
  // CHECK: [[T0:%.*]] = call %swift.type* @_T022class_bounded_generics13ConcreteClassCMa()
  // CHECK: [[T1:%.*]] = bitcast %swift.type* [[T0]] to i8*
  // CHECK: [[OUT_PTR:%.*]] = call i8* @swift_dynamicCastClassUnconditional(i8* [[IN_PTR]], i8* [[T1]])
  // CHECK: [[OUT:%.*]] = bitcast i8* [[OUT_PTR]] to %C22class_bounded_generics13ConcreteClass*
  // CHECK: ret %C22class_bounded_generics13ConcreteClass* [[OUT]]
}

// CHECK-LABEL: define hidden { %objc_object*, i8** } @_T022class_bounded_generics0a1_B22_protocol_conversion_{{.*}}(%objc_object*, i8**, i8**) {{.*}} {
func class_bounded_protocol_conversion_1(_ x: ClassBound & ClassBound2)
-> ClassBound {
  return x
}
// CHECK-LABEL: define hidden { %objc_object*, i8** } @_T022class_bounded_generics0a1_B22_protocol_conversion_{{.*}}(%objc_object*, i8**, i8**) {{.*}} {
func class_bounded_protocol_conversion_2(_ x: ClassBound & ClassBound2)
-> ClassBound2 {
  return x
}

// CHECK-LABEL: define hidden { %objc_object*, i8** } @_T022class_bounded_generics05objc_a1_B22_protocol_conversion_{{.*}}(%objc_object*, i8**) {{.*}} {
func objc_class_bounded_protocol_conversion_1
(_ x: ClassBound & ObjCClassBound) -> ClassBound {
  return x
}
// CHECK-LABEL: define hidden %objc_object* @_T022class_bounded_generics05objc_a1_B22_protocol_conversion_{{.*}}(%objc_object*, i8**) {{.*}} {
func objc_class_bounded_protocol_conversion_2
(_ x: ClassBound & ObjCClassBound) -> ObjCClassBound {
  return x
}
// CHECK-LABEL: define hidden %objc_object* @_T022class_bounded_generics05objc_a1_B22_protocol_conversion_{{.*}}(%objc_object*)
func objc_class_bounded_protocol_conversion_3
(_ x: ObjCClassBound & ObjCClassBound2) -> ObjCClassBound {
  return x
}
// CHECK-LABEL: define hidden %objc_object* @_T022class_bounded_generics05objc_a1_B22_protocol_conversion_{{.*}}(%objc_object*)
func objc_class_bounded_protocol_conversion_4
(_ x: ObjCClassBound & ObjCClassBound2) -> ObjCClassBound2 {
  return x
}

// CHECK-LABEL: define hidden { i64, %objc_object*, i64 } @_T022class_bounded_generics0A28_generic_field_struct_fields{{[_0-9a-zA-Z]*}}F(i64, %objc_object*, i64, %swift.type* %T, i8** %T.ClassBound)
func class_generic_field_struct_fields<T : ClassBound>
(_ x:ClassGenericFieldStruct<T>) -> (Int, T, Int) {
  return (x.x, x.y, x.z)
}

// CHECK-LABEL: define hidden void @_T022class_bounded_generics0A29_protocol_field_struct_fields{{[_0-9a-zA-Z]*}}F(<{ %Si, %P22class_bounded_generics10ClassBound_, %Si }>* noalias nocapture sret, %V22class_bounded_generics24ClassProtocolFieldStruct* noalias nocapture dereferenceable({{.*}}))
func class_protocol_field_struct_fields
(_ x:ClassProtocolFieldStruct) -> (Int, ClassBound, Int) {
  return (x.x, x.y, x.z)
}

// CHECK-LABEL: define hidden { i64, %objc_object*, i64 } @_T022class_bounded_generics0a15_generic_field_A7_fields{{[_0-9a-zA-Z]*}}F(%C22class_bounded_generics22ClassGenericFieldClass*)
func class_generic_field_class_fields<T : ClassBound>
(_ x:ClassGenericFieldClass<T>) -> (Int, T, Int) {
  return (x.x, x.y, x.z)
  // CHECK: getelementptr inbounds %C22class_bounded_generics22ClassGenericFieldClass, %C22class_bounded_generics22ClassGenericFieldClass* %0, i32 0, i32 1
  // CHECK: getelementptr inbounds %C22class_bounded_generics22ClassGenericFieldClass, %C22class_bounded_generics22ClassGenericFieldClass* %0, i32 0, i32 2
  // CHECK: getelementptr inbounds %C22class_bounded_generics22ClassGenericFieldClass, %C22class_bounded_generics22ClassGenericFieldClass* %0, i32 0, i32 3
}

// CHECK-LABEL: define hidden void @_T022class_bounded_generics0a16_protocol_field_A7_fields{{[_0-9a-zA-Z]*}}F(<{ %Si, %P22class_bounded_generics10ClassBound_, %Si }>* noalias nocapture sret, %C22class_bounded_generics23ClassProtocolFieldClass*)
func class_protocol_field_class_fields(_ x: ClassProtocolFieldClass)
-> (Int, ClassBound, Int) {
  return (x.x, x.y, x.z)
  // CHECK:  = call i64 %{{[0-9]+}}
  // CHECK:  = call { %objc_object*, i8** } %{{[0-9]+}}
  // CHECK:  = call i64 %{{[0-9]+}}
}

class SomeSwiftClass {
  class func foo() {}
}

// T must have a Swift layout, so we can load this metatype with a direct access.
// CHECK-LABEL: define hidden void @_T022class_bounded_generics0a1_B9_metatype{{[_0-9a-zA-Z]*}}F
// CHECK:      [[T0:%.*]] = getelementptr inbounds %C22class_bounded_generics14SomeSwiftClass, %C22class_bounded_generics14SomeSwiftClass* {{%.*}}, i32 0, i32 0, i32 0
// CHECK-NEXT: [[T1:%.*]] = load %swift.type*, %swift.type** [[T0]], align 8
// CHECK-NEXT: [[T2:%.*]] = bitcast %swift.type* [[T1]] to void (%swift.type*)**
// CHECK-NEXT: [[T3:%.*]] = getelementptr inbounds void (%swift.type*)*, void (%swift.type*)** [[T2]], i64 10
// CHECK-NEXT: load void (%swift.type*)*, void (%swift.type*)** [[T3]], align 8
func class_bounded_metatype<T: SomeSwiftClass>(_ t : T) {
  type(of: t).foo()
}

class WeakRef<T: AnyObject> {
  weak var value: T?
}

class A<T> {
  required init() {}
}

class M<T, S: A<T>> {
  private var s: S
  init() {
    // Don't crash generating the reference to 's'.
    s = S.init()
  }
}
