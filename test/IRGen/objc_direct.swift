// RUN: %target-swift-emit-ir %use_no_opaque_pointers -import-objc-header %S/../Inputs/objc_direct.h -o - %s | %FileCheck %s
// RUN: %target-swift-emit-ir -import-objc-header %S/../Inputs/objc_direct.h -o - %s

// REQUIRES: objc_interop

func markUsed<T>(_ t: T) {}

protocol BarProtocol {
    func directProtocolMethod() -> String!
}

extension Bar: BarProtocol {}

let bar = Bar()
markUsed(Bar(value: 0))
// CHECK: call swiftcc {{i32|i64}} @"$sSo3BarC5valueABSgs5Int32V_tcfC"
markUsed(Bar.init(value: 0))
// CHECK: call swiftcc {{i32|i64}} @"$sSo3BarC5valueABSgs5Int32V_tcfC"

bar.directProperty = 123
// CHECK: call void @"\01-[Bar setDirectProperty:]"(%{{[0-9]+}}* %{{[0-9]+}}, i32 {{.*}})

markUsed(bar.directProperty)
// CHECK: call i32 @"\01-[Bar directProperty]"(%{{[0-9]+}}* %{{[0-9]+}})

bar.directProperty2 = 456
// CHECK: call void @"\01-[Bar setDirectProperty2:]"(%{{[0-9]+}}* %{{[0-9]+}}, i32 {{.*}})

markUsed(bar.directProperty2)
// CHECK: call i32 @"\01-[Bar directProperty2]"(%{{[0-9]+}}* %{{[0-9]+}})

bar[0] = 789
// CHECK: call void @"\01-[Bar setObject:atIndexedSubscript:]"(%{{[0-9]+}}* %{{[0-9]+}}, i32 789, i32 0)

markUsed(bar[0])
// CHECK: call i32 @"\01-[Bar objectAtIndexedSubscript:]"(%{{[0-9]+}}* %{{[0-9]+}}, i32 0)

markUsed(bar.directMethod())
// CHECK: call {{.*}} @"\01-[Bar directMethod]"(%{{[0-9]+}}* %{{[0-9]+}})

markUsed(bar.directMethod2())
// CHECK: call {{.*}} @"\01-[Bar directMethod2]"(%{{[0-9]+}}* %{{[0-9]+}})

markUsed(Bar.directClassMethod())
// NOTE: The class must be realized before calling objc_direct class methods, even if
//       Swift avoids explicit class realization before calling regular class methods.
// CHECK: [[R0:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$_Bar"
// CHECK: [[R1:%.*]] = call %objc_class*  @{{(swift_getInitializedObjCClass|objc_opt_self)}}(%objc_class* [[R0]])
// CHECK: [[R2:%.*]] = bitcast %objc_class* [[R1]] to i8*
// CHECK: call {{.*}} @"\01+[Bar directClassMethod]"(i8* [[R2]])

markUsed(Bar.directClassMethod2())
// CHECK: [[R3:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$_Bar"
// CHECK: [[R4:%.*]] = call %objc_class* @{{(swift_getInitializedObjCClass|objc_opt_self)}}(%objc_class* [[R3]])
// CHECK: [[R5:%.*]] = bitcast %objc_class* [[R4]] to i8*
// CHECK: call {{.*}} @"\01+[Bar directClassMethod2]"(i8* [[R5]])

markUsed(bar.directProtocolMethod())
// CHECK: call {{.*}} @"\01-[Bar directProtocolMethod]"({{.*}})

// CHECK: define {{.*}} swiftcc {{i32|i64}} @"$sSo3BarC5valueABSgs5Int32V_tcfC"
// CHECK:   call swiftcc {{i32|i64}} @"$sSo3BarC5valueABSgs5Int32V_tcfcTO"
// CHECK: }

// CHECK-DAG: declare i32 @"\01-[Bar directProperty]"
// CHECK-DAG: declare void @"\01-[Bar setDirectProperty:]"
// CHECK-DAG: declare i32 @"\01-[Bar directProperty2]"
// CHECK-DAG: declare void @"\01-[Bar setDirectProperty2:]"
// CHECK-DAG: declare void @"\01-[Bar setObject:atIndexedSubscript:]"
// CHECK-DAG: declare i32 @"\01-[Bar objectAtIndexedSubscript:]"
// CHECK-DAG: declare {{.*}} @"\01-[Bar directMethod]"
// CHECK-DAG: declare {{.*}} @"\01-[Bar directMethod2]"
// CHECK-DAG: declare {{.*}} @"\01+[Bar directClassMethod]"
// CHECK-DAG: declare {{.*}} @"\01+[Bar directClassMethod2]"
// CHECK-DAG: declare {{.*}} @"\01-[Bar directProtocolMethod]"

// CHECK: define {{.*}} swiftcc {{i32|i64}} @"$sSo3BarC5valueABSgs5Int32V_tcfcTO"
// CHECK:   call {{.*}} @"\01-[Bar initWithValue:]"
// CHECK: }
