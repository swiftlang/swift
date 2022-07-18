// RUN: %target-swift-emit-ir -import-objc-header %S/../Inputs/objc_direct.h -o - %s | %FileCheck %s

// REQUIRES: objc_interop

func markUsed<T>(_ t: T) {}

protocol BarProtocol {
    func directProtocolMethod() -> String!
}

extension Bar: BarProtocol {}

let bar = Bar(value: 0)!
// CHECK: call swiftcc i64 @"$sSo3BarC5valueABSgs5Int32V_tcfC"

let bar2 = Bar.init(value: 0)!
// CHECK: call swiftcc i64 @"$sSo3BarC5valueABSgs5Int32V_tcfC"

bar.directProperty = 123
// CHECK: call void @"\01-[Bar setDirectProperty:]"({{.*}}, i8* undef, i32 {{.*}})

markUsed(bar.directProperty)
// CHECK: call i32 @"\01-[Bar directProperty]"({{.*}}, i8* undef)

bar.directProperty2 = 456
// CHECK: call void @"\01-[Bar setDirectProperty2:]"({{.*}}, i8* undef, i32 {{.*}})

markUsed(bar.directProperty2)
// CHECK: call i32 @"\01-[Bar directProperty2]"({{.*}}, i8* undef)

bar[0] = 789
// CHECK: call void @"\01-[Bar setObject:atIndexedSubscript:]"({{.*}}, i8* undef, i32 789, i32 0)

markUsed(bar[0])
// CHECK: call i32 @"\01-[Bar objectAtIndexedSubscript:]"({{.*}}, i8* undef, i32 0)

markUsed(bar.directMethod())
// CHECK: call {{.*}} @"\01-[Bar directMethod]"({{.*}}, i8* undef)

markUsed(bar.directMethod2())
// CHECK: call {{.*}} @"\01-[Bar directMethod2]"({{.*}}, i8* undef)

markUsed(Bar.directClassMethod())
// NOTE: The class must be realized before calling objc_direct class methods, even if
//       Swift avoids explicit class realization before calling regular class methods.
// CHECK: [[R0:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$_Bar"
// CHECK: [[R1:%.*]] = call %objc_class*  @{{(swift_getInitializedObjCClass|objc_opt_self)}}(%objc_class* [[R0]])
// CHECK: [[R2:%.*]] = bitcast %objc_class* [[R1]] to i8*
// CHECK: call {{.*}} @"\01+[Bar directClassMethod]"(i8* [[R2]], i8* undef)

markUsed(Bar.directClassMethod2())
// CHECK: [[R3:%.*]] = load %objc_class*, %objc_class** @"OBJC_CLASS_REF_$_Bar"
// CHECK: [[R4:%.*]] = call %objc_class* @{{(swift_getInitializedObjCClass|objc_opt_self)}}(%objc_class* [[R3]])
// CHECK: [[R5:%.*]] = bitcast %objc_class* [[R4]] to i8*
// CHECK: call {{.*}} @"\01+[Bar directClassMethod2]"(i8* [[R5]], i8* undef)

markUsed(bar.directProtocolMethod())
// CHECK: call {{.*}} @"\01-[Bar directProtocolMethod]"({{.*}}, i8* undef)

// CHECK: define {{.*}} swiftcc i64 @"$sSo3BarC5valueABSgs5Int32V_tcfC"
// CHECK:   call swiftcc i64 @"$sSo3BarC5valueABSgs5Int32V_tcfcTO"
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

// CHECK: define {{.*}} swiftcc i64 @"$sSo3BarC5valueABSgs5Int32V_tcfcTO"
// CHECK:   call {{.*}} @"\01-[Bar initWithValue:]"
// CHECK: }
