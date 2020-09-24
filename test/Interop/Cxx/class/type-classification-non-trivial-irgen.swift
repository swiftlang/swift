// RUN: %target-swift-frontend -enable-cxx-interop -I %S/Inputs %s -emit-ir | %FileCheck %s

// Verify that non-trival/address-only C++ classes are constructed and accessed
// correctly. Make sure that we correctly IRGen functions that construct
// non-trivial C++ classes, take those classes as a parameter, and access those
// classes members.

import TypeClassification

// TODO: C++ objects with destructors should be tested here once we fully
// support them.

// CHECK-LABEL: define {{.*}}i1 @"$s4main37testStructWithCopyConstructorAndValueSbyF"
// CHECK: [[OBJ:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[VAL_ELEMENT:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[OBJ]], i32 0, i32 0
// CHECK: [[VAL_INT:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[VAL_ELEMENT]], i32 0, i32 0
// CHECK: store i32 42, i32* [[VAL_INT]]
// CHECK: ret i1 true
public func testStructWithCopyConstructorAndValue() -> Bool {
  let obj = StructWithCopyConstructorAndValue(value: 42)
  return obj.value == 42
}

// CHECK-LABEL: define {{.*}}i1 @"$s4main46testStructWithSubobjectCopyConstructorAndValueSbyF"()
// CHECK: [[MEMBER:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[OBJ:%.*]] = alloca %TSo42StructWithSubobjectCopyConstructorAndValueV
// CHECK: alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[TMP:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[MEMBER_ELEMENT:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[MEMBER]], i32 0, i32 0
// CHECK: [[MEMBER_VALUE:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[MEMBER_ELEMENT]], i32 0, i32 0
// CHECK: store i32 42, i32* [[MEMBER_VALUE]]
// CHECK: [[TEMP_MEMBER:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[TMP]], i32 0, i32 0
// CHECK: [[TEMP_MEMBER_VALUE:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[TEMP_MEMBER]], i32 0, i32 0
// CHECK: [[LHS:%.*]] = load i32, i32* [[TEMP_MEMBER_VALUE]]
// CHECK: [[OUT:%.*]] = icmp eq i32 [[LHS]], 42
// CHECK: ret i1 [[OUT]]
public func testStructWithSubobjectCopyConstructorAndValue() -> Bool {
  let member = StructWithCopyConstructorAndValue(value: 42)
  let obj = StructWithSubobjectCopyConstructorAndValue(member: member)
  return obj.member.value == 42
}

// CHECK-LABEL: define {{.*}}i1 @"$s4main041testStructWithCopyConstructorAndSubobjectefG5ValueSbyF"()
// CHECK: [[MEMBER:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: alloca %TSo037StructWithCopyConstructorAndSubobjectcdE5ValueV
// CHECK: alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[TEMP:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[MEMBER_VAL:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[MEMBER]], i32 0, i32 0
// CHECK: [[MEMBER_VAL_VAL:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[MEMBER_VAL]], i32 0, i32 0
// CHECK: store i32 42, i32* [[MEMBER_VAL_VAL]]
// CHECK: [[TEMP_MEMBER:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[TEMP]], i32 0, i32 0
// CHECK: [[TEMP_MEMBER_VAL:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[TEMP_MEMBER]], i32 0, i32 0
// CHECK: [[LHS:%.*]] = load i32, i32* [[TEMP_MEMBER_VAL]]
// CHECK: [[OUT:%.*]] = icmp eq i32 [[LHS]], 42
// CHECK: ret i1 [[OUT]]
public func testStructWithCopyConstructorAndSubobjectCopyConstructorAndValue()
-> Bool {
  let member = StructWithCopyConstructorAndValue(value: 42)
  let obj = StructWithCopyConstructorAndSubobjectCopyConstructorAndValue(
    member: member
  )
  return obj.member.value == 42
}

// CHECK-LABEL: define {{.*}}i1 @"$s4main4test3objSbSo33StructWithCopyConstructorAndValueV_tF"(%TSo33StructWithCopyConstructorAndValueV* noalias nocapture dereferenceable(4) %0)
// CHECK: [[VAL:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* %0, i32 0, i32 0
// CHECK: [[VAL_VAL:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[VAL]], i32 0, i32 0
// CHECK: [[LHS:%.*]] = load i32, i32* [[VAL_VAL]]
// CHECK: [[OUT:%.*]] = icmp eq i32 %1, 42
// CHECK: ret i1 [[OUT]]
public func test(obj: StructWithCopyConstructorAndValue) -> Bool {
  return obj.value == 42
}

// CHECK-LABEL: define {{.*}}i1 @"$s4main4test3objSbSo42StructWithSubobjectCopyConstructorAndValueV_tF"(%TSo42StructWithSubobjectCopyConstructorAndValueV* noalias nocapture dereferenceable(4) %0)
// CHECK: [[TMP:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK: [[MEMBER:%.*]] = getelementptr inbounds %TSo42StructWithSubobjectCopyConstructorAndValueV, %TSo42StructWithSubobjectCopyConstructorAndValueV* %0, i32 0, i32 0
// CHECK: [[VAL:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[TMP]], i32 0, i32 0
// CHECK: [[VAL_VAL:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[VAL]], i32 0, i32 0
// CHECK: [[LHS:%.*]] = load i32, i32* [[VAL_VAL]]
// CHECK: [[OUT:%.*]] = icmp eq i32 [[LHS]], 42
// CHECK: ret i1 [[OUT]]
public func test(obj: StructWithSubobjectCopyConstructorAndValue) -> Bool {
  return obj.member.value == 42
}

// CHECK-LABEL: define {{.*}}i1 @"$s4main4test3objSbSo037StructWithCopyConstructorAndSubobjectfgH5ValueV_tF"(%TSo037StructWithCopyConstructorAndSubobjectcdE5ValueV* noalias nocapture dereferenceable(4) %0)
// CHECK:[[TEMP:%.*]] = alloca %TSo33StructWithCopyConstructorAndValueV
// CHECK:[[VAL:%.*]] = getelementptr inbounds %TSo33StructWithCopyConstructorAndValueV, %TSo33StructWithCopyConstructorAndValueV* [[TEMP]], i32 0, i32 0
// CHECK:[[VAL_VAL:%.*]] = getelementptr inbounds %Ts5Int32V, %Ts5Int32V* [[VAL]], i32 0, i32 0
// CHECK:[[LHS:%.*]] = load i32, i32* [[VAL_VAL]]
// CHECK:[[OUT:%.*]] = icmp eq i32 [[LHS]], 42
// CHECK:ret i1 [[OUT]]
public func test(
  obj: StructWithCopyConstructorAndSubobjectCopyConstructorAndValue
) -> Bool {
  return obj.member.value == 42
}
