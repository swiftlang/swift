// RUN: %target-swift-frontend -enable-type-layout -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK
// RUN: %target-swift-frontend -enable-type-layout -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT --check-prefix=OPT-%target-ptrsize
// RUN: %target-swift-frontend -enable-type-layout -force-struct-type-layouts -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=FORCE-OPT --check-prefix=FORCE-OPT-%target-ptrsize
// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s --check-prefix=NOTL

public struct B<T> {
  var x: T
  var y: T
}

public struct A<T> {
  var a : B<T>
  var b:  B<T>
}

public class C<T> {
  init(a: B<T>, b: B<T>) {
    self.a = a
    self.b = b
  }
  var a : B<T>
  var b:  B<T>
}

public struct Fixed<T> {
  var a : Int8
  var b : Int16
  var c : C<T>
}

public enum NullableEnum<T> {
  case Value(a: C<T>)
  case None
}

public struct ForwardStruct<T> {
  var a : UInt64
  var b : UInt64
  var c : C<T>
}
public enum ForwardEnum<T> {
  case Value(a: ForwardStruct<T>)
  case None
}

// NOTL-LABEL: define{{.*}} %swift.opaque* @"$s30typelayout_based_value_witness1AVwCP"(
// NOTL:   @"$s30typelayout_based_value_witness1BVMa"(
// NOTL: }

// CHECK-LABEL: define{{.*}} %swift.opaque* @"$s30typelayout_based_value_witness1AVwCP"(
// CHECK-NOT:   @"$s30typelayout_based_value_witness1BVMa"(
// CHECK: }

// CHECK-LABEL: define{{.*}} void @"$s30typelayout_based_value_witness1AVwxx"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} %swift.opaque* @"$s30typelayout_based_value_witness1AVwcp"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} %swift.opaque* @"$s30typelayout_based_value_witness1AVwca"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} %swift.opaque* @"$s30typelayout_based_value_witness1AVwtk"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} %swift.opaque* @"$s30typelayout_based_value_witness1AVwta"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} i32 @"$s30typelayout_based_value_witness1AVwet"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} void @"$s30typelayout_based_value_witness1AVwst"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }


// OPT: define{{.*}} void @"$s30typelayout_based_value_witness1AVwxx"(%swift.opaque* noalias %object, %swift.type* nocapture readonly %"A<T>")
// OPT:   [[T_PARAM:%.*]] = getelementptr inbounds %swift.type, %swift.type* %"A<T>", {{(i64|i32)}} 2
// OPT:   [[T2:%.*]] = bitcast %swift.type* [[T_PARAM]] to %swift.type**
// OPT:   [[T:%.*]] = load %swift.type*, %swift.type** [[T2]]
// OPT:   [[VWT_ADDR:%.*]] = getelementptr inbounds %swift.type, %swift.type* [[T]], {{(i64|i32)}} -1
// OPT:   [[VWT2:%.*]] = bitcast %swift.type* [[VWT_ADDR]] to i8***
// OPT:   [[VWT:%.*]] = load i8**, i8*** [[VWT2]]
// OPT:   [[DESTROY_VW:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], {{(i64|i32)}} 1
// OPT:   [[DESTROY2:%.*]] = bitcast i8** [[DESTROY_VW]] to void (%swift.opaque*, %swift.type*)**
// OPT:   [[DESTROY:%.*]] = load void (%swift.opaque*, %swift.type*)*, void (%swift.opaque*, %swift.type*)** [[DESTROY2]]
// OPT:   tail call void [[DESTROY]](%swift.opaque* noalias %object, %swift.type* [[T]])
// OPT:   [[SIZE_VW:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], {{(i64|i32)}} 8
// OPT:   [[SIZE2:%.*]] = bitcast i8** [[SIZE_VW]] to {{(i64|i32)}}*
// OPT:   [[SIZE_T:%.*]] = load {{(i64|i32)}}, {{(i64|i32)}}* [[SIZE2]]
// OPT:   [[OBJECT:%.*]] = ptrtoint %swift.opaque* %object to {{(i64|i32)}}
// OPT:   [[FLAGS_VW:%.*]] = getelementptr inbounds i8*, i8** [[VWT]], {{(i64|i32)}} 10
// OPT:   [[FLAGS2:%.*]] = bitcast i8** [[FLAGS_VW]] to i32*
// OPT:   [[FLAGS3:%.*]] = load i32, i32* [[FLAGS2]]
// OPT:   [[FLAGS:%.*]] = and i32 [[FLAGS3]], 255
// OPT-64:   %flags.alignmentMask = zext i32 [[FLAGS]] to i64
// OPT-64:   [[TMP:%.*]] = add {{(i64|i32)}} [[SIZE_T]], %flags.alignmentMask
// OPT-32:   [[TMP:%.*]] = add {{(i64|i32)}} %flags.alignmentMask, [[SIZE_T]]
// OPT:   [[TMP2:%.*]] = add {{(i64|i32)}} [[TMP]], [[OBJECT]]
// OPT:   [[INVERTED:%.*]] = xor {{(i64|i32)}} %flags.alignmentMask, -1
// OPT:   [[ADDR2:%.*]] = and {{(i64|i32)}} [[TMP2:%.*]], [[INVERTED:%.*]]
// OPT:   [[ADDR_T2:%.*]] = inttoptr {{(i64|i32)}} [[ADDR2]] to %swift.opaque*
// OPT:   tail call void [[DESTROY]](%swift.opaque* noalias [[ADDR_T2]], %swift.type* [[T]])
// OPT:   [[TMP3:%.*]] = and {{(i64|i32)}} [[TMP]], [[INVERTED:%.*]]
// OPT:   [[TMP4:%.*]] = add {{(i64|i32)}} [[TMP2:%.*]], [[TMP3]]
// OPT:   [[ADDR3:%.*]] = and {{(i64|i32)}} [[TMP4]], [[INVERTED:%.*]]
// OPT:   [[ADDR_T3:%.*]] = inttoptr {{(i64|i32)}} [[ADDR3]] to %swift.opaque*
// OPT:   tail call void [[DESTROY]](%swift.opaque* noalias [[ADDR_T3]], %swift.type* [[T]])
// OPT:   [[TMP5:%.*]] = add {{(i64|i32)}} [[TMP]], [[ADDR3]]
// OPT:   [[ADDR4:%.*]] = and {{(i64|i32)}} [[TMP5]], [[INVERTED:%.*]]
// OPT:   [[ADDR_T4:%.*]] = inttoptr {{(i64|i32)}} [[ADDR4]] to %swift.opaque*
// OPT:   tail call void [[DESTROY]](%swift.opaque* noalias [[ADDR_T4]], %swift.type* [[T]])
// OPT:   ret void
// CHECK: }

// FORCE-OPT: define{{.*}} void @"$s30typelayout_based_value_witness5FixedVwxx"(%swift.opaque* noalias nocapture readonly %object, %swift.type* nocapture readnone %"Fixed<T>")
// For fixed types, we should expect a direct gep to the address instead of a
// manual alignment computation only if we force creating aligned groups for structs
// FORCE-OPT:  [[T_PARAM:%.*]] = bitcast %swift.opaque* %object to i8*
// FORCE-OPT:  [[OFFSET:%.*]] = getelementptr inbounds i8, i8* [[T_PARAM]], {{(i64|i32)}} {{4|8}}
// FORCE-OPT:  [[CASTED:%.*]] = bitcast i8* [[OFFSET]] to %T30typelayout_based_value_witness1CC**
// FORCE-OPT:  %toDestroy = load %T30typelayout_based_value_witness1CC*, %T30typelayout_based_value_witness1CC** [[CASTED]]
// FORCE-OPT:  [[FIELD:%.*]] = getelementptr %T30typelayout_based_value_witness1CC, %T30typelayout_based_value_witness1CC* %toDestroy, {{(i64|i32)}} 0, i32 0
// FORCE-OPT:  tail call void @swift_release(%swift.refcounted* [[FIELD]])
// FORCE-OPT:  ret void
// FORCE-OPT:}

// FORCE-OPT: define{{.*}} void @"$s30typelayout_based_value_witness12NullableEnumOwxx"(%swift.opaque* noalias nocapture readonly %object, %swift.type* nocapture readnone %"NullableEnum<T>")
// For enums with a pointer type and a single empty case, the empty case should
// get encoded as the 0 value, so it should not generate a branch
// FORCE-OPT: %toDestroy = load %T30typelayout_based_value_witness1CC*, %T30typelayout_based_value_witness1CC** %{{.*}}
// FORCE-OPT: [[FIELD:%.*]] = getelementptr %T30typelayout_based_value_witness1CC, %T30typelayout_based_value_witness1CC* %toDestroy, {{(i64|i32)}} 0, i32 0
// FORCE-OPT: tail call void @swift_release(%swift.refcounted* [[FIELD]])
// FORCE-OPT: ret void
// FORCE-OPT:}

// FORCE-OPT: define{{.*}} void @"$s30typelayout_based_value_witness11ForwardEnumOwxx"(%swift.opaque* noalias nocapture readonly %object, %swift.type* nocapture readnone %"ForwardEnum<T>")
// Since fixed is able to hold the one extra tag required in it's pointer field,
// it's safe to release it without checking the case. We should not see a branch here
// FORCE-OPT:  [[T_PARAM:%.*]] = bitcast %swift.opaque* %object to i8*
// FORCE-OPT:  [[OFFSET:%.*]] = getelementptr inbounds i8, i8* [[T_PARAM]], {{(i64|i32)}} {{(8|16)}}
// FORCE-OPT:  [[CASTED:%.*]] = bitcast i8* [[OFFSET]] to %T30typelayout_based_value_witness1CC**
// FORCE-OPT:  %toDestroy = load %T30typelayout_based_value_witness1CC*, %T30typelayout_based_value_witness1CC** [[CASTED]]
// FORCE-OPT:  [[FIELD:%.*]] = getelementptr %T30typelayout_based_value_witness1CC, %T30typelayout_based_value_witness1CC* %toDestroy, {{(i64|i32)}} 0, i32 0
// FORCE-OPT:  tail call void @swift_release(%swift.refcounted* [[FIELD]])
// FORCE-OPT:  ret void
// FORCE-OPT:}


// Let's not crash on the following example.
public protocol P {}

public class Ref<T: P> {}

public enum E1<R: P> {
  case first(R)
  case second(S<R>)
}

public struct S<T: P> {
  public let f: E2<T>? = nil
}

public enum E2<T: P> {
    case first(Ref<T>)
    case second(String)
}
