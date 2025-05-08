// RUN: %target-swift-frontend -enable-type-layout -primary-file %s -emit-ir | %FileCheck %s --check-prefix=CHECK
// RUN: %target-swift-frontend -enable-type-layout -primary-file %s -O -emit-ir | %FileCheck %s --check-prefix=OPT --check-prefix=OPT-%target-ptrsize --check-prefix=OPT-%target-ptrauth
// RUN: %target-swift-frontend -primary-file %s -emit-ir | %FileCheck %s --check-prefix=NOTL

// REQUIRES: PTRSIZE=64

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

// NOTL-LABEL: define{{.*}} ptr @"$s30typelayout_based_value_witness1AVwCP"(
// NOTL:   @"$s30typelayout_based_value_witness1BVMa"(
// NOTL: }

// CHECK-LABEL: define{{.*}} ptr @"$s30typelayout_based_value_witness1AVwCP"(
// CHECK-NOT:   @"$s30typelayout_based_value_witness1BVMa"(
// CHECK: }

// CHECK-LABEL: define{{.*}} void @"$s30typelayout_based_value_witness1AVwxx"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} ptr @"$s30typelayout_based_value_witness1AVwcp"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} ptr @"$s30typelayout_based_value_witness1AVwca"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} ptr @"$s30typelayout_based_value_witness1AVwtk"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} ptr @"$s30typelayout_based_value_witness1AVwta"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} i32 @"$s30typelayout_based_value_witness1AVwet"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }

// CHECK-LABEL: define{{.*}} void @"$s30typelayout_based_value_witness1AVwst"(
// CHECK-NOT: @"$s30typelayout_based_value_witness1BVMa"
// CHECK: }


// OPT: define{{.*}} void @"$s30typelayout_based_value_witness1AVwxx"(ptr noalias %object, ptr{{( nocapture)?}} readonly{{( captures\(none\))?}} %"A<T>")
// OPT:   [[T_PARAM:%.*]] = getelementptr inbounds{{.*}} i8, ptr %"A<T>", i64 16
// OPT:   [[T:%.*]] = load ptr, ptr [[T_PARAM]]
// OPT:   [[VWT_ADDR:%.*]] = getelementptr inbounds i8, ptr [[T]], {{(i64|i32)}} -8

// OPT-noptrauth:   [[VWT:%.*]] = load ptr, ptr [[VWT_ADDR]]

// OPT-ptrauth: [[ADDR_INT:%.*]] = ptrtoint ptr [[VWT_ADDR]] to i64
// OPT-ptrauth: [[DISCRIMINANT:%.*]] = tail call i64 @llvm.ptrauth.blend(i64 [[ADDR_INT]], i64 11839)
// OPT-ptrauth: [[SIGNED_VWT:%.*]] = ptrtoint ptr %T.valueWitnesses
// OPT-ptrauth: [[VWT_INT:%.*]] = tail call i64 @llvm.ptrauth.auth(i64 [[SIGNED_VWT]], i32 2, i64 [[DISCRIMINANT]])
// OPT-ptrauth: [[VWT:%.*]] = inttoptr i64 [[VWT_INT]] to ptr

// OPT:   [[DESTROY_VW:%.*]] = getelementptr inbounds{{.*}} i8, ptr [[VWT]], {{(i64|i32)}} 8
// OPT:   [[DESTROY:%.*]] = load ptr, ptr [[DESTROY_VW]]

// OPT-ptrauth: [[DESTROY_ADDR:%.*]] = ptrtoint ptr [[DESTROY_VW]] to i64
// OPT-ptrauth: [[DISCRIMINANT:%.*]] = tail call i64 @llvm.ptrauth.blend(i64 [[DESTROY_ADDR]], i64 1272)

// OPT:   tail call void [[DESTROY]](ptr noalias %object, ptr [[T]])
// OPT:   [[SIZE_VW:%.*]] = getelementptr inbounds{{.*}} i8, ptr [[VWT]], {{(i64|i32)}} 64
// OPT:   [[SIZE_T:%.*]] = load {{(i64|i32)}}, ptr [[SIZE_VW]]
// OPT:   [[OBJECT:%.*]] = ptrtoint ptr %object to {{(i64|i32)}}
// OPT:   [[FLAGS_VW:%.*]] = getelementptr inbounds{{.*}} i8, ptr [[VWT]], {{(i64|i32)}} 80
// OPT:   [[FLAGS3:%.*]] = load i32, ptr [[FLAGS_VW]]
// OPT:   [[FLAGS:%.*]] = and i32 [[FLAGS3]], 255
// OPT-64:   %flags.alignmentMask = zext nneg i32 [[FLAGS]] to i64
// OPT-64:   [[TMP:%.*]] = add {{(i64|i32)}} [[SIZE_T]], %flags.alignmentMask
// OPT-32:   [[TMP:%.*]] = add {{(i64|i32)}} %flags.alignmentMask, [[SIZE_T]]
// OPT:   [[TMP2:%.*]] = add {{(i64|i32)}} [[TMP]], [[OBJECT]]
// OPT:   [[INVERTED:%.*]] = xor {{(i64|i32)}} %flags.alignmentMask, -1
// OPT:   [[ADDR2:%.*]] = and {{(i64|i32)}} [[TMP2:%.*]], [[INVERTED:%.*]]
// OPT:   [[ADDR_T2:%.*]] = inttoptr {{(i64|i32)}} [[ADDR2]] to ptr
// OPT:   tail call void [[DESTROY]](ptr noalias [[ADDR_T2]], ptr [[T]])
// OPT:   [[TMP3:%.*]] = and {{(i64|i32)}} [[TMP]], [[INVERTED:%.*]]
// OPT:   [[TMP4:%.*]] = add {{(i64|i32)}} [[TMP2:%.*]], [[TMP3]]
// OPT:   [[ADDR3:%.*]] = and {{(i64|i32)}} [[TMP4]], [[INVERTED:%.*]]
// OPT:   [[ADDR_T3:%.*]] = inttoptr {{(i64|i32)}} [[ADDR3]] to ptr
// OPT:   tail call void [[DESTROY]](ptr noalias [[ADDR_T3]], ptr [[T]])
// OPT:   [[TMP5:%.*]] = add {{(i64|i32)}} [[TMP]], [[ADDR3]]
// OPT:   [[ADDR4:%.*]] = and {{(i64|i32)}} [[TMP5]], [[INVERTED:%.*]]
// OPT:   [[ADDR_T4:%.*]] = inttoptr {{(i64|i32)}} [[ADDR4]] to ptr
// OPT:   tail call void [[DESTROY]](ptr noalias [[ADDR_T4]], ptr [[T]])
// OPT:   ret void
// CHECK: }

// OPT: define internal void @"$s30typelayout_based_value_witness2E3Owui"(ptr noalias{{( nocapture)?}} writeonly{{( captures\(none\))?}} %value, i32 %tag, ptr{{( nocapture)?}} readonly{{( captures\(none\))?}} %"E3<T>")
// OPT:  [[IS_EMPTY:%.*]] = icmp eq i32 {{%.*}}, 0
// OPT:  br i1 [[IS_EMPTY]], label %empty-payload, label %non-empty-payload
// OPT: }

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

public enum E3<T> {
  case empty
  case first(T)
  case second(T)
}
