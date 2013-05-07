// RUN: %swift -triple x86_64-apple-darwin10 %s -emit-llvm -o - | FileCheck %s

func standalone_generic<T>(x:T, y:T) -> T { return x }
// CHECK: define void [[GENERIC:@_T16specialize_thunk18standalone_genericU__FT1xQ_1yQ__Q_]](%swift.opaque* noalias sret, %swift.opaque*, %swift.opaque*, %swift.type* %T)

func return_specialization() -> (x:Int64, y:Int64) -> Int64 {
  return standalone_generic
}
// CHECK: define { i8*, %swift.refcounted* } @_T16specialize_thunk21return_specializationFT_FT1xSi1ySi_Si() {
// CHECK:   ret { i8*, %swift.refcounted* } { i8* bitcast (i64 (i64, i64)* [[INT64_SPECIALIZE:@.*]] to i8*), %swift.refcounted* null }
// CHECK: }

// CHECK: define internal i64 [[INT64_SPECIALIZE]](i64, i64) {
// CHECK:   [[X_OPAQUE:%.*]] = bitcast %Si* {{%.*}} to %swift.opaque*
// CHECK:   [[Y_OPAQUE:%.*]] = bitcast %Si* {{%.*}} to %swift.opaque*
// CHECK:   [[RES_OPAQUE:%.*]] = bitcast %Si* [[RES:%.*]] to %swift.opaque*
// CHECK:   call void [[GENERIC]](%swift.opaque* noalias sret [[RES_OPAQUE]], %swift.opaque* [[X_OPAQUE]], %swift.opaque* [[Y_OPAQUE]], %swift.type* {{.*}} @_TMdSi{{.*}})
// CHECK:   [[RES_FIELD:%.*]] = getelementptr inbounds %Si* [[RES]], i32 0, i32 0
// CHECK:   [[RES_VALUE:%.*]] = load i64* [[RES_FIELD]]
// CHECK:   ret i64 [[RES_VALUE]]
// CHECK: }

protocol Runcible {}

func return_indirect_specialization() -> (x:Runcible, y:Runcible) -> Runcible {
  return standalone_generic
}
// CHECK: define { i8*, %swift.refcounted* } @_T16specialize_thunk30return_indirect_specializationFT_FT1xPS_8Runcible1yS0__S0_() {
// CHECK:   ret { i8*, %swift.refcounted* } { i8* bitcast (void (%P16specialize_thunk8Runcible*, %P16specialize_thunk8Runcible*, %P16specialize_thunk8Runcible*)* [[RUNCIBLE_SPECIALIZE:@specialize1]] to i8*), %swift.refcounted* null }
// CHECK: }

// CHECK: define internal void @specialize1(%P16specialize_thunk8Runcible* noalias sret, %P16specialize_thunk8Runcible*, %P16specialize_thunk8Runcible*) {
// CHECK:   [[X_OPAQUE:%.*]] = bitcast %P16specialize_thunk8Runcible* {{%.*}} to %swift.opaque*
// CHECK:   [[Y_OPAQUE:%.*]] = bitcast %P16specialize_thunk8Runcible* {{%.*}} to %swift.opaque*
// CHECK:   [[RES_OPAQUE:%.*]] = bitcast %P16specialize_thunk8Runcible* {{%.*}} to %swift.opaque*
// CHECK:   call void [[GENERIC]](%swift.opaque* noalias sret [[RES_OPAQUE]], %swift.opaque* [[X_OPAQUE]], %swift.opaque* [[Y_OPAQUE]], %swift.type* {{.*}} @_TMdP16specialize_thunk8Runcible{{.*}})
// CHECK:   ret void
// CHECK: }
