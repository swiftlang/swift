// RUN: %swift -triple x86_64-apple-darwin10 -module-cache-path=%t/clang-module-cache -sdk=%S/Inputs %s -emit-llvm | FileCheck %s

// CHECK: define i64 @_T13generic_casts8allToIntU__FT1xQ__Si(%swift.opaque*, %swift.type* %T)
func allToInt<T>(x:T) -> Int {
  return x as! Int
  // CHECK: [[METADATA:%.*]] = bitcast %swift.type* %T to i8*{{$}}
  // CHECK: [[CAST:%.*]] = call %swift.opaque* @swift_dynamicCastIndirectUnconditional(%swift.opaque* {{%.*}}, i8* [[METADATA]], i8* bitcast ({{.*}}@_TMdSi{{.*}}))
  // CHECK: [[INT_RESULT_PTR:%.*]] = bitcast %swift.opaque* [[CAST]] to %Si*
  // CHECK: [[INT_RESULT_PTR_0:%.*]] = getelementptr inbounds %Si* [[INT_RESULT_PTR]], i32 0, i32 0
  // CHECK: [[INT_RESULT:%.*]] = load i64* [[INT_RESULT_PTR_0]], align 8
  // CHECK: ret i64 [[INT_RESULT]]
}

// CHECK: define i64 @_T13generic_casts8anyToIntFT1xP__Si(%"protocol<>"*)
func anyToInt(x:protocol<>) -> Int {
  return x as! Int
  // CHECK: [[METADATA_PTR:%.*]] = getelementptr inbounds %"protocol<>"* {{%.*}}, i32 0, i32 0
  // CHECK: [[METADATA_PTR:%.*]] = getelementptr inbounds %"protocol<>"* {{%.*}}, i32 0, i32 0
  // CHECK: [[METADATA_PTR:%.*]] = getelementptr inbounds %"protocol<>"* {{%.*}}, i32 0, i32 0
  // CHECK: [[METADATA_VAL:%.*]] = load %swift.type** [[METADATA_PTR]], align 8
  // CHECK: [[VALUE:%.*]] = call %swift.opaque* %projectBuffer([24 x i8]* {{%.*}}, %swift.type* [[METADATA_VAL]]) #1
  // CHECK: [[METADATA:%.*]] = bitcast %swift.type* %.metadata1 to i8*
  // CHECK: [[CAST:%.*]] = call %swift.opaque* @swift_dynamicCastIndirectUnconditional(%swift.opaque* [[VALUE]], i8* [[METADATA]], i8* bitcast ({{.*}}@_TMdSi{{.*}}) to i8*))
  // CHECK: [[INT_RESULT_PTR:%.*]] = bitcast %swift.opaque* [[CAST]] to %Si*
  // CHECK: [[INT_RESULT_PTR_0:%.*]] = getelementptr inbounds %Si* [[INT_RESULT_PTR]], i32 0, i32 0
  // CHECK: [[INT_RESULT:%.*]] = load i64* [[INT_RESULT_PTR_0]], align 8
  // CHECK: ret i64 [[INT_RESULT]]
}
