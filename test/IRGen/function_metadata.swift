// RUN: %target-swift-frontend -emit-ir -primary-file %s | FileCheck %s

// REQUIRES: CPU=x86_64

func arch<F>(f: F) {}

// CHECK: define hidden void @_TF17function_metadata9test_archFT_T_()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1(i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1)) #1
  arch( {() -> () in } )

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1(i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1))
  arch({(x: Int) -> () in  })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1(i8* inttoptr (i64 or (i64 ptrtoint (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1) to i64), i64 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1))
  arch({(inout x: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2(i8* inttoptr (i64 or (i64 ptrtoint (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1) to i64), i64 1) to i8*), i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1))
  arch({(inout x: Int, y: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3(i8* inttoptr (i64 or (i64 ptrtoint (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1) to i64), i64 1) to i8*), i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSf, i32 0, i32 1) to i8*), i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSS, i32 0, i32 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1))
  arch({(inout x: Int, y: Float, z: String) -> () in })

  // CHECK: getelementptr inbounds [4 x i8*], [4 x i8*]* %function-arguments, i32 0, i32 0
  // CHECK: store i8* inttoptr (i64 or (i64 ptrtoint (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSi, i32 0, i32 1) to i64), i64 1) to i8*), i8** %2, align 8
  // CHECK: getelementptr inbounds [4 x i8*], [4 x i8*]* %function-arguments, i32 0, i32 1
  // CHECK: store i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSd, i32 0, i32 1) to i8*), i8** %3, align 8
  // CHECK: getelementptr inbounds [4 x i8*], [4 x i8*]* %function-arguments, i32 0, i32 2
  // CHECK: store i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdSS, i32 0, i32 1) to i8*), i8** %4, align 8
  // CHECK: getelementptr inbounds [4 x i8*], [4 x i8*]* %function-arguments, i32 0, i32 3
  // CHECK: store i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type* @_TMdVSs4Int8, i32 0, i32 1) to i8*), i8** %5, align 8
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata(i64 4, i8** %2, %swift.type* getelementptr inbounds (%swift.full_type* @_TMdT_, i32 0, i32 1)) #1
  arch({(inout x: Int, y: Double, z: String, w: Int8) -> () in })
}
