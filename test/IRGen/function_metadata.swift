// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s

func arch<F>(_ f: F) {}

// CHECK: define hidden swiftcc void @_T017function_metadata9test_archyyF()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata(i8** %3) {{#[0-9]+}}
  arch( {() -> () in } )

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD:i(32|64)]] 1, i8* bitcast (%swift.type* @_T0SiN to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1)) {{#[0-9]+}}
  arch({(x: Int) -> () in  })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(_: ()) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1WithFlags([[WORD]] 1, i8* bitcast (%swift.type* @_T0SiN to i8*), i32* getelementptr inbounds ([1 x i32], [1 x i32]* @parameter-flags, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, i8* %3, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: (Int, Float)) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2WithFlags([[WORD]] 2, i8* bitcast (%swift.type* @_T0SiN to i8*), i8* bitcast (%swift.type* @_T0SiN to i8*), i32* getelementptr inbounds ([2 x i32], [2 x i32]* @parameter-flags.17, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2([[WORD]] 2, i8* bitcast (%swift.type* @_T0SfN to i8*), i8* bitcast (%swift.type* @_T0SiN to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(a: Float, b: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3WithFlags([[WORD]] 3, i8* bitcast (%swift.type* @_T0SiN to i8*), i8* bitcast (%swift.type* @_T0SfN to i8*), i8* bitcast (%swift.type* @_T0SSN to i8*), i32* getelementptr inbounds ([3 x i32], [3 x i32]* @parameter-flags.24, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Float, z: String) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3([[WORD]] 3, i8* bitcast (%swift.type* @_T0SfN to i8*), i8* bitcast (%swift.type* @_T0SfN to i8*), i8* bitcast (%swift.type* @_T0SiN to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(a: Float, b: Float, c: Int) -> () in })

  // CHECK: [[T0:%.*]] = getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 0
  // CHECK: store [[WORD]] 4
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 1
  // CHECK: store i8* bitcast (%swift.type* @_T0SiN to i8*), i8** %5, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 2
  // CHECK: store i8* bitcast (%swift.type* @_T0SdN to i8*)
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 3
  // CHECK: store i8* bitcast (%swift.type* @_T0SSN to i8*)
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 4
  // CHECK: store i8* bitcast (%swift.type* @_T0s4Int8VN to i8*)
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 5
  // CHECK: store i8* bitcast ([4 x i32]* @parameter-flags.31 to i8*), i8** %9, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 6
  // CHECK: bitcast i8** %10 to %swift.type**
  // CHECK: store %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1)
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata(i8** [[T0]]) {{#[0-9]+}}
  arch({(x: inout Int, y: Double, z: String, w: Int8) -> () in })
}
