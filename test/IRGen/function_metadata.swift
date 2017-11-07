// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s

func arch<F>(_ f: F) {}

// CHECK: define hidden swiftcc void @_T017function_metadata9test_archyyF()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata(i8** %3) {{#[0-9]+}}
  arch( {() -> () in } )

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD:i(32|64)]] 1, %swift.type* @_T0SiN, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1)) {{#[0-9]+}}
  arch({(x: Int) -> () in  })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(_: ()) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1WithFlags([[WORD]] 1, %swift.type* @_T0SiN, i32* getelementptr inbounds ([1 x i32], [1 x i32]* @parameter-flags, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, %swift.type* %2, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: (Int, Float)) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2WithFlags([[WORD]] 2, %swift.type* @_T0SiN, %swift.type* @_T0SiN, i32* getelementptr inbounds ([2 x i32], [2 x i32]* @parameter-flags.17, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2([[WORD]] 2, %swift.type* @_T0SfN, %swift.type* @_T0SiN, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(a: Float, b: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3WithFlags([[WORD]] 3, %swift.type* @_T0SiN, %swift.type* @_T0SfN, %swift.type* @_T0SSN, i32* getelementptr inbounds ([3 x i32], [3 x i32]* @parameter-flags.24, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Float, z: String) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3([[WORD]] 3, %swift.type* @_T0SfN, %swift.type* @_T0SfN, %swift.type* @_T0SiN, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(a: Float, b: Float, c: Int) -> () in })

  // CHECK: [[T0:%.*]] = getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 0
  // CHECK: store [[WORD]] 4
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 1
  // CHECK: store %swift.type* @_T0SiN, %swift.type** %6, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 2
  // CHECK: store %swift.type* @_T0SdN, %swift.type** %8, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 3
  // CHECK: store %swift.type* @_T0SSN, %swift.type** %10, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 4
  // CHECK: store %swift.type* @_T0s4Int8VN, %swift.type** %12, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 5
  // CHECK: store i8* bitcast ([4 x i32]* @parameter-flags.31 to i8*), i8** %13, align 8
  // CHECK: getelementptr inbounds [7 x i8*], [7 x i8*]* %function-arguments, i32 0, i32 6
  // CHECK: bitcast i8** %14 to %swift.type**
  // CHECK: store %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1)
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata(i8** [[T0]]) {{#[0-9]+}}
  arch({(x: inout Int, y: Double, z: String, w: Int8) -> () in })
}
