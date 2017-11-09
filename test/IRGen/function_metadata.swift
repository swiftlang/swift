// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s

func arch<F>(_ f: F) {}

// CHECK: define hidden swiftcc void @_T017function_metadata9test_archyyF()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata([[WORD:i(32|64)]] 0, %swift.type** null, i32* null, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch( {() -> () in } )

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD:i(32|64)]] 1, %swift.type* @_T0SiN, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1)) {{#[0-9]+}}
  arch({(x: Int) -> () in  })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(_: ()) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1WithFlags([[WORD]] 33554433, %swift.type* @_T0SiN, i32 1, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, %swift.type* %2, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: (Int, Float)) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2WithFlags([[WORD]] 33554434, %swift.type* @_T0SiN, i32 1, %swift.type* @_T0SiN, i32 0, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2([[WORD]] 2, %swift.type* @_T0SfN, %swift.type* @_T0SiN, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(a: Float, b: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3WithFlags([[WORD]] 33554435, %swift.type* @_T0SiN, i32 1, %swift.type* @_T0SfN, i32 0, %swift.type* @_T0SSN, i32 0, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Float, z: String) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3([[WORD]] 3, %swift.type* @_T0SfN, %swift.type* @_T0SfN, %swift.type* @_T0SiN, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(a: Float, b: Float, c: Int) -> () in })

  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 0
  // CHECK: store %swift.type* @_T0SiN, %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 1
  // CHECK: store %swift.type* @_T0SdN, %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 2
  // CHECK: store %swift.type* @_T0SSN, %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 3
  // CHECK: store %swift.type* @_T0s4Int8VN, %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: [[T:%.*]] = getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 0
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata([[WORD]] 33554436, %swift.type** %7, i32* getelementptr inbounds ([4 x i32], [4 x i32]* @parameter-flags, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_T0ytN, i32 0, i32 1))
  arch({(x: inout Int, y: Double, z: String, w: Int8) -> () in })
}
