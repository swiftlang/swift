// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s

func arch<F>(_ f: F) {}

// CHECK: define hidden swiftcc void @_TF17function_metadata9test_archFT_T_()
func test_arch() {
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD:i(64|32)]] 1, i8* bitcast (%swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1)) {{#[0-9]+}}
  arch( {() -> () in } )

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, i8* bitcast (%swift.type* @_TMSi to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1))
  arch({(x: Int) -> () in  })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 1, i8* inttoptr ([[WORD]] or ([[WORD]] ptrtoint (%swift.type* @_TMSi to [[WORD]]), [[WORD]] 1) to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1))
  arch({(x: inout Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2([[WORD]] 2, i8* inttoptr ([[WORD]] or ([[WORD]] ptrtoint (%swift.type* @_TMSi to [[WORD]]), [[WORD]] 1) to i8*), i8* bitcast (%swift.type* @_TMSi to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1))
  arch({(x: inout Int, y: Int) -> () in })

  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3([[WORD]] 3, i8* inttoptr ([[WORD]] or ([[WORD]] ptrtoint (%swift.type* @_TMSi to [[WORD]]), [[WORD]] 1) to i8*), i8* bitcast (%swift.type* @_TMSf to i8*), i8* bitcast (%swift.type* @_TMSS to i8*), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1))
  arch({(x: inout Int, y: Float, z: String) -> () in })

  // CHECK: [[T0:%.*]] = getelementptr inbounds [6 x i8*], [6 x i8*]* %function-arguments, i32 0, i32 0
  // CHECK: store [[WORD]] 4
  // CHECK: getelementptr inbounds [6 x i8*], [6 x i8*]* %function-arguments, i32 0, i32 1
  // CHECK: store i8* inttoptr ([[WORD]] or ([[WORD]] ptrtoint (%swift.type* @_TMSi to [[WORD]]), [[WORD]] 1) to i8*)
  // CHECK: getelementptr inbounds [6 x i8*], [6 x i8*]* %function-arguments, i32 0, i32 2
  // CHECK: store i8* bitcast (%swift.type* @_TMSd to i8*)
  // CHECK: getelementptr inbounds [6 x i8*], [6 x i8*]* %function-arguments, i32 0, i32 3
  // CHECK: store i8* bitcast (%swift.type* @_TMSS to i8*)
  // CHECK: getelementptr inbounds [6 x i8*], [6 x i8*]* %function-arguments, i32 0, i32 4
  // CHECK: store i8* bitcast (%swift.type* @_TMVs4Int8 to i8*)
  // CHECK: getelementptr inbounds [6 x i8*], [6 x i8*]* %function-arguments, i32 0, i32 5
  // CHECK: store %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @_TMT_, i32 0, i32 1)
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata(i8** [[T0]]) {{#[0-9]+}}
  arch({(x: inout Int, y: Double, z: String, w: Int8) -> () in })
}
