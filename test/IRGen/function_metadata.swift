// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-ir -primary-file %s | %FileCheck %s

func arch<F>(_ f: F) {}

// CHECK: define hidden swiftcc void @"$s17function_metadata9test_archyyF"()
func test_arch() {
  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$syycMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata0([[WORD:i(32|64)]] 67108864, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch( {() -> () in } )

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySicMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD:i(32|64)]] 67108865, %swift.type* @"$sSiN", %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1)) {{#[0-9]+}}
  arch({(x: Int) -> () in  })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$syyt_tcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 67108865, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(_: ()) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySizcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata([[WORD]] 100663297, %swift.type** {{%.*}}, i32* getelementptr inbounds ([1 x i32], [1 x i32]* @parameter-flags, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(x: inout Int) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySi_Sft_tcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1([[WORD]] 67108865, %swift.type* {{%.*}}, %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(x: (Int, Float)) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySiz_SitcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata([[WORD]] 100663298, %swift.type** {{%.*}}, i32* getelementptr inbounds ([2 x i32], [2 x i32]* @parameter-flags.16, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(x: inout Int, y: Int) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySf_SitcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata2([[WORD]] 67108866, %swift.type* @"$sSfN", %swift.type* @"$sSiN", %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(a: Float, b: Int) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySiz_SfSStcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata([[WORD]] 100663299, %swift.type** {{%.*}}, i32* getelementptr inbounds ([3 x i32], [3 x i32]* @parameter-flags.23, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(x: inout Int, y: Float, z: String) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySf_SfSitcMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata3([[WORD]] 67108867, %swift.type* @"$sSfN", %swift.type* @"$sSfN", %swift.type* @"$sSiN", %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(a: Float, b: Float, c: Int) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$sySiz_SdSSs4Int8VtcMa"
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 0
  // CHECK: store %swift.type* @"$sSiN", %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 1
  // CHECK: store %swift.type* @"$sSdN", %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 2
  // CHECK: store %swift.type* @"$sSSN", %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: getelementptr inbounds [4 x %swift.type*], [4 x %swift.type*]* %function-parameters, i32 0, i32 3
  // CHECK: store %swift.type* @"$ss4Int8VN", %swift.type** [[T:%.*]], align [[ALIGN:(4|8)]]
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata([[WORD]] 100663300, %swift.type** {{%.*}}, i32* getelementptr inbounds ([4 x i32], [4 x i32]* @parameter-flags.{{.*}}, i32 0, i32 0), %swift.type* getelementptr inbounds (%swift.full_type, %swift.full_type* @"$sytN", i32 0, i32 1))
  arch({(x: inout Int, y: Double, z: String, w: Int8) -> () in })

  // CHECK-LABEL: define{{( protected)?}} linkonce_odr hidden swiftcc %swift.metadata_response @"$syyyccMa"
  // CHECK: call %swift.type* @swift_getFunctionTypeMetadata1(i64 67108865
  arch({(x: @escaping () -> ()) -> () in })
}
