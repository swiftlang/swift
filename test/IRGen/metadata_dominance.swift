// RUN: %target-swift-frontend -emit-ir -primary-file %s | FileCheck %s

func use_metadata<F>(f: F) {}

func voidToVoid() {}
func intToInt(x: Int) -> Int { return x }

func cond() -> Bool { return true }

// CHECK: define hidden void @_TF18metadata_dominance5test1FT_T_()
func test1() {
// CHECK: call i1 @_TF18metadata_dominance4condFT_Sb()
  if cond() {
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    use_metadata(voidToVoid)
// CHECK: call i1 @_TF18metadata_dominance4condFT_Sb()
// CHECK-NOT: @_TMaFT_T_
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    if cond() {
      use_metadata(voidToVoid)
    } else {
// CHECK-NOT: @_TMaFT_T_
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
      use_metadata(voidToVoid)
    }
  }
// CHECK: [[T1:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T1]])
  use_metadata(voidToVoid)
}

// CHECK: define hidden void @_TF18metadata_dominance5test2FT_T_()
func test2() {
// CHECK: call i1 @_TF18metadata_dominance4condFT_Sb()
  if cond() {
// CHECK: call i1 @_TF18metadata_dominance4condFT_Sb()
// CHECK: [[T0:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T0]])
    if cond() {
      use_metadata(voidToVoid)
    } else {
// CHECK: [[T1:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T1]])
      use_metadata(voidToVoid)
    }
  }
// CHECK: [[T2:%.*]] = call %swift.type* @_TMaFT_T_()
// CHECK: call void @_TF18metadata_dominance12use_metadataurFxT_(%swift.opaque* {{.*}}, %swift.type* [[T2]])
  use_metadata(voidToVoid)
}
