// RUN: %target-swift-emit-silgen -enable-sil-opaque-values -Xllvm -sil-full-demangle -enable-library-evolution %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime


// CHECK-LABEL: sil [ossa] @$s30opaque_values_silgen_resilient10OneOfTheseO4hash4intoys6HasherVz_tF : {{.*}} {
// CHECK:       {{bb[0-9]+}}({{%[^,]+}} : $*Hasher, [[ENUM:%[^,]+]] : @guaranteed $OneOfThese):
// CHECK:         switch_enum [[ENUM]] : $OneOfThese, case #OneOfThese.loadable!enumelt: [[LOADABLE:bb[0-9]+]], case #OneOfThese.resilient!enumelt: [[RESILIENT:bb[0-9]+]]
// CHECK:       [[LOADABLE]]([[LOADABLE_BOX:%[^,]+]] :
// CHECK:         [[LOADABLE_ADDR:%[^,]+]] = project_box [[LOADABLE_BOX]]
// CHECK:         load_borrow [[LOADABLE_ADDR]] : $*OneOfThese.Loadable
// CHECK:       [[RESILIENT]]([[RESILIENT_BOX:%[^,]+]] :
// CHECK:         [[RESILIENT_ADDR:%[^,]+]] = project_box [[RESILIENT_BOX]]
// CHECK:         load_borrow [[RESILIENT_ADDR]] : $*OneOfThese.Resilient
// CHECK-LABEL: } // end sil function '$s30opaque_values_silgen_resilient10OneOfTheseO4hash4intoys6HasherVz_tF'

@frozen
public indirect enum OneOfThese : Hashable {
  @frozen
  public struct Loadable : Hashable {
    var o: AnyHashable
    var i: Int
  }
  case loadable(Loadable)
  public struct Resilient : Hashable {
    var o: AnyHashable
    var i: Int
  }
  case resilient(Resilient)
}


