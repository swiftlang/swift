// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -parse-as-library -emit-silgen -primary-file %s | %FileCheck %s

// <rdar://problem/17405715> lazy property crashes silgen of implicit memberwise initializer
// CHECK-LABEL: // lazy_properties.StructWithLazyField.init
// CHECK-NEXT: sil hidden @_T015lazy_properties19StructWithLazyFieldVACSiSg4once_tcfC : $@convention(method) (Optional<Int>, @thin StructWithLazyField.Type) -> @owned StructWithLazyField {
struct StructWithLazyField {
  lazy var once : Int = 42
  let someProp = "Some value"
}

// <rdar://problem/21057425> Crash while compiling attached test-app.
// CHECK-LABEL: // lazy_properties.test21057425
func test21057425() {
  var x = 0, y: Int = 0
}

// Anonymous closure parameters in lazy initializer crash SILGen

// CHECK-LABEL: sil hidden @_T015lazy_properties22HasAnonymousParametersV1xSifg : $@convention(method) (@inout HasAnonymousParameters) -> Int
// CHECK-LABEL: sil private @_T015lazy_properties22HasAnonymousParametersV1xSifgS2icfU_ : $@convention(thin) (Int) -> Int

struct HasAnonymousParameters {
  lazy var x = { $0 }(0)
}
