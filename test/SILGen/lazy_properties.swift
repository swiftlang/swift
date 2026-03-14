// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -Xllvm -sil-full-demangle -parse-as-library -primary-file %s | %FileCheck %s

// <rdar://problem/17405715> lazy property crashes silgen of implicit memberwise initializer

// CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s15lazy_properties19StructWithLazyFieldV4onceSivg : $@convention(method) (@inout StructWithLazyField) -> Int

// CHECK-LABEL: sil hidden [ossa] @$s15lazy_properties19StructWithLazyFieldV4onceSivs : $@convention(method) (Int, @inout StructWithLazyField) -> ()

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s15lazy_properties19StructWithLazyFieldV4onceSivM : $@yield_once @convention(method) (@inout StructWithLazyField) -> @yields @inout Int

// CHECK-LABEL: sil hidden [ossa] @$s15lazy_properties19StructWithLazyFieldV4onceACSiSg_tcfC : $@convention(method) (Optional<Int>, @thin StructWithLazyField.Type) -> @owned StructWithLazyField

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

// CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s15lazy_properties22HasAnonymousParametersV1xSivg : $@convention(method) (@inout HasAnonymousParameters) -> Int

// CHECK-LABEL: sil private [ossa] @$s15lazy_properties22HasAnonymousParametersV1xSivgS2iXEfU_ : $@convention(thin) (Int) -> Int

// CHECK-LABEL: sil hidden [ossa] @$s15lazy_properties22HasAnonymousParametersV1xSivs : $@convention(method) (Int, @inout HasAnonymousParameters) -> ()

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s15lazy_properties22HasAnonymousParametersV1xSivM : $@yield_once @convention(method) (@inout HasAnonymousParameters) -> @yields @inout Int

struct HasAnonymousParameters {
  lazy var x = { $0 }(0)
}

class LazyClass {
  lazy var x = 0
}

// CHECK-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s15lazy_properties9LazyClassC1xSivg : $@convention(method) (@guaranteed LazyClass) -> Int
// CHECK: ref_element_addr %0 : $LazyClass, #LazyClass.$__lazy_storage_$_x
// CHECK: return

// CHECK-LABEL: sil hidden [ossa] @$s15lazy_properties9LazyClassC1xSivs : $@convention(method) (Int, @guaranteed LazyClass) -> ()
// CHECK: ref_element_addr %1 : $LazyClass, #LazyClass.$__lazy_storage_$_x
// CHECK: return

// rdar://problem/53956342
class Butt {
  func foo() -> Int { return 0 }

  lazy var butt: Int = {
    func bar() -> Int{
      return foo()
    }
    return bar()
  }()
}

// Both the closure and the local function inside of it should capture 'self':

// CHECK-LABEL: sil private [ossa] @$s15lazy_properties4ButtC4buttSivgSiyXEfU_ : $@convention(thin) (@guaranteed Butt) -> Int
// CHECK-LABEL: sil private [ossa] @$s15lazy_properties4ButtC4buttSivgSiyXEfU_3barL_SiyF : $@convention(thin) (@guaranteed Butt) -> Int
