// RUN: %target-swift-emit-silgen -primary-file %s | %FileCheck %s --check-prefix=SINGLE
// RUN: %target-swift-emit-silgen %s | %FileCheck %s --check-prefix=WHOLEMOD
// RUN: %target-swift-emit-sil -verify -primary-file %s

// https://github.com/apple/swift/issues/61128
// Ensure we only delay emission of functions that do not contain user code.

// We cannot delay functions that contain user code.
// SINGLE-LABEL: sil hidden [ossa] @$s17delayed_functions3fooSiyF : $@convention(thin) () -> Int
// WHOLEMOD-LABEL: sil hidden [ossa] @$s17delayed_functions3fooSiyF : $@convention(thin) () -> Int
func foo() -> Int { 5 }

// Cannot delay property initializers that contain user code.
struct R {
  // variable initialization expression of R.i
  // SINGLE-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1RV1iSivpfi : $@convention(thin) () -> Int
  // WHOLEMOD-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1RV1iSivpfi : $@convention(thin) () -> Int
  var i = 0

  // R.j.getter
  // SINGLE-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s17delayed_functions1RV1jSivg : $@convention(method) (@inout R) -> Int
  // WHOLEMOD-LABEL: sil hidden [lazy_getter] [noinline] [ossa] @$s17delayed_functions1RV1jSivg : $@convention(method) (@inout R) -> Int
  lazy var j = Int.max + 1 // expected-error {{results in an overflow}}
}

@propertyWrapper
struct Wrapper<T> {
  var wrappedValue: T
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
  init(wrappedValue: T, _ x: Int) {
    self.wrappedValue = wrappedValue
  }
}

struct Projected<T> {
  var value: T
}
@propertyWrapper
struct ProjectedWrapper<T> {
  var wrappedValue: T
  init(wrappedValue: T) {
    self.wrappedValue = wrappedValue
  }
  var projectedValue: Projected<T> {
    .init(value: wrappedValue)
  }
  init(projectedValue: Projected<T>) {
    self.wrappedValue = projectedValue.value
  }
}

struct S {
  // We can delay property wrapper backing initializers if they don't contain user code.
  // property wrapper backing initializer of S.i
  // SINGLE-LABEL: sil hidden [ossa] @$s17delayed_functions1SV1iSivpfP : $@convention(thin) (Int) -> Wrapper<Int>
  // WHOLEMOD-NOT: sil hidden [ossa] @$s17delayed_functions1SV1iSivpfP : $@convention(thin) (Int) -> Wrapper<Int>
  @Wrapper var i: Int

  // But not if they contain user code.
  // property wrapper backing initializer of S.j
  // SINGLE-LABEL: sil hidden [ossa] @$s17delayed_functions1SV1jSSvpfP : $@convention(thin) (@owned String) -> @owned Wrapper<String>
  // WHOLEMOD-LABEL: sil hidden [ossa] @$s17delayed_functions1SV1jSSvpfP : $@convention(thin) (@owned String) -> @owned Wrapper<String>

  // variable initialization expression of S._j
  // SINGLE-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1SV2_j33_6D0D158845E4BE9792202A0D16664A88LLAA7WrapperVySSGvpfi : $@convention(thin) () -> @owned String
  // WHOLEMOD-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1SV2_j33_6D0D158845E4BE9792202A0D16664A88LLAA7WrapperVySSGvpfi : $@convention(thin) () -> @owned String
  @Wrapper(5) var j = ""

  // variable initialization expression of S._k
  // SINGLE-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1SV2_k33_6D0D158845E4BE9792202A0D16664A88LLAA7WrapperVySiGvpfi : $@convention(thin) () -> Int
  // WHOLEMOD-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1SV2_k33_6D0D158845E4BE9792202A0D16664A88LLAA7WrapperVySiGvpfi : $@convention(thin) () -> Int
  @Wrapper var k = Int.max + 1 // expected-error 2{{results in an overflow}}

  // property wrapper backing initializer of S.l
  // SINGLE-LABEL: sil hidden [ossa] @$s17delayed_functions1SV1lSSvpfP : $@convention(thin) (@owned String) -> @owned Wrapper<Wrapper<String>>
  // WHOLEMOD-LABEL: sil hidden [ossa] @$s17delayed_functions1SV1lSSvpfP : $@convention(thin) (@owned String) -> @owned Wrapper<Wrapper<String>>

  // variable initialization expression of S._l
  // SINGLE-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1SV2_l33_6D0D158845E4BE9792202A0D16664A88LLAA7WrapperVyAGySSGGvpfi : $@convention(thin) () -> @owned String
  // WHOLEMOD-LABEL: sil hidden [transparent] [ossa] @$s17delayed_functions1SV2_l33_6D0D158845E4BE9792202A0D16664A88LLAA7WrapperVyAGySSGGvpfi : $@convention(thin) () -> @owned String
  @Wrapper
  @Wrapper(.random() ? 1 : 2)
  var l = ""
}

// The backing init for a projected wrapper can be delayed.
// property wrapper init from projected value of x #1 in takesProjected(_:)
// SINGLE-LABEL: sil hidden [ossa] @$s17delayed_functions14takesProjectedyyAA0D7WrapperVySiGF1xL_SivpfW : $@convention(thin) (Projected<Int>) -> ProjectedWrapper<Int>
// WHOLEMOD-NOT: sil hidden [ossa] @$s17delayed_functions14takesProjectedyyAA0D7WrapperVySiGF1xL_SivpfW : $@convention(thin) (Projected<Int>) -> ProjectedWrapper<Int>
func takesProjected(@ProjectedWrapper _ x: Int) {}

struct HasPrivate {
  // These backing initializers can be dropped entirely as they're private and
  // unused.

  // property wrapper backing initializer of x #1 in HasPrivate.testPrivateWrapper(x:)
  // SINGLE-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV04testD7Wrapper{{.*}}LL1xyAA09ProjectedF0VySiG_tFAFL_SivpfP : $@convention(thin) (Int) -> ProjectedWrapper<Int>
  // WHOLEMOD-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV04testD7Wrapper{{.*}}LL1xyAA09ProjectedF0VySiG_tFAFL_SivpfP : $@convention(thin) (Int) -> ProjectedWrapper<Int>

  // property wrapper init from projected value of x #1 in HasPrivate.testPrivateWrapper(x:)
  // SINGLE-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV04testD7Wrapper{{.*}}LL1xyAA09ProjectedF0VySiG_tFAFL_SivpfW : $@convention(thin) (Projected<Int>) -> ProjectedWrapper<Int>
  // WHOLEMOD-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV04testD7Wrapper{{.*}}LL1xyAA09ProjectedF0VySiG_tFAFL_SivpfW : $@convention(thin) (Projected<Int>) -> ProjectedWrapper<Int>

  // The function itself needs to be emitted because it can contain user code.
  // HasPrivate.testPrivateWrapper(x:)
  // SINGLE: sil private [ossa] @$s17delayed_functions10HasPrivateV04testD7Wrapper{{.*}}LL1xyAA09ProjectedF0VySiG_tF : $@convention(method) (ProjectedWrapper<Int>, HasPrivate) -> ()
  // WHOLEMOD: sil private [ossa] @$s17delayed_functions10HasPrivateV04testD7Wrapper{{.*}}LL1xyAA09ProjectedF0VySiG_tF : $@convention(method) (ProjectedWrapper<Int>, HasPrivate) -> ()
  private func testPrivateWrapper(@ProjectedWrapper x: Int) {}

  // property wrapper backing initializer of x #1 in HasPrivate.testFilePrivateWrapper(x:)
  // SINGLE-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV08testFileD7Wrapper{{.*}}LL1xyAA09ProjectedG0VySiG_tFAFL_SivpfP : $@convention(thin) (Int) -> ProjectedWrapper<Int>
  // WHOLEMOD-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV08testFileD7Wrapper{{.*}}LL1xyAA09ProjectedG0VySiG_tFAFL_SivpfP : $@convention(thin) (Int) -> ProjectedWrapper<Int>

  // property wrapper init from projected value of x #1 in HasPrivate.testFilePrivateWrapper(x:)
  // SINGLE-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV08testFileD7Wrapper{{.*}}LL1xyAA09ProjectedG0VySiG_tFAFL_SivpfW : $@convention(thin) (Projected<Int>) -> ProjectedWrapper<Int>
  // WHOLEMOD-NOT: sil private [ossa] @$s17delayed_functions10HasPrivateV08testFileD7Wrapper{{.*}}LL1xyAA09ProjectedG0VySiG_tFAFL_SivpfW : $@convention(thin) (Projected<Int>) -> ProjectedWrapper<Int>

  // The function itself needs to be emitted because it can contain user code.
  // HasPrivate.testFilePrivateWrapper(x:)
  // SINGLE: sil private [ossa] @$s17delayed_functions10HasPrivateV08testFileD7Wrapper{{.*}}LL1xyAA09ProjectedG0VySiG_tF : $@convention(method) (ProjectedWrapper<Int>, HasPrivate) -> ()
  // WHOLEMOD: sil private [ossa] @$s17delayed_functions10HasPrivateV08testFileD7Wrapper{{.*}}LL1xyAA09ProjectedG0VySiG_tF : $@convention(method) (ProjectedWrapper<Int>, HasPrivate) -> ()
  fileprivate func testFilePrivateWrapper(@ProjectedWrapper x: Int) {}
}
