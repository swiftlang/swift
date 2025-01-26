// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen %s -module-name main -experimental-skip-non-inlinable-function-bodies | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -module-name main -experimental-skip-non-inlinable-function-bodies-without-types | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen %s -module-name main -import-objc-header %S/Inputs/open_enum.h -debug-forbid-typecheck-prefix SKIP_ALL_NO_TYPECHECK -experimental-skip-all-function-bodies | %FileCheck %s --check-prefix=CHECK-SKIP-ALL

// CHECK-SKIP-ALL: sil_stage raw
// CHECK-SKIP-ALL-NOT: sil

public protocol P {
  @_borrowed var borrowedVar: Int { get }
}

func generateNumber() -> Int { return 1 }

public struct S {
  public var borrowedVar: Int

  public lazy var lazyVar: Int = generateNumber()

  // CHECK: sil [transparent] [serialized]{{.*}} @$s4main1SV7lazyVarSivM : $@yield_once @convention(method) (@inout S) -> @yields @inout Int
  // CHECK: end sil function '$s4main1SV7lazyVarSivM'

  // CHECK: sil [lazy_getter]{{.*}} @$s4main1SV7lazyVarSivg : $@convention(method) (@inout S) -> Int
  // CHECK-NOT: end sil function '$s4main1SV7lazyVarSivg'

  // CHECK: sil{{.*}} @$s4main1SV7lazyVarSivs : $@convention(method) (Int, @inout S) -> ()
  // CHECK-NOT: end sil function '$s4main1SV7lazyVarSivs'

  // CHECK: sil [transparent]{{.*}} @$s4main1SV018$__lazy_storage_$_B3Var33_39316373847D37F82BD23977A13DEF23LLSiSgvpfi : $@convention(thin) () -> Optional<Int>
  // CHECK: end sil function '$s4main1SV018$__lazy_storage_$_B3Var33_39316373847D37F82BD23977A13DEF23LLSiSgvpfi'

}

/// Since `borrowedVar` implements a `@_borrowed` requirement of `P` the synthesized
/// `_read` accessor has forced static dispatch and is a serialized function
/// even though it is not `@_transparent`.
extension S: P {}

// CHECK: sil shared [serialized]{{.*}} @$s4main1SV11borrowedVarSivr : $@yield_once @convention(method) (S) -> @yields Int {
// CHECK:   yield
// CHECK: } // end sil function '$s4main1SV11borrowedVarSivr'

// We type-check this function since it has a nested type, but we don't
// type-check the implicit lazy getter for 'x'. As such, the nested autoclosure
// won't have captures computed. Make sure we don't attempt to query the
// captures.
func testAutoclosureInLazyVar(_ y: Int?) {
  struct R {}
  lazy var x = y ?? 0
}
