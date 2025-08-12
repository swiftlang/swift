// RUN: %target-swift-frontend -emit-sil -verify %s | %FileCheck %s

// REQUIRES: OS=macosx

protocol View {}

extension View {
  @_disfavoredOverload
  func frame(width: Double?, height: Double? = nil) {
  }
}

@available(macOS 999, *)
extension View {
  func frame(width: Double?) {
  }
}

// CHECK-LABEL: sil {{.*}} @$s29availability_with_overloading30test_disfavored_vs_unavailableyyAA4View_pF :
func test_disfavored_vs_unavailable(_ view: View) {
  view.frame(width: 100) // Ok
  // CHECK: function_ref @$s29availability_with_overloading4ViewPAAE5frame5width6heightySdSg_AGtF :
}

struct S {
  func foo<T: StringProtocol>(_: T) {}
  func bar(_: Int, _: Int = 0) {}
}

@available(macOS 999, *)
extension S {
  func foo(_: String) {}
  func bar(_: Int) {}
}

// CHECK-LABEL: sil {{.*}} @$s29availability_with_overloading27test_generic_vs_unavailableyyAA1SVF :
func test_generic_vs_unavailable(_ s: S) {
  s.foo("") // Ok (picks available generic overload)
  // CHECK: function_ref @$s29availability_with_overloading1SV3fooyyxSyRzlF :
  s.bar(42) // Ok (picks overload with defaulted argument)
  // CHECK: function_ref @$s29availability_with_overloading1SV3baryySi_SitF :
}

extension S {
  @available(macOS, obsoleted: 999)
  func baz(_: Int) {}

  @available(macOS, introduced: 999)
  func baz(_: Int, _: Int = 0) {}
}

// CHECK-LABEL: sil {{.*}} @$s29availability_with_overloading28test_introduced_vs_obsoletedyyAA1SVF :
func test_introduced_vs_obsoleted(_ s: S) {
  // CHECK: function_ref @$s29availability_with_overloading1SV3bazyySiF :
  s.baz(42)
  // CHECK: function_ref @$ss26_stdlib_isOSVersionAtLeastyBi1_Bw_BwBwtF :
  if #available(macOS 999, *) {
    // CHECK: function_ref @$s29availability_with_overloading1SV3bazyySiF :
    s.baz(42)
  }
}

// CHECK-LABEL: sil {{.*}} @$s29availability_with_overloading29test_introduced_vs_obsoleted2yyAA1SVF :
@available(macOS 999, *)
func test_introduced_vs_obsoleted2(_ s: S) {
  // CHECK: function_ref @$s29availability_with_overloading1SV3bazyySiF :
  s.baz(42)
}
