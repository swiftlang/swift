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

func test_disfavored_vs_unavailable(_ view: View) {
  view.frame(width: 100) // Ok
  // CHECK: function_ref @$s29availability_with_overloading4ViewPAAE5frame5width6heightySdSg_AGtF
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

func test_generic_vs_unavailable(_ s: S) {
  s.foo("") // Ok (picks available generic overload)
  // CHECK: function_ref @$s29availability_with_overloading1SV3fooyyxSyRzlF
  s.bar(42) // Ok (picks overload with defaulted argument)
  // CHECK: function_ref @$s29availability_with_overloading1SV3baryySi_SitF
}
