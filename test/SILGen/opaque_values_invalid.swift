// RUN: %target-swift-emit-silgen -verify -enable-sil-opaque-values -Xllvm -sil-full-demangle %s | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-%target-runtime

func take<T>(_ t: T) {}

// CHECK-LABEL: sil hidden [ossa] @$s21opaque_values_invalid27forward_capture_generic_let4withSayxGx_tlF : {{.*}} {
// CHECK:         [[F:%[^,]+]] = function_ref @$s21opaque_values_invalid27forward_capture_generic_let4withSayxGx_tlF1fL_yylF {{.*}}
// CHECK:         [[F]]<U>(undef)
// CHECK-LABEL: } // end sil function '$s21opaque_values_invalid27forward_capture_generic_let4withSayxGx_tlF'
func forward_capture_generic_let<U>(with u: U) -> [U] {
  func f() { g() } // expected-error {{closure captures 'x' before it is declared}}
  f()
  let x = u // expected-note{{captured value declared here}}
  func g() { 
    take(x) // expected-note {{captured here}}
  }
}

