// RUN: %target-typecheck-verify-swift %clang-importer-sdk
// RUN: %target-swift-emit-silgen(mock-sdk: %clang-importer-sdk) -swift-version 5 -verify %s | %FileCheck %s

// REQUIRES: objc_interop

import Foundation
import CoreGraphics

func test_to_cgfloat(_: CGFloat) {}
func test_from_cgfloat(_: Double) {}

func test_returns_double(_: CGFloat) -> Double {
  42.0
}

func test_returns_cgfloat(_: Double) -> CGFloat {
  42.0
}

let d: Double    = 0.0
let cgf: CGFloat = 0.0

// CHECK: test_various_situations_converting_to_cgfloat()
func test_various_situations_converting_to_cgfloat() {
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  let _: CGFloat = d

  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion08test_to_C0yy12CoreGraphics7CGFloatVF : $@convention(thin) (CGFloat) -> ()
  test_to_cgfloat(d)

  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  test_to_cgfloat(d + d) // One implicit conversion, `+` is (Double, Double) -> Double

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  test_to_cgfloat(d + cgf) // Two conversions but `+` on Double is still preferred.

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1soiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  test_to_cgfloat(d + cgf - d) // One conversion (`cgf` -> Double) inside and one outside, both operators preferred on Double.

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1soiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  test_to_cgfloat(d + cgf - cgf) // Double is always preferred over CGFloat, so three conversion here.

  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion013test_returns_B0ySd12CoreGraphics7CGFloatVF : $@convention(thin) (CGFloat) -> Double
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion08test_to_C0yy12CoreGraphics7CGFloatVF : $@convention(thin) (CGFloat) -> ()
  test_to_cgfloat(test_returns_double(d)) // Two conversions

  // Overloads with CGFloat are preferred if that allows to avoid any implicit conversions.
  func test_loading_tuple_elements(values: inout (CGFloat, CGFloat)) {
    struct S {
      init(x: Double, y: Double) {}
      init(x: CGFloat, y: CGFloat) {}
    }

    // CHECK: function_ref @$s34implicit_double_cgfloat_conversion038test_various_situations_converting_to_C0yyF0E23_loading_tuple_elementsL_6valuesy12CoreGraphics7CGFloatV_AGtz_tF1SL_V1x1yAiG_AGtcfC : $@convention(method) (CGFloat, CGFloat, @thin S.Type) -> S
    _ = S(x: 0.0, y: values.0) // Ok
  }
}

// CHECK: test_various_situations_converting_to_double()
func test_various_situations_converting_to_double() {
  // function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  let _: Double = cgf

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion010test_from_C0yySdF : $@convention(thin) (Double) -> ()
  test_from_cgfloat(cgf)
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  test_from_cgfloat(cgf + cgf) // Two conversions for `cgf` and `+` is on `Double`
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  test_from_cgfloat(d + cgf) // One conversion `cgf` to Double and `+` is on `Double`
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1soiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  test_from_cgfloat(cgf + d - cgf) // Two conversions of `cgf` and both `+` and `-` are on Double
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$sSd1soiyS2d_SdtFZ : $@convention(method) (Double, Double, @thin Double.Type) -> Double
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion010test_from_C0yySdF : $@convention(thin) (Double) -> ()
  test_from_cgfloat(cgf + d - d) // One conversion and both operators are on Double

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion013test_returns_C0y12CoreGraphics7CGFloatVSdF : $@convention(thin) (Double) -> CGFloat
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion010test_from_C0yySdF : $@convention(thin) (Double) -> ()
  test_from_cgfloat(test_returns_cgfloat(cgf)) // Two conversions - argument and result.
}

func test_conversions_with_optionals(v: CGFloat?) {
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion31test_conversions_with_optionals1vy12CoreGraphics7CGFloatVSg_tFAFyKXEfu_ : $@convention(thin) () -> (CGFloat, @error Error)
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  let _: Double = (v ?? 0)
}

func test_static_members_are_contextually_convertible() {
  struct S {
    static var testProp: CGFloat { 42 }
    static func testFunc() -> CGFloat { 42 }
  }

  func test_prop(s: S) -> Double {
    // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
    return S.testProp // Ok
  }

  func test_method(s: S) -> Double {
    // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
    return S.testFunc() // Ok
  }
}
