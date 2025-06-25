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

// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion038test_various_situations_converting_to_C0yyF : $@convention(thin) () -> () {
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
  // CHECK-LABEL: sil private [ossa] @$s34implicit_double_cgfloat_conversion038test_various_situations_converting_to_C0yyF0E23_loading_tuple_elementsL_6valuesy12CoreGraphics7CGFloatV_AGtz_tF : $@convention(thin) (@inout (CGFloat, CGFloat)) -> () {
  func test_loading_tuple_elements(values: inout (CGFloat, CGFloat)) {
    struct S {
      init(x: Double, y: Double) {}
      init(x: CGFloat, y: CGFloat) {}
    }

    // CHECK: function_ref @$s34implicit_double_cgfloat_conversion038test_various_situations_converting_to_C0yyF0E23_loading_tuple_elementsL_6valuesy12CoreGraphics7CGFloatV_AGtz_tF1SL_V1x1yAiG_AGtcfC : $@convention(method) (CGFloat, CGFloat, @thin S.Type) -> S
    _ = S(x: 0.0, y: values.0) // Ok
  }
}

// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion038test_various_situations_converting_to_B0yyF : $@convention(thin) () -> () {
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

// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion31test_conversions_with_optionals1vy12CoreGraphics7CGFloatVSg_tF : $@convention(thin) (Optional<CGFloat>) -> () {
func test_conversions_with_optionals(v: CGFloat?) {
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion31test_conversions_with_optionals1vy12CoreGraphics7CGFloatVSg_tFAFyKXEfu_
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC : $@convention(method) (CGFloat, @thin Double.Type) -> Double
  let _: Double = (v ?? 0)
}

// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion48test_static_members_are_contextually_convertibleyyF : $@convention(thin) () -> () {
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

// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF : $@convention(thin) (Double, CGFloat) -> () {
func test_narrowing_is_delayed(x: Double, y: CGFloat) {
  func test(_: CGFloat) {}

  func overloaded(_: Double, _: Double) -> Double {}
  func overloaded(_: CGFloat, _: CGFloat) -> CGFloat {}

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$sSd1doiyS2d_SdtFZ
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC
  let _: CGFloat = x / y // CGFloat.init(x / Double.init(y))
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$sSd1doiyS2d_SdtFZ
  // CHECK: function_ref @$sSd22_builtinIntegerLiteralSdBI_tcfC
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC
  let _: CGFloat = x / y + 1 // CGFloat.init(x / Double(y) + 1 as Double)
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF10overloadedL_yS2d_SdtF
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcf
  let _: CGFloat = overloaded(x, y) // Prefers `overloaded(Double, Double) -> Double`
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF10overloadedL_yS2d_SdtF
  // CHECK: @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF10overloadedL_yS2d_SdtF
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcf
  let _: CGFloat = overloaded(x, overloaded(x, y)) // Prefers `overloaded(Double, Double) -> Double` in both occurrences.

  // Calls should behave exactly the same as contextual conversions.

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$sSd1doiyS2d_SdtFZ
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC
  test(x / y)
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$sSd1doiyS2d_SdtFZ
  // CHECK: function_ref @$sSd22_builtinIntegerLiteralSdBI_tcfC
  // CHECK: function_ref @$sSd1poiyS2d_SdtFZ
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC
  test(x / y + 1)
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF10overloadedL_yS2d_SdtF
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcf
  test(overloaded(x, y))
  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF10overloadedL_yS2d_SdtF
  // CHECK: @$s34implicit_double_cgfloat_conversion25test_narrowing_is_delayed1x1yySd_12CoreGraphics7CGFloatVtF10overloadedL_yS2d_SdtF
  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcf
  test(overloaded(x, overloaded(x, y)))
}

extension CGFloat {
  static func /(_: CGFloat, _: CGFloat) -> CGFloat { fatalError() }

  static prefix func -(_: Self) -> Self { fatalError() }
}

// Make sure that solution with no Double/CGFloat conversions is preferred
// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion38test_no_ambiguity_with_unary_operators5width6heighty12CoreGraphics7CGFloatV_AGtF : $@convention(thin) (CGFloat, CGFloat) -> () {
func test_no_ambiguity_with_unary_operators(width: CGFloat, height: CGFloat) {
  struct R {
    init(x: CGFloat, y: CGFloat, width: CGFloat, height: CGFloat) {}
    init(x: Double,  y: Double,  width: Double,  height: Double) {}
    init(x: Int,     y: Int,     width: Int,     height: Int) {}
  }

  // CHECK: function_ref @$s12CoreGraphics7CGFloatV34implicit_double_cgfloat_conversionE1doiyA2C_ACtFZ
  // CHECK: function_ref @$s34implicit_double_cgfloat_conversion38test_no_ambiguity_with_unary_operators5width6heighty12CoreGraphics7CGFloatV_AGtF1RL_V1x1yAcdiG_A3GtcfC
  _ = R(x: width / 4, y: -height / 2, width: width, height: height)
}

// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion40test_conversions_with_optional_promotion1d3cgfySd_12CoreGraphics7CGFloatVtF : $@convention(thin) (Double, CGFloat) -> () {
func test_conversions_with_optional_promotion(d: Double, cgf: CGFloat) {
  func test_double(_: Double??, _: Double???) {}
  func test_cgfloat(_: CGFloat??, _: CGFloat???) {}

  // CHECK: function_ref @$sSd12CoreGraphicsEySdAA7CGFloatVcfC
  // CHECK-NEXT: apply
  // CHECK-NEXT: enum $Optional<Double>, #Optional.some!enumelt
  // CHECK-NEXT: enum $Optional<Optional<Double>>, #Optional.some!enumelt
  test_double(cgf, cgf)

  // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC
  // CHECK-NEXT: apply
  // CHECK-NEXT: enum $Optional<CGFloat>, #Optional.some!enumelt
  // CHECK-NEXT: enum $Optional<Optional<CGFloat>>, #Optional.some!enumelt
  test_cgfloat(d, d)
}

// https://github.com/apple/swift/issues/59374
func test_multi_argument_conversion_with_optional(d: Double, cgf: CGFloat) {
  func test(_: Double, _: CGFloat?) {}

  test(cgf, d) // Ok (CGFloat -> Double and Double? -> CGFloat?)
}

extension CGFloat: @retroactive Hashable {
  public func hash(into hasher: inout Hasher) { fatalError() }
}

func test_collection_literals_as_call_arguments() {
  enum E {
    case test_arr([CGFloat])
    case test_dict_key([CGFloat: String])
    case test_dict_value([String: CGFloat])
    case test_arr_nested([String: [[CGFloat]: String]])
    case test_dict_nested([String: [String: CGFloat]])
  }

  struct Container {
    var prop: E
  }

  struct Point {
    var x: Double
    var y: Double
  }

  func test(cont: inout Container, point: Point) {
    cont.prop = .test_arr([point.x]) // Ok
    cont.prop = .test_dict_key([point.y: ""]) // Ok
    cont.prop = .test_dict_value(["": point.y]) // Ok
    cont.prop = .test_arr_nested(["": [[point.x]: ""]]) // Ok
    cont.prop = .test_dict_nested(["": ["": point.x]]) // Ok
  }
}

func assignments_with_and_without_optionals() {
  class C {
    var prop: CGFloat = 0
  }

  func test(c: C?, v: Double, cgf: CGFloat) {
    c?.prop = v / 2.0 // Ok
    c?.prop = (false ? cgf : v)

    let copy = c!
    copy.prop = Optional(v) ?? 0 // Ok
    copy.prop = (true ? cgf : (false ? v : cgf))
  }
}

extension CGFloat {
  static let `default` = 42.0
}

// rdar://97261826 - crash during constraint application with leading-dot syntax
func assignment_with_leading_dot_syntax() {
  class Container {
    var prop: CGFloat = 0
  }

  struct Test {
    let test: Void = {
      let c = Container()
      c.prop = .default // Ok (Double -> CGFloat)
    }()
  }
}

func test_conversion_inside_tuple_elements() -> (a: CGFloat, b: (c: Int, d: CGFloat)) {
  let x: Double = 0.0
  return (a: x, b: (c: 42, d: x)) // Ok
}

do {
  struct Data {
    var prop: CGFloat
  }

  func single(get: () -> Double) {}
  func multiple(get1: () -> Double,
                get2: () -> CGFloat = { Double(1) },
                get3: () -> Double) {}

  func test(data: Data) {
    single { data.prop } // Ok
    single { return data.prop } // Ok

    single {
      _ = 42
      if true {
        return data.prop // Ok
      }
      return data.prop // Ok
    }

    multiple {
      data.prop // Ok
    } get3: {
      return data.prop // Ok
    }
  }
}

// rdar://99282938
func test_implicit_conversion_clash_with_partial_application_check() {
  class C {
    var duration: CGFloat { 0.3 }

    var use: Double {
      duration // Ok
    }

    func transitionDuration() -> TimeInterval {
      duration // Ok
    }
  }
}

// rdar://99352676
// CHECK-LABEL: sil hidden [ossa] @$s34implicit_double_cgfloat_conversion20test_init_validationyyF : $@convention(thin) () -> () {
func test_init_validation() {
  class Foo {
    static let bar = 100.0

    func getBar() -> CGFloat? {
      return Self.bar
      // CHECK: function_ref @$s12CoreGraphics7CGFloatVyACSdcfC : $@convention(method) (Double, @thin CGFloat.Type) -> CGFloat
    }
  }
}

func test_ternary_and_nil_coalescing() {
  func test(_: Double?) {}

  func ternary(v: CGFloat) {
    test(true ? v : nil) // Ok
  }

  func test_nil_coalescing(v: CGFloat?) {
    test(v ?? 0.0) // Ok
  }
}

do {
  struct G<T> {
    init(_: T) {}
  }

  func round(_: Double) -> Double {}
  func round<T: FloatingPoint>(_: T) -> T {}

  func test_cgfloat_over_double(withColors colors: Int, size: CGSize) -> G<CGFloat> {
    let g = G(1.0 / CGFloat(colors))
    return g // Ok
  }

  func test_no_ambiguity(width: Int, height: Int) -> CGFloat {
    let v = round(CGFloat(width / height) * 10) / 10.0
    return v // Ok
  }
}

func test_cgfloat_operator_is_attempted_with_literal_arguments(v: CGFloat?) {
  let ratio = v ?? (2.0 / 16.0)
  let _: CGFloat = ratio // Ok
}
