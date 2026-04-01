// RUN: %target-typecheck-verify-swift -solver-enable-crash-on-valid-salvage
// RUN: %target-typecheck-verify-swift -DSALVAGE -solver-disable-crash-on-valid-salvage
// RUN: not --crash %target-typecheck-verify-swift -DSALVAGE -solver-enable-crash-on-valid-salvage

// REQUIRES: objc_interop

// Note this cannot use a fake Foundation because it lacks required operator overloads

import Foundation
import CoreGraphics

do {
  func test(a: CGFloat, b: CGFloat) {
    let _: Double = a + b + 1 // Ok
    let _: Double = a + b + 1.0 // Ok

    var test: Double

    test = a + b + 1 // Ok
    test = a + b + 1.0 // Ok

    _ = test

    let _ = a + b + 1 // Ok
    let _ = a + b + 1.0 // Ok

    let _: Double = 1 + 2 + 3 // Ok

    test = 1 + 2 + 3 // Ok
  }
}

func test(cond: Bool, a: CGFloat, b: CGFloat) {
  var test: Double

  let width = a - b // CGFloat

  if cond {
    test = (width - 10) / 2 // Ok
  } else {
    test = (width - 20.0) / 3 // Ok
  }

  _ = test
}

func test_atan_ambiguity(points: (CGPoint, CGPoint)) {
  var test = 0.0
  test = atan((points.1.y - points.0.y) / (points.1.x - points.0.x)) // Ok
  _ = test
}

func test_ambigity_with_generic_funcs(a: CGFloat, b: CGFloat) -> [CGFloat] {
  let result = [round(abs(a - b) * 100) / 100.0]
  return result
}

func testMultipleClosureInference(_ d: Double, i: Int) {
  struct S<T> {
    init(_ get: () -> T, _ set: (T) -> Void) {}
  }
  func foo<T>(_: S<T>, _: T) {}
  foo(S({ CGFloat(i) }, { _ in }), d)
}

struct JoinIs<T> {}

func testOptionalJoin(x1: Double, x2: CGFloat, y1: Double?, y2: CGFloat?) {
  func same<T>(_: T, _: T) -> JoinIs<T> {}

  let _: JoinIs<Double?> = same(x1, y1)

  // FIXME: This violates the proposal's "widening conversion preferred" rule.
  // We should be converting y2 to Double?, instead of x1 to CGFloat.
  let _: JoinIs<CGFloat?> = same(x1, y2)

  let _: JoinIs<Double?> = same(x2, y1)
  let _: JoinIs<CGFloat?> = same(x2, y2)
}


// Regression tests reduced from projects.

func testOptionalVsNonOptionalBindings() {
  func f(x: Double?, y: Double, b: Bool, z: Int?) {
    let _: Double? = x != nil ? x! + y : x
    let _: Double? = z != nil ? Double(z!) : x
    let _: Double? = b ? (y - 0) / 1 : x
  }

  func f(x: CGFloat?, y: CGFloat, b: Bool, z: Int?) {
    let _: CGFloat? = x != nil ? x! + y : x
    let _: CGFloat? = z != nil ? CGFloat(z!) : x
    let _: CGFloat? = b ? (y - 0) / 1 : x
  }
}

extension Double {
  static let x: CGFloat = 0
  // expected-note@-1 10{{'x' declared here}}
}

extension CGFloat {
  static let y: Double = 0
}

// FIXME: This is all completely broken.
func testLeadingDotAmbiguity() {
  func f1(_: CGFloat, _: CGFloat) {}
  func f2(_: CGFloat, _: CGFloat?) {}
  func f3(_: CGFloat?, _: CGFloat) {}
  func f4(_: CGFloat?, _: CGFloat?) {}

  func f5(_: Double, _: CGFloat) {}
  func f6(_: Double, _: CGFloat?) {}
  func f7(_: Double?, _: CGFloat) {}
  func f8(_: Double?, _: CGFloat?) {}

  func f9(_: Double, _: Double) {}
  func f10(_: Double, _: Double?) {}
  func f11(_: Double?, _: Double) {}
  func f12(_: Double?, _: Double?) {}

  func f13(_: CGFloat, _: Double) {}
  func f14(_: CGFloat, _: Double?) {}
  func f15(_: CGFloat?, _: Double) {}
  func f16(_: CGFloat?, _: Double?) {}

  func test1(z: Double) {
    f1(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
#if SALVAGE
    f2(max(.x, z), max(.y, z))
#endif
    f3(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
#if SALVAGE
    f4(max(.x, z), max(.y, z))
#endif
    f5(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
#if SALVAGE
    f6(max(.x, z), max(.y, z))
#endif
    f7(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
#if SALVAGE
    f8(max(.x, z), max(.y, z))
#endif
    f9(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f10(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f11(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f12(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f13(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f14(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f15(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
    f16(max(.x, z), max(.y, z))  // expected-error {{type 'Double' has no member 'y'}}
  }

  func test2(z: Double) {
    f1(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f2(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
#if SALVAGE
    f3(max(.y, z), max(.x, z))
    f4(max(.y, z), max(.x, z))
#endif
    f5(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f6(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f7(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f8(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f9(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f10(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f11(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f12(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f13(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
    f14(max(.y, z), max(.x, z))  // expected-error {{type 'Double' has no member 'y'}}
#if SALVAGE
    f15(max(.y, z), max(.x, z))
    f16(max(.y, z), max(.x, z))
#endif
  }
}