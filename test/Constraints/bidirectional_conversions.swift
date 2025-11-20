// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation
import CoreGraphics

/////////////

struct G<T> {
  var t: T
}

func foo1(x: (x: Int, y: Int)?, y: (Int, Int)) -> G<(x: Int, y: Int)> {
  let g = G(t: x ?? y)
  return g
}

func foo2(x: (Int, Int)?, y: (x: Int, y: Int)) -> G<(Int, Int)> {
  let g = G(t: x ?? y)
  return g
}

func foo3(x: (@convention(block) () -> ())?, y: @escaping () -> ()) -> G<@convention(block) () -> ()> {
  let g = G(t: x ?? y)
  return g
}

func foo4(x: (() -> ())?, y: @escaping @convention(block) () -> ()) -> G<() -> ()> {
  let g = G(t: x ?? y)
  return g
}

func foo5(x: CGFloat?, y: Double) -> G<CGFloat> {
  let g = G(t: x ?? y)
  return g
}

func foo6(x: Double?, y: CGFloat) -> G<Double> {
  let g = G(t: x ?? y)
  return g
}

/////////////

func id<T>(_: T) -> T {}

func bar1(x: (x: Int, y: Int)) {
  func f(_: (Int, Int)) {}
  f(id(x))
}

func bar2(x: (Int, Int)) {
  func f(_: (x: Int, y: Int)) {}
  f(id(x))
}

func bar3(x: @escaping () -> ()) {
  func f(_: @escaping @convention(block) () -> ()) {}
  // FIXME
  f(id(x))  // expected-error {{conflicting arguments to generic parameter 'T' ('@convention(block) () -> ()' vs. '() -> ()')}}
}

func bar4(x: @escaping @convention(block) () -> ()) {
  func f(_: @escaping () -> ()) {}
  // FIXME
  f(id(x))  // expected-error {{conflicting arguments to generic parameter 'T' ('() -> ()' vs. '@convention(block) () -> ()')}}
}

func bar5(x: Double) {
  func f(_: CGFloat) {}
  f(id(x))
}

func bar6(x: CGFloat) {
  func f(_: Double) {}
  f(id(x))
}

/////////////

func unwrap<T>(_: T?) -> T {}

func baz1(x: (x: Int, y: Int)?) {
  func f(_: (Int, Int)) {}
  f(unwrap(x))
}

func baz2(x: (Int, Int)?) {
  func f(_: (x: Int, y: Int)) {}
  f(unwrap(x))
}

func baz3(x: (() -> ())?) {
  func f(_: @escaping @convention(block) () -> ()) {}
  f(unwrap(x))
}

func baz4(x: (@convention(block) () -> ())?) {
  func f(_: @escaping () -> ()) {}
  f(unwrap(x))
}

func baz5(x: Double?) {
  func f(_: CGFloat) {}
  f(unwrap(x))
}

func baz6(x: CGFloat?) {
  func f(_: Double) {}
  f(unwrap(x))
}

