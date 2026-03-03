// RUN: %target-typecheck-verify-swift
// REQUIRES: objc_interop

import Foundation
import CoreGraphics

/////////////

struct G<T> {
  var t: T
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

func bar3(x: @escaping () -> ()) {
  func f(_: @escaping @convention(block) () -> ()) {}
  f(id(x))
}

func bar4(x: @escaping @convention(block) () -> ()) {
  func f(_: @escaping () -> ()) {}
  f(id(x))
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

/////////////

func g(_: AnyObject?, _: AnyObject?) -> Bool {}
func g(_: AnyClass, _: AnyClass) -> Bool {}
func g(_: NSNull, _: NSNull) -> Bool {}

func f1(x: AnyObject, y: CGImage) -> Bool {
  return g(x, y)
}

func f2(x: AnyObject, y: CFString) -> Bool {
  return g(x, y)
}