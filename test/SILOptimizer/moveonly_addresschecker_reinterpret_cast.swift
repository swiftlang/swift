// RUN: %target-swift-emit-sil -sil-verify-all -verify \
// RUN: -enable-experimental-feature BuiltinModule %s

// RUN: %target-swift-emit-sil -sil-verify-all -verify \
// RUN: -enable-experimental-feature BuiltinModule \
// RUN: -enable-sil-opaque-values %s

// REQUIRES: swift_feature_BuiltinModule

// SILGen emits `Builtin.reinterpretCast` of a noncopyable value in memory as a
// load whose value only feeds an unchecked_bitwise_cast, followed by a
// copy_value of the cast's result and a destroy_value of the loaded value. The
// move checker must classify that load as a consume of the memory, the same way
// that it classifies the copy_addr of the address-only form.
//
// The loadable cases hit this shape with and without opaque values. The generic
// cases only do so with opaque values, once address lowering runs after the
// move checker.

import Builtin

struct NC: ~Copyable {
  var obj: AnyObject
  deinit {}
}

struct Pair: ~Copyable {
  var first: NC
  var second: NC
}

final class Holder {
  let nc: NC
  init(_ nc: consuming NC) { self.nc = nc }
}

func drop(_ nc: consuming NC) {}

//////////////////
// MARK: Valid //
//////////////////

func castLoadable(_ x: consuming NC) -> AnyObject {
  Builtin.reinterpretCast(x)
}

func castGeneric<T: ~Copyable, U>(_ x: consuming T, to type: U.Type) -> U {
  Builtin.reinterpretCast(x)
}

func castInoutThenReinitialize(_ x: inout NC) -> AnyObject {
  let r: AnyObject = Builtin.reinterpretCast(x)
  x = NC(obj: r)
  return r
}

func castFieldThenConsumeOtherField(_ p: consuming Pair) -> AnyObject {
  let r: AnyObject = Builtin.reinterpretCast(p.first)
  drop(p.second)
  return r
}

///////////////////////////////
// MARK: Consumed by the cast //
///////////////////////////////

func useAfterCast(_ x: consuming NC) -> AnyObject { // expected-error {{'x' consumed more than once}}
  let r: AnyObject = Builtin.reinterpretCast(x) // expected-note {{consumed here}}
  _ = consume x // expected-note {{consumed again here}}
  return r
}

func genericUseAfterCast<T: ~Copyable, U>(_ x: consuming T, to type: U.Type) -> U { // expected-error {{'x' consumed more than once}}
  let r: U = Builtin.reinterpretCast(x) // expected-note {{consumed here}}
  _ = consume x // expected-note {{consumed again here}}
  return r
}

func castInLoop(_ x: consuming NC) -> AnyObject { // expected-error {{'x' consumed in a loop}}
  var r: AnyObject = x.obj
  for _ in 0..<2 {
    r = Builtin.reinterpretCast(x) // expected-note {{consumed here}}
  }
  return r
}

func castInout(_ x: inout NC) -> AnyObject { // expected-error {{missing reinitialization of inout parameter 'x' after consume}}
  Builtin.reinterpretCast(x) // expected-note {{consumed here}}
}

func castFieldThenConsumeAggregate(_ p: consuming Pair) -> AnyObject { // expected-error {{'p' consumed more than once}}
  let r: AnyObject = Builtin.reinterpretCast(p.first) // expected-note {{consumed here}}
  _ = consume p // expected-note {{consumed again here}}
  return r
}

// A `let` property of a class cannot be consumed.
func castLetProperty(_ h: Holder) -> AnyObject {
  Builtin.reinterpretCast(h.nc) // expected-error {{'h.nc' is borrowed and cannot be consumed}}
  // expected-note @-1 {{consumed here}}
}
