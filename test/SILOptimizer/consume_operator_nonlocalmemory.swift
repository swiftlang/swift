// RUN: %target-swift-frontend -verify %s -emit-sil -o /dev/null

// This file tests that we emit good errors for global lets/vars,

////////////////////////
// MARK: Declarations //
////////////////////////

class Klass {
  var s1 = "123"
  let s2 = "123"
}
struct LoadableStruct { var k = Klass() }

struct AddressOnlyStruct<T> { var t: T? = nil }

////////////////////////
// MARK: Global Tests //
////////////////////////

let globalLoadableLet = LoadableStruct()
let _ = consume globalLoadableLet // expected-error {{'consume' cannot be applied to globals}}

let globalAddressOnlyLet = AddressOnlyStruct<Any>()
let _ = consume globalAddressOnlyLet // expected-error {{'consume' cannot be applied to globals}}

var globalLoadableVar = LoadableStruct()
let _ = consume globalLoadableVar // expected-error {{'consume' cannot be applied to globals}}

var globalAddressOnlyVar = AddressOnlyStruct<Any>()
let _ = consume globalAddressOnlyVar // expected-error {{'consume' cannot be applied to globals}}

////////////////////////////
// MARK: Mutable Captures //
////////////////////////////

func accessMutableCapture() -> (() -> ()) {
  func useKlassInOut(_ x: inout Klass) {}
  func useAddressOnlyStructInOut<T>(_ x: inout AddressOnlyStruct<T>) {}

  var x = Klass()
  var x2 = AddressOnlyStruct<Any>()
  var f: () -> () = {}
  f = {
    useKlassInOut(&x)
    useAddressOnlyStructInOut(&x2)
  }
  let _ = consume x // expected-error {{'consume' cannot be applied to escaping captures}}
  let _ = consume x2 // expected-error {{'consume' cannot be applied to escaping captures}}
  return f
}
