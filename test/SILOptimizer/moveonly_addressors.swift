// RUN: %target-swift-frontend  -enable-experimental-feature BuiltinModule -parse-stdlib -module-name Swift -DADDRESS_ONLY -emit-sil -verify %s
// RUN: %target-swift-frontend  -enable-experimental-feature BuiltinModule -parse-stdlib -module-name Swift -DLOADABLE -emit-sil -verify %s
// RUN: %target-swift-frontend  -enable-experimental-feature BuiltinModule -parse-stdlib -module-name Swift -DTRIVIAL -emit-sil -verify %s
// RUN: %target-swift-frontend  -enable-experimental-feature BuiltinModule -parse-stdlib -module-name Swift -DEMPTY -emit-sil -verify %s

// REQUIRES: swift_feature_BuiltinModule

// TODO: Use the real stdlib types once `UnsafePointer` supports noncopyable
// types.

import Builtin

@_marker public protocol Copyable: ~Escapable {}
@_marker public protocol Escapable: ~Copyable {}
@frozen public struct UnsafePointer<T: ~Copyable>: Copyable {
   var value: Builtin.RawPointer
}

@frozen public struct UnsafeMutablePointer<T: ~Copyable>: Copyable {
    var value: Builtin.RawPointer
}

@frozen public struct Int { var value: Builtin.Word }

@_silgen_name("makeUpAPointer")
func makeUpAPointer<T: ~Copyable>() -> UnsafePointer<T>
@_silgen_name("makeUpAMutablePointer")
func makeUpAPointer<T: ~Copyable>() -> UnsafeMutablePointer<T>
@_silgen_name("makeUpAnInt")
func makeUpAnInt() -> Int

class X {}

struct NC: ~Copyable {
#if EMPTY
#elseif TRIVIAL
  var x: Int = makeUpAnInt()
#elseif LOADABLE
  var x: X = X()
#elseif ADDRESS_ONLY
  var x: Any = X()
#else
#error("pick a mode")
#endif
  deinit {}
}

struct S {
  var data: NC {
    unsafeAddress { return makeUpAPointer() }
  }

  var mutableData: NC {
    unsafeAddress { return makeUpAPointer() }
    unsafeMutableAddress { return makeUpAPointer() }
  }
}

struct SNC: ~Copyable {
  var data: NC {
    unsafeAddress { return makeUpAPointer() }
  }

  var mutableData: NC {
    unsafeAddress { return makeUpAPointer() }
    unsafeMutableAddress { return makeUpAPointer() }
  }
}

class C {
  final var data: NC {
    unsafeAddress { return makeUpAPointer() }
  }

  final var mutableData: NC {
    unsafeAddress { return makeUpAPointer() }
    unsafeMutableAddress { return makeUpAPointer() }
  }
}

func borrow(_ nc: borrowing NC) {}
func mod(_ nc: inout NC) {}
func take(_ nc: consuming NC) {}

// TODO: Use more specific diagnostic than "reinitialization of inout parameter"

func test(c: C) {
  borrow(c.data)
  take(c.data) // expected-error{{'c.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(c.mutableData)
  mod(&c.mutableData)
  take(c.mutableData) // expected-error{{missing reinitialization of inout parameter 'c.mutableData' after consume}} expected-note{{consumed here}}
}
func test(s: S) {
  borrow(s.data)
  take(s.data) // expected-error{{'s.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(s.mutableData)
  take(s.mutableData) // expected-error{{'s.mutableData' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(mut_s s: inout S) {
  borrow(s.data)
  take(s.data) // expected-error{{'s.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(s.mutableData)
  mod(&s.mutableData)
  take(s.mutableData) // expected-error{{missing reinitialization of inout parameter 's.mutableData' after consume}} expected-note{{consumed here}}
}
func test(snc: borrowing SNC) {
  borrow(snc.data)
  take(snc.data) // expected-error{{'snc.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(snc.mutableData)
  take(snc.mutableData) // expected-error{{'snc.mutableData' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(mut_snc snc: inout SNC) {
  borrow(snc.data)
  take(snc.data) // expected-error{{'snc.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(snc.mutableData)
  mod(&snc.mutableData)
  take(snc.mutableData) // expected-error{{missing reinitialization of inout parameter 'snc.mutableData' after consume}} expected-note{{consumed here}}
}
