// RUN: %target-swift-frontend -DADDRESS_ONLY -emit-sil -verify %s
// RUN: %target-swift-frontend -DLOADABLE -emit-sil -verify %s
// RUN: %target-swift-frontend -DTRIVIAL -emit-sil -verify %s
// RUN: %target-swift-frontend -DEMPTY -emit-sil -verify %s

class X {}

struct NC: ~Copyable {
#if EMPTY
#elseif TRIVIAL
  var x: Int = 0
#elseif LOADABLE
  var x: X = X()
#elseif ADDRESS_ONLY
  var x: Any = X()
#else
#error("pick a mode")
#endif
  deinit { print("destroy") }
}

struct S {
  var data: NC {
    _read { yield NC() }
  }
}

struct SNC: ~Copyable {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
  }
}

class C {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
  }
}

protocol P {
  @_borrowed
  var data: NC { get }
}

func borrow(_ nc: borrowing NC) {}
func take(_ nc: consuming NC) {}

func test(c: C) {
  borrow(c.data)
  take(c.data) // expected-error{{'c.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(s: S) {
  borrow(s.data)
  take(s.data) // expected-error{{'s.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(snc: borrowing SNC) {
  borrow(snc.data)
  take(snc.data) // expected-error{{'snc.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}

func test<T: P>(t: T) {
  borrow(t.data)
  take(t.data) // expected-error{{'t.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(p: P) {
  borrow(p.data)
  take(p.data) // expected-error{{'p.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}

