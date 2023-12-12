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
    _modify { var nc = NC(); yield &nc }
  }
}

struct SNC: ~Copyable {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
    _modify { yield &_data }
  }
}

class C {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
    _modify { yield &_data }
  }
}

protocol P {
  @_borrowed
  var data: NC { get set }
}

func borrow(_ nc: borrowing NC) {}
func take(_ nc: consuming NC) {}
func mod(_ nc: inout NC) {}

func test(c: C) {
  borrow(c.data)
  // TODO: Not an inout parameter, should use a "cannot be consumed" error message
  take(c.data) // expected-error{{missing reinitialization of inout parameter 'c.data' after consume}} expected-note{{consumed here}}
  mod(&c.data)
}
func test(s: S) {
  borrow(s.data)
  take(s.data) // expected-error{{'s.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(m_s s: inout S) {
  borrow(s.data)
  take(s.data) // expected-error{{missing reinitialization of inout parameter 's.data' after consume}} expected-note{{consumed here}}
  mod(&s.data)
}
func test(snc: borrowing SNC) {
  borrow(snc.data)
  take(snc.data) // expected-error{{'snc.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(m_snc snc: inout SNC) {
  borrow(snc.data)
  take(snc.data) // expected-error{{missing reinitialization of inout parameter 'snc.data' after consume}} expected-note{{consumed here}}
  mod(&snc.data)
}

func test<T: P>(t: T) {
  borrow(t.data)
  take(t.data) // expected-error{{'unknown.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test<T: P>(m_t t: inout T) {
  borrow(t.data)
  take(t.data) // expected-error{{missing reinitialization of inout parameter 't.data' after consume}} expected-note{{consumed here}}
  mod(&t.data)
}

// FIXME: get the actual variable name instead of 'unknown' in these cases.
func test(p: P) {
  borrow(p.data)
  take(p.data) // expected-error{{'unknown.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}
}
func test(m_p p: inout P) {
  borrow(p.data)
  take(p.data) // expected-error{{missing reinitialization of inout parameter 'unknown' after consume}} expected-note{{consumed here}}
  mod(&p.data)
}


