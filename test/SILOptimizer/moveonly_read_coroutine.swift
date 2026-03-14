// RUN: %target-swift-frontend -DADDRESS_ONLY -emit-sil -verify %s
// RUN: %target-swift-frontend -DLOADABLE -emit-sil -verify %s
// RUN: %target-swift-frontend -DTRIVIAL -emit-sil -verify %s
// RUN: %target-swift-frontend -DEMPTY -emit-sil -verify %s

class X {}

struct NC: ~Copyable {
  var field: Int = 0
  var consumingGetter: Int {
    consuming get { return 0 }
  }
  var readCoroutine: Int {
    _read { yield field }
  }
  consuming func consumingFunc() -> Int { return 0 }
  borrowing func borrowingFunc() -> Int { return 0 }

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

  subscript(index: Int) -> NC {
    _read { yield NC() }
  }
}

struct SNC: ~Copyable {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
  }

  subscript(index: Int) -> NC {
    _read { yield _data }
  }
}

class C {
  private var _data: NC = NC()

  var data: NC {
    _read { yield _data }
  }

  subscript(index: Int) -> NC {
    _read { yield _data }
  }
}

protocol P {
  @_borrowed
  var data: NC { get }

  @_borrowed
  subscript(index: Int) -> NC { get }
}

func borrow(_ nc: borrowing NC) {}
func take(_ nc: consuming NC) {}

func assume(_ b: Bool) { precondition(b, "uh oh") }

func test(c: C) {
  borrow(c.data)
  take(c.data) // expected-error{{'c.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(c[0])
  assume(c[0].field == 0)
  assume(c[0].borrowingFunc() == 0)
  assume(c[0].readCoroutine == 0)
  // take(c[0]) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
  assume(c[0].consumingGetter == 0) // expected-error {{'c.subscript' is borrowed and cannot be consumed}} expected-note {{consumed here}}
  // assume(c[0].consumingFunc() == 0) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
}
func test(s: S) {
  borrow(s.data)
  take(s.data) // expected-error{{'s.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(s[0])
  assume(s[0].field == 0)
  assume(s[0].borrowingFunc() == 0)
  assume(s[0].readCoroutine == 0)
  // take(s[0]) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
  assume(s[0].consumingGetter == 0) // expected-error {{'s.subscript' is borrowed and cannot be consumed}} expected-note {{consumed here}}
  // assume(s[0].consumingFunc() == 0) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
}
func test(snc: borrowing SNC) {
  borrow(snc.data)
  take(snc.data) // expected-error{{'snc.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(snc[0])
  assume(snc[0].field == 0)
  assume(snc[0].borrowingFunc() == 0)
  assume(snc[0].readCoroutine == 0)
  // take(snc[0]) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
  assume(snc[0].consumingGetter == 0) // expected-error {{'snc.subscript' is borrowed and cannot be consumed}} expected-note {{consumed here}}
  // assume(snc[0].consumingFunc() == 0) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
}

func test<T: P>(t: T) {
  borrow(t.data)
  take(t.data) // expected-error{{'t.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(t[0])
  assume(t[0].field == 0)
  assume(t[0].borrowingFunc() == 0)
  assume(t[0].readCoroutine == 0)
  // take(t[0]) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
  assume(t[0].consumingGetter == 0) // expected-error {{'t.subscript' is borrowed and cannot be consumed}} expected-note {{consumed here}}
  // assume(t[0].consumingFunc() == 0) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
}
func test(p: P) {
  borrow(p.data)
  take(p.data) // expected-error{{'p.data' is borrowed and cannot be consumed}} expected-note{{consumed here}}

  borrow(p[0])
  assume(p[0].field == 0)
  assume(p[0].borrowingFunc() == 0)
  assume(p[0].readCoroutine == 0)
  // take(p[0]) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
  assume(p[0].consumingGetter == 0) // expected-error {{'p.subscript' is borrowed and cannot be consumed}} expected-note {{consumed here}}
  // assume(p[0].consumingFunc() == 0) // FIXME: the move-checker's diagnostics produced for this code are non-deterministic?
}

