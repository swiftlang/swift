// RUN: %swift-frontend -dump-parse -verify %s > /dev/null

func _forget<T>(_ t: T) {}

func a() {
  _forget case // expected-error {{'case' label can only appear inside a 'switch' statement}}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
}

func b() {
  _forget 124 // expected-error {{consecutive statements on a line must be separated by ';'}}
}

func c() {
  _forget where 5 == 0 // expected-error {{expected expression}}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
}

func c() {
  // sticking a let here to ensure the parser is treating this like an expr.
  let _ = _forget ++ something
}

func c() {
  let _ = _forget self // expected-error {{consecutive statements on a line must be separated by ';'}}
}

func c() {
  _forget Self
}

func c() {
  _forget self
}

func c() {
  _forget `self`
}

func c() {
  _forget switch  // expected-error {{expected expression in 'switch' statement}}
  // expected-error@-1 {{consecutive statements on a line must be separated by ';'}}
}

func c() {
  _forget SarahMarshall
}

func c() {
  _forget x.y.z
}

func c() {
  _forget _forget self // expected-error {{consecutive statements on a line must be separated by ';'}}
}

class _forget {
  func _forget() {
    _forget _forget(self)

    let _ = _forget()

    let _ = _forget{
      _forget   {
        _forget itALL
      }
    }
  }

  var x: Int {
    get {
      _forget self.x()
    }
  }
}
