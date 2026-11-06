// RUN: %target-swift-frontend -dump-parse -verify %s > /dev/null

func discard<T>(_ t: T) {}

func a() {
  discard case // expected-cparser-error {{'case' label can only appear inside a 'switch' statement}} expected-astgen-error {{'case' can only appear inside a 'switch' statement or 'enum' declaration}}
  // expected-cparser-error@-1 {{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-1 {{consecutive statements on a line must be separated by newline or ';'}}
}

func b() {
  discard 124 // expected-cparser-error {{consecutive statements on a line must be separated by ';'}}
}

func c() {
  discard where 5 == 0 // expected-cparser-error {{expected expression}}
  // expected-cparser-error@-1 {{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-1 {{unexpected code 'where 5 == 0' in function}}
}

func c() {
  // sticking a let here to ensure the parser is treating this like an expr.
  let _ = discard ++ something
}

func c() {
  let _ = discard self // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}}
}

func c() {
  discard Self
}

func c() {
  discard self
}

func c() {
  _forget self  // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}}
}

func c() {
  discard `self`
}

func c() { // expected-astgen-note {{to match this opening '{'}}
  discard switch  // expected-cparser-error {{expected expression in 'switch' statement}}
  // expected-cparser-error@-1 {{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-1 {{expected expression and '{' in 'switch' statement}}
}

func c() {
  discard SarahMarshall
}

func c() {
  discard x.y.z
}

func c() {
  discard discard self // expected-cparser-error {{consecutive statements on a line must be separated by ';'}} expected-astgen-error {{consecutive statements on a line must be separated by newline or ';'}}
}

class discard {
  func discard() {
    discard discard(self)

    let _ = discard()

    let _ = discard{
      discard   {
        discard itALL
      }
    }
  }

  var x: Int {
    get {
      discard self.x()
    }
  }
} // expected-astgen-error {{expected '}' to end function}}
