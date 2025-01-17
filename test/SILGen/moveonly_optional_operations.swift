// RUN: %target-swift-frontend -emit-sil -parse-stdlib -module-name Swift -verify %s

@_marker protocol Copyable {}
@_marker protocol Escapable {}

enum Optional<Wrapped: ~Copyable>: ~Copyable {
    case none
    case some(Wrapped)
}

extension Optional: Copyable where Wrapped: Copyable { }

func _diagnoseUnexpectedNilOptional(_filenameStart: Builtin.RawPointer,
                                    _filenameLength: Builtin.Word,
                                    _filenameIsASCII: Builtin.Int1,
                                    _line: Builtin.Word,
                                    _isImplicitUnwrap: Builtin.Int1) {
}

precedencegroup AssignmentPrecedence {}

struct NC: ~Copyable {
    borrowing func b() {}
    mutating func m() {}
    consuming func c() {}

    consuming func c2() -> NC { c2() } // expected-warning{{}}
    consuming func c3() -> NCAO { c3() } // expected-warning{{}}
}

struct NCAO: ~Copyable {
    var x: Any

    borrowing func b() {}
    mutating func m() {}
    consuming func c() {}

    consuming func c2() -> NC { c2() } // expected-warning{{}}
    consuming func c3() -> NCAO { c3() } // expected-warning{{}}
}

func borrowingChains(nc: borrowing NC?, // expected-error{{'nc' is borrowed and cannot be consumed}}
                     ncao: borrowing NCAO?) { // expected-error{{'ncao' is borrowed and cannot be consumed}}
    nc?.b()
    nc?.c() // expected-note{{consumed here}}

    ncao?.b()
    ncao?.c() // expected-note{{consumed here}}
}

func borrowingChains2(nc: borrowing NC?,
                      ncao: borrowing NCAO?) {
    nc?.b()

    ncao?.b()
}

func mutatingChains(nc: inout NC?, // expected-error{{'nc' used after consume}}
                    ncao: inout NCAO?) { // expected-error{{'ncao' used after consume}}
    nc?.b()
    nc?.m()
    nc?.c() // expected-note{{consumed here}}
    nc?.b() // expected-note{{used here}}
    nc?.m()
    nc = .none
    nc?.b()
    nc?.m()

    ncao?.b()
    ncao?.m()
    ncao?.c() // expected-note{{consumed here}}
    ncao?.b() // expected-note{{used here}}
    ncao?.m()
    ncao = .none
    ncao?.b()
    ncao?.m()
}

func mutatingChains2(nc: inout NC?, // expected-error{{'nc' used after consume}}
                    ncao: inout NCAO?) { // expected-error{{'ncao' used after consume}}
    nc?.b()
    nc?.m()
    nc?.c() // expected-note{{consumed here}}
    nc?.m() // expected-note{{used here}}
    nc = .none
    nc?.m()

    ncao?.b()
    ncao?.m()
    ncao?.c() // expected-note{{consumed here}}
    ncao?.m() // expected-note{{used here}}
    ncao = .none
    ncao?.m()
}

func mutatingChains3(nc: inout NC?,
                    ncao: inout NCAO?) {
    nc?.b()
    nc?.m()
    nc?.c()
    nc = .none
    nc?.b()
    nc?.m()

    ncao?.b()
    ncao?.m()
    ncao?.c()
    ncao = .none
    ncao?.b()
    ncao?.m()
}

func mutatingChains4(nc: inout NC?, // expected-error{{missing reinitialization of inout parameter 'nc' after consume}}
                    ncao: inout NCAO?) { // expected-error{{missing reinitialization of inout parameter 'ncao' after consume}}
    nc?.b()
    nc?.m()
    nc?.c() // expected-note{{consumed here}}

    ncao?.b()
    ncao?.m()
    ncao?.c() // expected-note{{consumed here}}
}

func consumingChains(nc: consuming NC?, // expected-error{{'nc' used after consume}}
                     ncao: consuming NCAO?) { // expected-error{{'ncao' used after consume}}
    nc?.b()
    nc?.m()
    nc?.c() // expected-note{{consumed here}}
    nc?.b() // expected-note{{used here}}
    nc?.m()

    ncao?.b()
    ncao?.m()
    ncao?.c() // expected-note{{consumed here}}
    ncao?.b() // expected-note{{used here}}
    ncao?.m()
}

func consumingChains2(nc: consuming NC?,
                      ncao: consuming NCAO?) {
    nc?.b()
    nc?.m()
    nc?.c()

    ncao?.b()
    ncao?.m()
    ncao?.c()
}

func consumingSwitchSubject(nc: consuming NC?,
                            ncao: consuming NCAO?) {
    switch nc?.c2() {
    default:
      break
    }
    switch ncao?.c2() {
    default:
      break
    }
}

func consumingSwitchSubject2(nc: consuming NC?,
                             ncao: consuming NCAO?) {
    switch nc?.c3() {
    default:
      break
    }
    switch ncao?.c3() {
    default:
      break
    }
}
