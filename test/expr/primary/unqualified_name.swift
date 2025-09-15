// RUN: %target-typecheck-verify-swift -swift-version 4

func f0(_ x: Int, y: Int, z: Int) { }
func f1(_ x: Int, while: Int) { }
func f2(_ x: Int, `let` _: Int) { }
func f3(_ x: Int, `var` _: Int) { }
func f4(_ x: Int, _ y: Int, z: Int) { }

func test01() {
  _ = f0(_:y:z:)
  _ = f0(:y:z:) // expected-error{{an empty argument label is spelled with '_'}}{{10-10=_}}
  _ = f1(_:`while`:) // expected-warning{{keyword 'while' does not need to be escaped in argument list}}{{12-13=}}{{18-19=}}
  _ = f2(_:`let`:) // expected-warning {{keyword 'let' does not need to be escaped in argument list}} {{12-13=}} {{16-17=}}
  _ = f3(_:`var`:) // expected-warning {{keyword 'var' does not need to be escaped in argument list}} {{12-13=}} {{16-17=}}
  _ = f4(_::z:) // expected-error{{an empty argument label is spelled with '_'}}{{12-12=_}}
}

struct S0 {
  func f0(_ x: Int, y: Int, z: Int) { }
  func f1(_ x: Int, while: Int) { }
  func f2(_ x: Int, `let` _: Int) { }
  func f3(_ x: Int, `var` _: Int) { }

  func testS0() {
    _ = f0(_:y:z:)
    _ = f0(:y:z:) // expected-error{{an empty argument label is spelled with '_'}}{{12-12=_}}
    _ = f1(_:`while`:) // expected-warning{{keyword 'while' does not need to be escaped in argument list}}{{14-15=}}{{20-21=}}
    _ = f2(_:`let`:) // expected-warning {{keyword 'let' does not need to be escaped in argument list}} {{14-15=}} {{18-19=}}
    _ = f3(_:`var`:) // expected-warning {{keyword 'var' does not need to be escaped in argument list}} {{14-15=}} {{18-19=}}
    
    _ = self.f0(_:y:z:)
    _ = self.f0(:y:z:) // expected-error{{an empty argument label is spelled with '_'}}{{17-17=_}}
    _ = self.f1(_:`while`:) // expected-warning{{keyword 'while' does not need to be escaped in argument list}}{{19-20=}}{{25-26=}}
    _ = self.f2(_:`let`:) // expected-warning {{keyword 'let' does not need to be escaped in argument list}}{{19-20=}}{{23-24=}}
    _ = self.f3(_:`var`:) // expected-warning {{keyword 'var' does not need to be escaped in argument list}}{{19-20=}}{{23-24=}}

    _ = f3(_:y:z:) // expected-error{{static member 'f3(_:y:z:)' cannot be used on instance of type 'S0'}}{{9-9=S0.}}
  }

  static func testStaticS0() {
    _ = f0(_:y:z:)
    _ = f3(_:y:z:)
  }

  static func f3(_ x: Int, y: Int, z: Int) -> S0 { return S0() }
}

// Determine context from type.
let s0_static: S0 = .f3(_:y:z:)(0, 0, 0)

class C0 {
  init(x: Int, y: Int, z: Int) { }

  convenience init(all: Int) {
    self.init(x:y:z:)(all, all, all)
  }

  func f0(_ x: Int, y: Int, z: Int) { }
  func f1(_ x: Int, while: Int) { }
  func f2(_ x: Int, `let` _: Int) { }
  func f3(_ x: Int, `var` _: Int) { }
}

class C1 : C0 {
  init(all: Int) {
    super.init(x:y:z:)(all, all, all)
  }

  func testC0() {
    _ = f0(_:y:z:)
    _ = f0(:y:z:) // expected-error{{an empty argument label is spelled with '_'}}{{12-12=_}}
    _ = f1(_:`while`:) // expected-warning{{keyword 'while' does not need to be escaped in argument list}}{{14-15=}}{{20-21=}}
    _ = f2(_:`let`:) // expected-warning {{keyword 'let' does not need to be escaped in argument list}} {{14-15=}} {{18-19=}}
    _ = f3(_:`var`:) // expected-warning {{keyword 'var' does not need to be escaped in argument list}} {{14-15=}} {{18-19=}}
    
    _ = super.f0(_:y:z:)
    _ = super.f0(:y:z:) // expected-error{{an empty argument label is spelled with '_'}}{{18-18=_}}
    _ = super.f1(_:`while`:) // expected-warning{{keyword 'while' does not need to be escaped in argument list}}{{20-21=}}{{26-27=}}
    _ = self.f2(_:`let`:) // expected-warning {{keyword 'let' does not need to be escaped in argument list}} {{19-20=}} {{23-24=}}
    _ = self.f3(_:`var`:) // expected-warning {{keyword 'var' does not need to be escaped in argument list}} {{19-20=}} {{23-24=}}
    
  }
}

struct S1 {
  init(x: Int) {} // expected-note {{'init(x:)' declared here}}

  func testS1() {
    _ = S1.init(x:)(1)
    _ = S1.init(`x`: 1)  // expected-warning {{keyword 'x' does not need to be escaped in argument list}} {{17-18=}} {{19-20=}}

    // Test for unknown token.
    _ = S1.init(x: 0xG) // expected-error {{'G' is not a valid hexadecimal digit (0-9, A-F) in integer literal}} expected-error {{missing argument for parameter 'x' in call}}
  }
}
