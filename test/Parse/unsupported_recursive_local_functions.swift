// RUN: %target-parse-verify-swift

func foo() {
  var i = 0

  func bar() {
    ++i
    bar() // expected-error{{local functions cannot reference themselves}}
  }

  func bas() {
    ++i
    bar() // expected-error{{cannot reference a local function with captures from another local function}}
  }

  func zim() {
    ++i
    func zang() {
      ++i
      zim()  // expected-error{{cannot reference a local function with captures from another local function}}
    }
    zang() // OK
  }

  func flippity() {
    ++i
    floppity() // expected-error{{cannot capture 'floppity' before it is declared}}
  }
  func floppity() { // expected-note{{declared here}}
    ++i
    flippity() // expected-error{{cannot reference a local function with captures from another local function}}
  }

  func nocaptures() { }
  func capturesTheNoCaptures() {
    nocaptures() // okay
  }
  func capturesTheNoCapturesNoCaptures() {
    capturesTheNoCaptures() // okay
  }
}
