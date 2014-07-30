// RUN: %swift -parse -verify %s

func foo() {
  func bar() {
    bar() // expected-error{{cannot reference a local function from another local function}}
  }

  func bas() {
    bar() // expected-error{{cannot reference a local function from another local function}}
  }

  func zim() {
    func zang() {
      zim()  // expected-error{{cannot reference a local function from another local function}}
    }
    zang() // OK
  }

  func flippity() {
    floppity() // expected-error{{cannot capture 'floppity' before it is declared}}
  }
  func floppity() { // expected-note{{declared here}}
    flippity() // expected-error{{cannot reference a local function from another local function}}
  }
}
