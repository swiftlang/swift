// RUN: %target-swift-frontend -parse -verify %s

// No more diags expected during Sema.
// RUN: %target-typecheck-verify-swift

do {
  func something() -> Bool {}
  func foo() {}

  do {
    if something() {
    // expected-error@+1:11 {{expected '{' or 'if' after 'else'}}{{none}}
    } else
  }

  do {
    if something() {
    // expected-error@+1:11 {{expected '{' or 'if' after 'else'}}{{none}}
    } else
    foo()
  }

  do {
    if something() {
    // expected-error@+1:7 {{expected '{' or 'if' after 'else'; did you mean to write 'if'?}}{{11-11= if}}
    } else something() { // Fix-it because all on one line.
    }
  }

  do {
    if something() {
    // expected-error@+1:12 {{expected '{' or 'if' after 'else'}}{{none}}
    } else something()
    {
    }
  }

 do {
    if something() {
    // expected-error@+1:12 {{expected '{' or 'if' after 'else'}}{{none}}
    } else something()
    foo1()
  }
}
