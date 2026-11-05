// RUN: %target-swift-frontend -parse -verify %s

// No more diags expected during Sema.
// RUN: %target-typecheck-verify-swift

do {
  // The statements ASTGen keeps after a bad 'else' are type-checked, so keep
  // them free of semantic diagnostics.
  @discardableResult func something() -> Bool {}
  func foo() {}

  do {
    if something() {
    // expected-error@+1:11 {{expected '{' or 'if' after 'else'}}{{none}} expected-astgen-error@+1:11 {{expected '}' to end 'if' statement}}
    } else
  }

  do {
    if something() {
    // expected-error@+1:11 {{expected '{' or 'if' after 'else'}}{{none}} expected-astgen-error@+2:10 {{expected '}' to end 'if' statement}}
    } else
    foo()
  }

  do {
    if something() {
    // expected-cparser-error@+1:7 {{expected '{' or 'if' after 'else'; did you mean to write 'if'?}}{{11-11= if}} expected-astgen-error@+1:12 {{unexpected code 'something()' in 'if' statement}}
    } else something() { // Fix-it because all on one line.
    }
  }

  do {
    if something() {
    // expected-cparser-error@+1:12 {{expected '{' or 'if' after 'else'}}{{none}} expected-astgen-error@+1:12 {{unexpected code 'something()' in 'if' statement}}
    } else something()
    {
    }
  }

 do {
    if something() {
    // expected-error@+1:12 {{expected '{' or 'if' after 'else'}}{{none}} expected-astgen-error@+2:10 {{expected '}' to end 'if' statement}}
    } else something()
    foo()
  }
}
