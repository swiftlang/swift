// RUN: %target-typecheck-verify-swift

// SR-8510

func foo() -> Int? { return 0 }
func bar() {
  gaurd let x = foo() else { return }  // expected-error {{consecutive statements}} 
  // expected-note@-1 {{did you misspell 'guard'}} {{3-8=guard}}
  // expected-error@-2 {{use of unresolved identifier 'gaurd'}}
  print(x)
}

func stmtMisspelledAtTheBegginingOfTheLineWithLet() {
  gaurd let firstWithLet = foo() else { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'guard'}} {{3-8=guard}}
  // expected-error@-2 {{use of unresolved identifier 'gaurd'}}
  i let secondWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'if'}} {{3-4=if}}
  whle let thirdWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'while'}} {{3-7=while}}
}

func stmtMisspelledAtTheBegginingOfTheLineWithVar() {
  gaurd var firstWithLet = foo() else { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'guard'}} {{3-8=guard}}
  // expected-error@-2 {{use of unresolved identifier 'gaurd'}}
  i var secondWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'if'}} {{3-4=if}}
  whle var thirdWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'while'}} {{3-7=while}}
}

func stmtMisspelledNotAtTheBegginingOfTheLine() {
  let i = 5
  var x = i x=x+1 // expected-error {{consecutive statements}}
}
