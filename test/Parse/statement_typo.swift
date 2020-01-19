// RUN: %target-typecheck-verify-swift

// SR-8510

func foo() -> Int? { return 0 }
func bar() {
  gaurd let x = foo() else { return }  // expected-error {{consecutive statements}} 
  // expected-note@-1 {{did you misspell 'guard'}} {{3-8=guard}}
  // expected-error@-2 {{use of unresolved identifier 'gaurd'}}
  print(x)
}

func stmtMisspelledAtTheBegginingOfTheLineWithLet1() {
  gaurd let firstWithLet = foo() else { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'guard'}} {{3-8=guard}}
  // expected-error@-2 {{use of unresolved identifier 'gaurd'}}
}

func stmtMisspelledAtTheBegginingOfTheLineWithLet2() {
  i let secondWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'if'}} {{3-4=if}}
  // expected-error@-2 {{use of unresolved identifier 'i'}}
}

func stmtMisspelledAtTheBegginingOfTheLineWithLet3() {
  whle let thirdWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'while'}} {{3-7=while}}
  // expected-error@-2 {{use of unresolved identifier 'whle'}}
}

func stmtMisspelledAtTheBegginingOfTheLineWithVar1() {
  gaurd var firstWithLet = foo() else { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'guard'}} {{3-8=guard}}
  // expected-error@-2 {{use of unresolved identifier 'gaurd'}}
  i var secondWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'if'}} {{3-4=if}}
  whle var thirdWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'while'}} {{3-7=while}}
}

func stmtMisspelledAtTheBegginingOfTheLineWithVar2() {
  i var secondWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'if'}} {{3-4=if}}
  // expected-error@-2 {{use of unresolved identifier 'i'}}
}

func stmtMisspelledAtTheBegginingOfTheLineWithVar3() {
  whle var thirdWithLet = foo() { return } // expected-error {{consecutive statements}}
  // expected-note@-1 {{did you misspell 'while'}} {{3-7=while}}
  // expected-error@-2 {{use of unresolved identifier 'whle'}}
}

func stmtMisspelledNotAtTheBegginingOfTheLine() {
  let i = 5
  var x = i x=x+1 // expected-error {{consecutive statements}}
}
