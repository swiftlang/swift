// RUN: %target-typecheck-verify-swift

func statementMisspelledAtTheBegginingOfTheLine() {
  let test = 5
  gaurd test > 5 else { return } // expected-error {{consecutive statements}} // expected-note {{did you misspell 'guard'}} {{3-8=guard}}
  i test > 5 { return } // expected-error {{consecutive statements}} // expected-note {{did you misspell 'if'}} {{3-4=if}}
  whle test > 5 { return } // expected-error {{consecutive statements}} // expected-note {{did you misspell 'while'}} {{3-7=while}}
}

func statementMisspelledNotAtTheBegginingOfTheLine() {
  let i = 5
  var x = i x=x+1 // expected-error {{consecutive statements}}
}

// SR-8510

func foo() -> Int? { return 0 }
func bar() {
  gaurd let x = foo() else { return }  // expected-error {{consecutive statements}} // expected-note {{did you misspell 'guard'}} {{3-8=guard}}
  print(x)
}
