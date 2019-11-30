// RUN: %target-typecheck-verify-swift

func statementMisspelledAtTheBegginingOfTheLine() {
  let test = 5
  gaurd test > 5 else { return } // expected-error {{did you misspell 'guard'}} {{3-8=guard}} expected-error {{use of unresolved identifier 'gaurd'}}
  i test > 5 { return } // expected-error {{did you misspell 'if'}} {{3-4=if}} expected-error {{use of unresolved identifier 'i'}}
  whle test > 5 { return } // expected-error {{did you misspell 'while'}} {{3-7=while}} expected-error {{use of unresolved identifier 'whle'}}
}

func statementMisspelledNotAtTheBegginingOfTheLine() {
  let i = 5
  var x = i x=x+1 // expected-error {{consecutive statements}}
}