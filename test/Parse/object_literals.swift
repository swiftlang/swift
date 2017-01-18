// RUN: %target-typecheck-verify-swift

let _ = [##] // expected-error{{expected identifier after '#' in object literal expression}} expected-error{{object literal syntax no longer uses '[# ... #]'}} {{9-10=}} {{11-13=}}
let _ = [#what#] // expected-error{{object literal syntax no longer uses '[# ... #]'}} {{9-10=}} {{15-17=}}
let _ = [#what()#] // expected-error{{object literal syntax no longer uses '[# ... #]'}} {{9-10=}} {{17-19=}}
let _ = [#colorLiteral( // expected-error@+1 {{expected expression in list of expressions}}
