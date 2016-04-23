// RUN: %target-parse-verify-swift

let _ = [##] // expected-error{{expected identifier after '#' in object literal expression}} expected-error{{object literal syntax no longer uses '[# ... #]'}}
let _ = [#what#] // expected-error{{object literal syntax no longer uses '[# ... #]'}}
let _ = [#what()#] // expected-error{{object literal syntax no longer uses '[# ... #]'}}
let _ = [#colorLiteral( // expected-error{{expected ',' separator}} expected-error{{expected expression in list of expressions}}