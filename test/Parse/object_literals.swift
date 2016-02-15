// RUN: %target-parse-verify-swift

let _ = [##] // expected-error{{expected identifier after '[#' in object literal expression}} expected-error{{consecutive statements on a line must be separated by ';'}} {{11-11=;}} expected-error{{expected expression}}
let _ = [#what#] // expected-error{{expected argument list in object literal}} expected-error{{consecutive statements on a line must be separated by ';'}} {{15-15=;}} expected-error{{expected expression}}
let _ = [#what()#] // expected-error{{use of unknown object literal name 'what'}}
let _ = [#Color( // expected-error{{expected expression in container literal}}
