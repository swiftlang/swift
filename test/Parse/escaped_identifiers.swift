// RUN: %swift -parse -verify %s

func `protocol`() {}

`protocol`()

class `Type` {}

var `class` = `Type`.self

`class = `Type`.self // expected-error{{escaped identifier does not have a closing '`'}}

`class` = `Type // expected-error{{escaped identifier does not have a closing '`'}}
  .type

let `0` = 0 // expected-error{{escaped identifier does not begin with a valid identifier start character}} expected-error{{expected pattern}}
let `foo-bar` = 0 // expected-error{{escaped identifier contains invalid identifier character}} expected-error{{expected pattern}}

func foo() {}

`foo`()

``() // expected-error{{escaped identifier cannot be empty}}
