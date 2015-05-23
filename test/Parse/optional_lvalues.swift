// RUN: %target-parse-verify-swift

struct S {
  var x: Int = 0
  let y: Int = 0 // expected-note 4 {{change 'let' to 'var' to make it mutable}}

  init() {}
}

struct T {
  var mutS: S? = nil
  let immS: S? = nil // expected-note 10 {{change 'let' to 'var' to make it mutable}}

  init() {}
}

var mutT: T?
let immT: T? = nil  // expected-note 4 {{change 'let' to 'var' to make it mutable}}

let mutTPayload = mutT!

mutT! = T()
mutT!.mutS = S()
mutT!.mutS! = S()
mutT!.mutS!.x = 0
mutT!.mutS!.y = 0 // expected-error{{cannot assign to property: 'y' is a 'let' constant}}
mutT!.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
mutT!.immS! = S() // expected-error{{cannot assign through '!': 'immS' is a 'let' constant}}
mutT!.immS!.x = 0 // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
mutT!.immS!.y = 0 // expected-error{{cannot assign to property: 'y' is a 'let' constant}}

immT! = T() // expected-error{{cannot assign through '!': 'immT' is a 'let' constant}}
immT!.mutS = S() // expected-error{{cannot assign to property: 'immT' is a 'let' constant}}
immT!.mutS! = S() // expected-error{{cannot assign through '!': 'immT' is a 'let' constant}}
immT!.mutS!.x = 0 // expected-error{{cannot assign to property: 'immT' is a 'let' constant}}
immT!.mutS!.y = 0 // expected-error{{cannot assign to property: 'y' is a 'let' constant}}
immT!.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
immT!.immS! = S() // expected-error{{cannot assign through '!': 'immS' is a 'let' constant}}
immT!.immS!.x = 0 // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
immT!.immS!.y = 0 // expected-error{{cannot assign to property: 'y' is a 'let' constant}}

var mutIUO: T! = nil
let immIUO: T! = nil // expected-note 2 {{change 'let' to 'var' to make it mutable}}

mutIUO!.mutS = S()
mutIUO!.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
immIUO!.mutS = S() // expected-error{{cannot assign to property: 'immIUO' is a 'let' constant}}
immIUO!.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}

mutIUO.mutS = S()
mutIUO.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
immIUO.mutS = S() // expected-error{{cannot assign to property: 'immIUO' is a 'let' constant}}
immIUO.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}

func foo(x: Int) {}

var nonOptional: S = S()
_ = nonOptional! // expected-error{{operand of postfix '!' should have optional type; type is 'S'}}
_ = nonOptional!.x // expected-error{{operand of postfix '!' should have optional type; type is 'S'}}

class C {}
class D: C {}

let c = C()
let d = (c as! D)! // expected-error{{forced downcast already produces a non-optional value}}
