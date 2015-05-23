// RUN: %target-parse-verify-swift

struct S {
  var x: Int = 0
  let y: Int = 0

  mutating func mutateS() {}

  init() {}
}

struct T {
  var mutS: S? = nil
  let immS: S? = nil  // expected-note {{change 'let' to 'var' to make it mutable}}

  mutating func mutateT() {}

  init() {}
}

var mutT: T?
let immT: T? = nil

mutT?.mutateT()
immT?.mutateT() // expected-error{{only has mutating members}}
mutT?.mutS?.mutateS()
mutT?.immS?.mutateS() // expected-error{{only has mutating members}}
mutT?.mutS?.x++
mutT?.mutS?.y++ // expected-error{{could not find an overload for '++' that accepts the supplied arguments}}

// Prefix operators don't chain
++mutT?.mutS?.x // expected-error{{cannot pass immutable value of type 'Int?' to mutating operator '++'}}
++mutT?.mutS?.y // expected-error{{cannot pass immutable value of type 'Int?' to mutating operator '++'}}

// TODO: assignment operators
mutT? = T()
mutT?.mutS = S()
mutT?.mutS? = S()
mutT?.mutS?.x += 0
_ = mutT?.mutS?.x + 0 // expected-error{{value of optional type 'Int?' not unwrapped}}
mutT?.mutS?.y -= 0 // expected-error{{could not find an overload for '-=' that accepts the supplied arguments}}
mutT?.immS = S() // expected-error{{cannot assign to 'let' property 'immS'}}
mutT?.immS? = S() // expected-error{{cannot assign to expression with non-lvalue type 'S'}}
mutT?.immS?.x += 0 // expected-error{{could not find an overload for '+=' that accepts the supplied arguments}}
mutT?.immS?.y -= 0 // expected-error{{could not find an overload for '-=' that accepts the supplied arguments}}
