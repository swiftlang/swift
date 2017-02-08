// RUN: %target-typecheck-verify-swift

struct S {
  var x: Int = 0
  let y: Int = 0  // expected-note 3 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}} {{3-6=var}}

  mutating func mutateS() {}

  init() {}
}

struct T {
  var mutS: S? = nil
  let immS: S? = nil  // expected-note 4 {{change 'let' to 'var' to make it mutable}} {{3-6=var}} {{3-6=var}} {{3-6=var}} {{3-6=var}}

  mutating func mutateT() {}

  init() {}
}

var mutT: T?
let immT: T? = nil  // expected-note {{change 'let' to 'var' to make it mutable}} {{1-4=var}}

mutT?.mutateT()
immT?.mutateT() // expected-error{{cannot use mutating member on immutable value: 'immT' is a 'let' constant}}
mutT?.mutS?.mutateS()
mutT?.immS?.mutateS() // expected-error{{cannot use mutating member on immutable value: 'immS' is a 'let' constant}}
mutT?.mutS?.x += 1
mutT?.mutS?.y++ // expected-error{{cannot pass immutable value to mutating operator: 'y' is a 'let' constant}}

// Prefix operators don't chain
++mutT?.mutS?.x // expected-error{{cannot pass immutable value of type 'Int?' to mutating operator}}
++mutT?.mutS?.y // expected-error{{cannot pass immutable value of type 'Int?' to mutating operator}}

mutT? = T()
mutT?.mutS = S()
mutT?.mutS? = S()
mutT?.mutS?.x += 0
_ = mutT?.mutS?.x + 0 // expected-error{{value of optional type 'Int?' not unwrapped}} {{5-5=(}} {{18-18=)!}}
mutT?.mutS?.y -= 0 // expected-error{{left side of mutating operator isn't mutable: 'y' is a 'let' constant}}
mutT?.immS = S() // expected-error{{cannot assign to property: 'immS' is a 'let' constant}}
mutT?.immS? = S() // expected-error{{cannot assign to value: 'immS' is a 'let' constant}}
mutT?.immS?.x += 0 // expected-error{{left side of mutating operator isn't mutable: 'immS' is a 'let' constant}}
mutT?.immS?.y -= 0 // expected-error{{left side of mutating operator isn't mutable: 'y' is a 'let' constant}}
