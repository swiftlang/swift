// RUN: %target-typecheck-verify-swift

let f1: (Int) -> Int = { $0 }
let f2: @convention(swift) (Int) -> Int = { $0 }
let f3: @convention(block) (Int) -> Int = { $0 }
let f4: @convention(c) (Int) -> Int = { $0 }

let f5: @convention(INTERCAL) (Int) -> Int = { $0 } // expected-error{{convention 'INTERCAL' not supported}}

// SR-11027

func sr11027(_ f: @convention(block) @autoclosure () -> Int) -> Void {} // expected-error {{@convention(block) attribute is not allowed on @autoclosure type}} 
sr11027(1)

func sr11027_c(_ f: @convention(c) @autoclosure () -> Int) -> Void {} // expected-error{{@convention(c) attribute is not allowed on @autoclosure type}}
sr11027_c(1)