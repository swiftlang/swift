// RUN: %target-typecheck-verify-swift

let f1: (Int) -> Int = { $0 }
let f2: @convention(swift) (Int) -> Int = { $0 }
let f3: @convention(block) (Int) -> Int = { $0 }
let f4: @convention(c) (Int) -> Int = { $0 }

let f5: @convention(INTERCAL) (Int) -> Int = { $0 } // expected-error{{convention 'INTERCAL' not supported}}

// SR-11027

func sr11027(_ f: @convention(block) @autoclosure () -> Int) -> Void {} // expected-error {{'@convention(block)' attribute is not allowed on '@autoclosure' types}} 
sr11027(1)

func sr11027_c(_ f: @convention(c) @autoclosure () -> Int) -> Void {} // expected-error{{'@convention(c)' attribute is not allowed on '@autoclosure' types}}
sr11027_c(1)

func sr11027_swift(_ f: @convention(swift) @autoclosure () -> Int) -> Void {} // OK
sr11027_swift(1)

func sr11027_thin(_ f: @convention(thin) @autoclosure () -> Int) -> Void {} // OK
sr11027_thin(1)

func sr11027_2(_ f: @autoclosure @convention(block) () -> Int) -> Void {} // expected-error {{'@convention(block)' attribute is not allowed on '@autoclosure' types}} 
sr11027_2(1)

func sr11027_c_2(_ f: @autoclosure @convention(c) () -> Int) -> Void {} // expected-error {{'@convention(c)' attribute is not allowed on '@autoclosure' types}} 
sr11027_c_2(1)

func sr11027_swift_2(_ f: @autoclosure @convention(swift) () -> Int) -> Void {} // OK
sr11027_swift_2(1)

func sr11027_thin_2(_ f: @autoclosure @convention(thin) () -> Int) -> Void {} // OK
sr11027_thin_2(1)
