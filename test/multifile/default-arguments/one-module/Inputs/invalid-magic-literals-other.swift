
func badMagicLiteral(_ x: String = #line) {} // expected-error {{default argument value of type 'Int' cannot be converted to type 'String'}}

func badGenericMagicLiteral<T : ExpressibleByIntegerLiteral>(_ x: T = #function) -> T { x } // expected-error {{default argument value of type 'String' cannot be converted to type 'T'}}
