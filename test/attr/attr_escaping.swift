// RUN: %target-parse-verify-swift

@escaping var fn : () -> Int = { 4 }  // expected-error {{@escaping may only be used on 'parameter' declarations}} {{1-11=}}

func wrongParamType(a: @escaping Int) {} // expected-error {{@escaping attribute only applies to function types}}

func conflictingAttrs(_ fn: @noescape @escaping () -> Int) {} // expected-error {{@escaping conflicts with @noescape}}

func takesEscaping(_ fn: @escaping () -> Int) {} // ok
