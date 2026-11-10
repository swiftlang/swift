// RUN: %target-typecheck-verify-swift -enable-experimental-feature TrailingComma

// REQUIRES: swift_feature_TrailingComma

// Condition List

func testConditionListTrailingComma() {
    if true, { }

    if true, { }; { }()

    if true, { print("if-body") } else { print("else-body") }

    if true, { print("if-body") } else if true, { print("else-if-body") } else { print("else-body") }

    if true, { if true { { } } } // expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}}

    { if true, { print(0) } } // expected-error {{closure expression is unused}} expected-note {{did you mean to use a 'do' statement?}}

    ( if true, { print(0) } ) // expected-error {{'if' may only be used as expression in return, throw, or as the source of an assignment}} expected-error {{'if' must have an unconditional 'else' to be used as expression}}

    if true, {true}(), { }

    if true, { true }, { print(0) } // expected-error {{function produces expected type 'Bool'; did you mean to call it with '()'?}}

    if true, { print(0) }
    { }()

    if true, { true } // expected-error {{function produces expected type 'Bool'; did you mean to call it with '()'?}}
    ,{ print(0) }

    if true, { (x: () -> Void) in true } != nil { print(0) } // expected-warning {{comparing non-optional value of type '(() -> Void) -> Bool' to 'nil' always returns true}}

    if true, { (x: () -> Void) in true }
    != nil // expected-warning {{comparing non-optional value of type '(() -> Void) -> Bool' to 'nil' always returns true}}
    {
    print(0)
    }

    if { } // expected-error {{missing condition in 'if' statement}} 

    if , { } // expected-cparser-error {{expected expression, var, or let in 'if' condition}} expected-astgen-error {{missing condition in 'if' statement}}

    guard else { return } // expected-cparser-error {{missing condition in 'guard' statement}} expected-astgen-error {{expected condition in 'guard' statement}} 

    guard , else { return } // expected-cparser-error {{expected expression, var, let or case in 'guard' condition}} expected-astgen-error {{expected expression in 'guard' statement}} 

    guard true, else { return }

    guard true, , else { return } // expected-cparser-error {{expected expression in conditional}} expected-astgen-error {{expected expression in 'guard' statement}}

    guard true, { return } // expected-cparser-error {{expected 'else' after 'guard' condition}} expected-astgen-error {{expected 'else' in 'guard' statement}}

    while true, { }
}

// Switch Case Pattern List

switch 5 {
    case 1, 2,:
        break
    default:
        break
}

protocol P1 { }
protocol P2 { }

// Generic Where Clause List

struct S1<T1, T2,> where T1: P1, T2: P2, { }

protocol P3 {
  func f<T1, T2>(a: T1, b: T2) where T1: P1, T2: P2, // expected-cparser-error {{expected type}} expected-astgen-error {{expected left-hand type, '==', and right-hand type in same type requirement}} 
}

// Inheritance Clause List

struct S2: P1, P2, { }

struct S3<T>: P1, P2, where T: Equatable { }

protocol P4 {
    associatedtype T: P1, P2, // expected-error {{expected type}} 
}