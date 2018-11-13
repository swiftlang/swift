// RUN: %target-typecheck-verify-swift

@noreturn func noReturn1(_: Int) {}
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{33-33= -> Never }}

@noreturn func noReturn2(_: Int)
{}
// expected-error@-2 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{33-33= -> Never}}

@noreturn
func noReturn3(_: Int)
{}
// expected-error@-3 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-10=}}{{23-23= -> Never}}

@noreturn func noReturnInt1(_: Int) -> Int {}
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{40-43=Never}}

@noreturn func noReturnInt2(_: Int) -> Int
{}
// expected-error@-2 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{40-43=Never}}

@noreturn func noReturnThrows1(_: Int) throws {}
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{46-46= -> Never }}

@noreturn func noReturnThrows2(_: Int) throws
{}
// expected-error@-2 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{46-46= -> Never}}

@noreturn func noReturnThrowsInt1(_: Int) throws -> Int {}
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{53-56=Never}}

@noreturn func noReturnThrowsInt2(_: Int) throws -> Int
{}
// expected-error@-2 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{1-11=}}{{53-56=Never}}

// Test that error recovery gives us the 'Never' return type
let x: Never = noReturn1(0) // No error

// @noreturn in function type declarations
let valueNoReturn: @noreturn () -> ()
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{20-30=}}{{36-38=Never}}

let valueNoReturnInt: @noreturn () -> Int
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{23-33=}}{{39-42=Never}}

let valueNoReturnInt2: @noreturn
() -> Int
// expected-error@-2 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{24-1=}}{{7-10=Never}}

let valueNoReturn2: @noreturn () -> () = {}
// expected-error@-1 {{'@noreturn' has been removed; functions that never return should have a return type of 'Never' instead}}{{21-31=}}{{37-39=Never}}
