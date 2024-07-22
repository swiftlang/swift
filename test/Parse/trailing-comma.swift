// RUN: %target-swift-frontend -parse -verify %s -disable-experimental-parser-round-trip

// Tuple and Tuple Pattern

let _ = (a: 1, b: 2, c: 3,)

let (_, _,) = (0,1,)

// Arguments and Parameters

func foo(a: Int = 0, b: Int = 0, c: Int = 0,) -> Int {
    return a + b + c
}

foo(a: 1, b: 2, c: 3,)

// Subscript

foo[1, 2,]

// KeyPath Subscript

\Foo.bar[0,1,]

// Generic Parameters

struct S<T1, T2,> { }

func foo<T1, T2,>() { }

protocol P<T1, T2> {
    associatedtype T1
    associatedtype T2
}

// Closure Capture List

let _ = { [obj1, obj2,] in }

// Attributes

@Foo(a, b, c,) struct S { }

f(_: @foo(1, 2,) Int)

// Macro Expansions

#foo(1, 2,)

struct S {
    #foo(1, 2,)
}

// Macro Role Attribute

@attached(extension, conformances: OptionSet,)
macro OptionSet<RawType>() = #externalMacro(module: "SwiftMacros", type: "OptionSetMacro")

// String Literal Interpolation

"\(1,)"

// Availability Spec List

if #unavailable(iOS 15, watchOS 9,) { }

// The following cases are only supported with the 'TrailingComma' experimental feature flag enabled
 
// Switch Case Pattern List

switch number {
    case 1, 2,: // expected-error {{expected pattern}} 
        break
    default:
        break
}

// Generic Where Clause List

struct S<T1, T2, T3,> where T1: P1, T2: P2, { } // expected-error {{expected type}} 

// Inheritance Clause List

struct S: P1, P2, P3, { } // expected-error {{expected type}} 

struct S<T>: P1, P2, P3, where T: Equatable { } // expected-error {{expected type}} expected-error {{expected '{' in struct}} 

// Condition List

if true, { } // expected-error {{expected '{' after 'if' condition}} 

guard true, else { } // expected-error {{expected expression in conditional}} 

while true, { } // expected-error {{expected '{' after 'while' condition}} 