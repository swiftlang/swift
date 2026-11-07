// RUN: %target-swift-frontend -parse -verify %s

// Array Literal

let values = [1,2,3,]

let values: [Int,] = [] // expected-cparser-note {{to match this opening '['}} expected-cparser-error {{expected ']' in array type}} expected-cparser-error {{expected pattern}} expected-astgen-error {{unexpected code ',' in array type}} 

// Tuple and Tuple Pattern

let _ = (a: 1, b: 2, c: 3,)
let _: (a: Int, b: Int, c: Int,) = (a: 1, b: 2, c: 3,)

// Closures
let _: (String, Int, Float,) -> Void

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

protocol P<T1, T2,> {
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

// String Literal Interpolation

"\(1,)"
"\(1, f:)" // expected-cparser-error {{expected expression in list of expressions}} expected-astgen-error {{expected value in string literal}}

// Availability Spec List

if #unavailable(iOS 15, watchOS 9,) { }

if #available(iOS 15,) { }  // expected-cparser-error {{expected platform name}} expected-astgen-error {{expected version restriction in availability argument}}

// Built-in Attributes

@attached(extension, conformances: OptionSet,)  // expected-cparser-error {{unexpected ',' separator}} {{45-46=}} expected-astgen-error {{expected value in attribute}}
macro OptionSet<RawType>() = #externalMacro(module: "SwiftMacros", type: "OptionSetMacro")

@inline(never,) // expected-cparser-error {{expected declaration}} expected-error {{expected ')' in 'inline' attribute}} 
func foo() { }

@available(iOS 15,) // expected-error {{expected platform name}} expected-cparser-error {{expected declaration}} 
func foo() { }

@backDeployed(before: SwiftStdlib 6.0,) // expected-cparser-error {{unexpected ',' separator}} {{38-39=}} expected-astgen-error {{expected version restriction in version}}
func foo() { }

struct Foo {
    
  var x: Int
  var y: Int
  
  var value: (Int, Int) {
    @storageRestrictions(initializes: x, y,)  // expected-cparser-error {{expected property name in '@storageRestrictions' list}} expected-astgen-error {{expected ')' in 'storageRestrictions' attribute}}
    init(initialValue) {
      self.x = initialValue.0
      self.y = initialValue.1
    }
    get { (x, y) }
  }

}

func f(in: @differentiable(reverse,) (Int) -> Int) { } // expected-warning {{@differentiable' has been renamed to '@differentiable(reverse)' and will be removed in the next release}} expected-cparser-error {{expected ',' separator}} expected-astgen-error {{unexpected code '(Int) -> Int)' in function}} expected-cparser-error {{unnamed parameters must be written with the empty name '_'}} expected-astgen-error {{expected ')' to end parameter clause}} expected-astgen-note {{to match this opening '('}}

@derivative(of: Self.other,) // expected-cparser-error {{unexpected ',' separator}} {{27-28=}} expected-astgen-error {{expected 'wrt:' and arguments in differentiability argument}}
func foo() {}

@transpose(of: S.instanceMethod,) // expected-cparser-error {{unexpected ',' separator}} {{32-33=}} expected-astgen-error {{expected 'wrt:' and arguments in differentiability argument}}
func transposeInstanceMethodWrtSelf(_ other: S, t: S) -> S {
    other + t
}

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

struct S<T>: P1, P2, P3, where T: Equatable { } // expected-error {{expected type}} expected-cparser-error {{expected '{' in struct}} 

// Condition List

if true, { } // expected-cparser-error {{expected '{' after 'if' condition}} expected-astgen-error {{expected body in 'if' statement}} 

guard true, else { } // expected-cparser-error {{expected expression in conditional}} expected-astgen-error {{expected expression in 'guard' statement}} 

while true, { } // expected-cparser-error {{expected '{' after 'while' condition}} expected-astgen-error {{expected code block in 'while' statement}} 

if #available(OSX 51,) { // expected-cparser-error {{expected platform name}} expected-astgen-error {{expected version restriction in availability argument}}
}

@available(OSX 10.7, iOS 7.0, *,) // expected-error {{expected platform name}} expected-cparser-error {{expected declaration}}
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0,) // expected-cparser-error {{unexpected ',' separator}} {{62-63=}} expected-astgen-error {{expected version restriction in version}}
@backDeployed(before: OSX 10.9,) // expected-cparser-error {{unexpected ',' separator}} {{31-32=}} expected-astgen-error {{expected version restriction in version}}
public struct StructWithAvailability {} //
