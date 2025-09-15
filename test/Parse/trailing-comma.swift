// RUN: %target-swift-frontend -parse -verify %s

// Array Literal

let values = [1,2,3,]

let values: [Int,] = [] // expected-note {{to match this opening '['}} expected-error {{expected ']' in array type}} expected-error {{expected pattern}} 

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
"\(1, f:)" // expected-error {{expected expression in list of expressions}}

// Availability Spec List

if #unavailable(iOS 15, watchOS 9,) { }

if #available(iOS 15,) { }  // expected-error {{expected platform name}}

// Built-in Attributes

@attached(extension, conformances: OptionSet,)  // expected-error {{unexpected ',' separator}}
macro OptionSet<RawType>() = #externalMacro(module: "SwiftMacros", type: "OptionSetMacro")

@inline(never,) // expected-error {{expected declaration}} expected-error {{expected ')' in 'inline' attribute}} 
func foo() { }

@available(iOS 15,) // expected-error {{expected platform name}} expected-error {{expected declaration}} 
func foo() { }

@backDeployed(before: SwiftStdlib 6.0,) // expected-error {{unexpected ',' separator}}
func foo() { }

struct Foo {
    
  var x: Int
  var y: Int
  
  var value: (Int, Int) {
    @storageRestrictions(initializes: x, y,)  // expected-error {{expected property name in '@storageRestrictions' list}}
    init(initialValue) {
      self.x = initialValue.0
      self.y = initialValue.1
    }
    get { (x, y) }
  }

}

func f(in: @differentiable(reverse,) (Int) -> Int) { } // expected-warning {{@differentiable' has been renamed to '@differentiable(reverse)' and will be removed in the next release}} expected-error {{expected ',' separator}} expected-error {{unnamed parameters must be written with the empty name '_'}}

@derivative(of: Self.other,) // expected-error {{unexpected ',' separator}}
func foo() {}

@transpose(of: S.instanceMethod,) // expected-error {{unexpected ',' separator}}
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

struct S<T>: P1, P2, P3, where T: Equatable { } // expected-error {{expected type}} expected-error {{expected '{' in struct}} 

// Condition List

if true, { } // expected-error {{expected '{' after 'if' condition}} 

guard true, else { } // expected-error {{expected expression in conditional}} 

while true, { } // expected-error {{expected '{' after 'while' condition}} 

if #available(OSX 51,) { // expected-error {{expected platform name}}
}

@available(OSX 10.7, iOS 7.0, *,) // expected-error {{expected platform name}} expected-error {{expected declaration}}
@_originallyDefinedIn(module: "HighLevel", OSX 10.9, iOS 13.0,) // expected-error {{unexpected ',' separator}}
@backDeployed(before: OSX 10.9,) // expected-error {{unexpected ',' separator}}
public struct StructWithAvailability {}
