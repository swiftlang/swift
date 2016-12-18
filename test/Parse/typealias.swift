// RUN: %target-typecheck-verify-swift

//===--- Simple positive tests.

typealias IntPair = (Int, Int)
typealias IntTriple = (Int, Int, Int)
typealias FiveInts = (IntPair, IntTriple)
var fiveInts : FiveInts = ((4,2), (1,2,3))


// <rdar://problem/13339798> QoI: poor diagnostic in malformed typealias
typealias Foo1 : Int  // expected-error {{expected '=' in typealias declaration}} {{16-17==}}
typealias Foo2: Int  // expected-error {{expected '=' in typealias declaration}} {{15-16= =}}
typealias Foo3 :Int  // expected-error {{expected '=' in typealias declaration}} {{16-17== }}
typealias Foo4:/*comment*/Int  // expected-error {{expected '=' in typealias declaration}} {{15-16= = }}

//===--- Tests for error recovery.

typealias Recovery1 // expected-error {{expected '=' in typealias declaration}}

typealias Recovery2 : // expected-error {{expected '=' in typealias declaration}}
// expected-error @-1 {{expected type in typealias declaration}}

typealias Recovery3 = // expected-error {{expected type in typealias declaration}}

typealias Recovery4 : Int // expected-error {{expected '=' in typealias declaration}}

typealias Recovery5 : Int, Float // expected-error {{expected '=' in typealias declaration}}
// expected-error @-1 {{consecutive statements on a line must be separated by ';'}}
// expected-error @-2 {{expected expression}}

typealias Recovery6 = = // expected-error {{expected type in typealias declaration}}

typealias switch = Int // expected-error {{keyword 'switch' cannot be used as an identifier here}} expected-note {{if this name is unavoidable, use backticks to escape it}} {{11-17=`switch`}}
