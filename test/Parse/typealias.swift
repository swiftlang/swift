// RUN: %target-typecheck-verify-swift

//===--- Simple positive tests.

typealias IntPair = (Int, Int)
typealias IntTriple = (Int, Int, Int)
typealias FiveInts = (IntPair, IntTriple)
var fiveInts : FiveInts = ((4,2), (1,2,3))


// <rdar://problem/13339798> QoI: poor diagnostic in malformed typealias
typealias Foo1 : Int  // expected-cparser-error {{expected '=' in type alias declaration}} {{16-17==}} expected-astgen-error {{expected '=' in typealias declaration}}
typealias Foo2: Int  // expected-cparser-error {{expected '=' in type alias declaration}} {{15-16= =}} expected-astgen-error {{expected '=' in typealias declaration}}
typealias Foo3 :Int  // expected-cparser-error {{expected '=' in type alias declaration}} {{16-17== }} expected-astgen-error {{expected '=' in typealias declaration}}
typealias Foo4:/*comment*/Int  // expected-cparser-error {{expected '=' in type alias declaration}} {{15-16= = }} expected-astgen-error {{expected '=' in typealias declaration}}

//===--- Tests for error recovery.

typealias Recovery1 // expected-cparser-error {{expected '=' in type alias declaration}} expected-astgen-error {{expected '=' and type in typealias declaration}}

typealias Recovery2 : // expected-cparser-error {{expected '=' in type alias declaration}} expected-astgen-error {{expected type in typealias declaration}}
// expected-cparser-error @-1 {{expected type in type alias declaration}} expected-astgen-error@-1 {{expected '=' in typealias declaration}}

typealias Recovery3 = // expected-cparser-error {{expected type in type alias declaration}} expected-astgen-error {{expected type in typealias declaration}}

typealias Recovery4 : Int // expected-cparser-error {{expected '=' in type alias declaration}} expected-astgen-error {{expected '=' in typealias declaration}}

typealias Recovery5 : Int, Float // expected-cparser-error {{expected '=' in type alias declaration}}
// expected-cparser-error @-1 {{consecutive statements on a line must be separated by ';'}} expected-astgen-error@-1 {{unexpected code ', Float' in source file}}
// expected-cparser-error @-2 {{expected expression}} expected-astgen-error@-2 {{expected '=' in typealias declaration}}

typealias Recovery6 = = // expected-cparser-error {{expected type in type alias declaration}} expected-astgen-error {{expected type in typealias declaration}} expected-astgen-error {{unexpected code '=' in source file}}

typealias switch = Int // expected-error {{keyword 'switch' cannot be used as an identifier here}} expected-cparser-note {{if this name is unavoidable, use backticks to escape it}} {{11-17=`switch`}}
