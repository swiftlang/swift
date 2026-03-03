// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=upcoming-swift

import StdFunction

var ctx: Int32 = 123
let _ = FunctionIntIntToInt({ a, b in a + b + ctx }) // OK

var stringCtx = std.string("abc")
let _ = invokeFunctionTwiceConstRefX2(.init({ _ in stringCtx }), std.string("prefix")) // expected-error {{cannot convert value of type 'std.string'}}

let _ = invokeTemplatedCallableIntToInt(.init({ x in x + 1 })) // expected-error {{cannot infer contextual base in reference to member 'init'}}
let _ = invokeTemplatedCallableIntToInt(FunctionIntToInt({ x in x + 1 })) // OK

let _ = invokeTemplatedCallableByConstRefIntToInt(.init({ x in x + 1 })) // expected-error {{cannot infer contextual base in reference to member 'init'}}
let _ = invokeTemplatedCallableByConstRefIntToInt(FunctionIntToInt({ x in x + 1 })) // OK
