// RUN: %target-parse-verify-swift
// RUN: %target-parse-verify-swift

func unconstrained<T>(x: T) {}

var x = 0
var y = "one"

// Although the generic parameter is unconstrained, it can only be bound to
// a materializable type. This should resolve T = Int and load the @lvalue x.
unconstrained(x)
// Although the generic parameter is unconstrained, it can only be bound to
// This should resolve T = (Int, String) and load the (@lvalue x, @lvalue y)
// tuple.
// FIXME: There's an inconsistency here in call argument matching between
// rvalues and lvalues. <rdar://problem/17786730>
unconstrained((x, y))
unconstrained(x, y) // expected-error{{extra argument in call}}


let a = 0
let b = "one"
unconstrained(a)
unconstrained((a, b))
unconstrained(a, b)
