// RUN: %swift %s -verify

import swift

struct X { }
struct Y { }

func +(lhs : X, rhs : X) -> X { } // okay

func +++(lhs : X, rhs : X) -> X { } // expected-error{{binary operators must be declared 'infix_left'}}

func [infix_left=200] +(lhs : Y, rhs : Y) -> Y { } // FIXME: should error

func [infix_left=195] ++++(lhs : X, rhs : X) -> X { }
func ++++(lhs : Y, rhs : Y) -> Y { } // okay


