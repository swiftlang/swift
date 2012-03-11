// RUN: %swift %s -verify

import swift

func &(x : int) {} // expected-error {{cannot declare a custom unary '&' operator}}