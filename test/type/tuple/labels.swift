// RUN: %target-swift-frontend -module-name TestModule -typecheck -verify %s

typealias Tuple1 = (a: Int,
                    b _: Int, // expected-error{{tuple element cannot have two labels}}{{22-24=}}
                    _ c: Int, // expected-error{{tuple element cannot have two labels}}{{21-26=}}
                    d e: Int) // expected-error{{tuple element cannot have two labels}}{{22-24=}}
