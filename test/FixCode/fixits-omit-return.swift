// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify

func ff_fixit_addreturn() -> String {
    print("entering ff_fixit_addreturn()")
    "foo" // expected-warning {{string literal is unused}} expected-error {{missing return in a function expected to return 'String'; did you mean to return the last expression?}} {{5-5=return }}
}

let cl_fixit_addreturn: () -> String = {
    print("entering cl_fixit_addreturn()")
    "foo" // expected-warning {{string literal is unused}} expected-error {{missing return in a closure expected to return 'String'; did you mean to return the last expression?}} {{5-5=return }}
}


