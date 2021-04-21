// RUN: %target-swift-frontend -emit-sil -primary-file %s -o /dev/null -verify

func ff_fixit_addreturn() -> String {
    print("entering ff_fixit_addreturn()")
    "foo" // expected-warning {{string literal is unused}} expected-error {{missing return in global function expected to return 'String'}} 
    // expected-note@-1 {{did you mean to return the last expression?}}{{5-5=return }}
}

let cl_fixit_addreturn: () -> String = {
    print("entering cl_fixit_addreturn()")
    "foo" // expected-warning {{string literal is unused}} expected-error {{missing return in closure expected to return 'String'}}
    // expected-note@-1 {{did you mean to return the last expression?}}{{5-5=return }}
}

func ff_fixit_addreturn_ifdecl() -> String {
    #if true
    print("entering ff_fixit_addreturn_ifdecl()")
    "foo" // expected-warning {{string literal is unused}} expected-error {{missing return in global function expected to return 'String'}}
    // expected-note@-1 {{did you mean to return the last expression?}}{{5-5=return }}
    #endif
}

let cl_fixit_addreturn_ifdecl: () -> String = {
    #if true
    print("entering cl_fixit_addreturn_ifdecl()")
    "foo" // expected-warning {{string literal is unused}} expected-error {{missing return in closure expected to return 'String'}}
    // expected-note@-1 {{did you mean to return the last expression?}}{{5-5=return }}
    #endif
}
