// RUN: %target-typecheck-verify-swift -parse-stdlib -disable-availability-checking

import Swift

@_moveOnly class C { // expected-error {{only structs or enums can be noncopyable}}{{1-12=}}
                     // expected-warning@-1 {{'@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6}}
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

@_moveOnly // expected-warning {{'@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6}}
struct S {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}

    deinit {}
}

@_moveOnly // expected-warning {{'@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6}}
enum E {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

@_moveOnly let l = C()  // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}

@_moveOnly protocol P {} // expected-error {{only structs or enums can be noncopyable}}{{1-12=}}
// expected-warning@-1 {{'@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6}}

@_moveOnly actor A {} // expected-error {{only structs or enums can be noncopyable}}{{1-12=}}
// expected-warning@-1 {{'@_moveOnly' attribute is deprecated; use the ~Copyable constraint suppression instead; this is an error in Swift 6}}

@_moveOnly extension C {} // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}{{1-12=}}
