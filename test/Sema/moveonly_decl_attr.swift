// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

// expected-error@+1 {{'@_moveOnly' attribute is deprecated and will be removed; use '~Copyable' instead}}
@_moveOnly class C { // expected-error {{'@_moveOnly' attribute is only valid on structs or enums}}{{1-12=}}
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

// expected-error@+1 {{'@_moveOnly' attribute is deprecated and will be removed; use '~Copyable' instead}}
@_moveOnly struct S {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

// expected-error@+1 {{'@_moveOnly' attribute is deprecated and will be removed; use '~Copyable' instead}}
@_moveOnly enum E {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

@_moveOnly let l = C()  // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}

// expected-error@+1 {{'@_moveOnly' attribute is deprecated and will be removed; use '~Copyable' instead}}
@_moveOnly protocol P {} // expected-error {{'@_moveOnly' attribute is only valid on structs or enums}}{{1-12=}}
// expected-error@+1 {{'@_moveOnly' attribute is deprecated and will be removed; use '~Copyable' instead}}
@_moveOnly actor A {} // expected-error {{'@_moveOnly' attribute is only valid on structs or enums}}{{1-12=}}
@_moveOnly extension C {} // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}{{1-12=}}
