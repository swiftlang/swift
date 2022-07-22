// RUN: %target-typecheck-verify-swift -parse-stdlib -disable-availability-checking -verify-syntax-tree

import Swift

@_moveOnly
class C {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

@_moveOnly
struct S {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

@_moveOnly
enum E {
    @_moveOnly // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
    func foo() {}
}

@_moveOnly let l = C()  // expected-error {{'@_moveOnly' attribute cannot be applied to this declaration}}
