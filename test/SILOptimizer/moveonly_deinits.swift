// RUN: %target-swift-frontend -sil-verify-all -verify -emit-sil %s

class Klass {}

var globalMoveOnlyStruct = MoveOnlyStruct()
var globalMoveOnlyEnum = MoveOnlyEnum.lhs(Klass())

@_moveOnly
struct MoveOnlyStruct {
    var k = Klass()

    deinit {
        // expected-error @-1 {{'self' consumed more than once}}
        // expected-error @-2 {{'self' consumed more than once}}
        // expected-error @-3 {{'self' consumed more than once}}
        let x = self // expected-note {{consuming use here}}
        _ = x
        var y = MoveOnlyStruct() // expected-error {{'y' consumed more than once}}
        y = self
        // expected-note @-1 {{consuming use here}}
        // expected-note @-2 {{consuming use here}}
        // We get an infinite recursion since we are going to call our own
        // deinit here. We are just testing diagnostics here though.
        // expected-warning @-6 {{function call causes an infinite recursion}}
        _ = y
        // expected-note @-1 {{consuming use here}}
        let z = y // expected-note {{consuming use here}}
        let _ = z
        globalMoveOnlyStruct = self // expected-note {{consuming use here}}
        // expected-note @-1 {{consuming use here}}
    } // expected-note {{consuming use here}}
}

@_moveOnly
enum MoveOnlyEnum {
    case lhs(Klass)
    case rhs(Klass)

    deinit {
        // expected-error @-1 {{'self' consumed more than once}}
        // expected-error @-2 {{'self' consumed more than once}}
        // expected-error @-3 {{'self' consumed more than once}}
        let x = self // expected-note {{consuming use here}}
        _ = x
        var y = MoveOnlyEnum.lhs(Klass())
        y = self // expected-note {{consuming use here}}
        // expected-note @-1 {{consuming use here}}
        // We get an infinite recursion since we are going to call our own
        // deinit here. We are just testing diagnostics here though.
        // expected-warning @-5 {{function call causes an infinite recursion}}
        _ = y 
        globalMoveOnlyEnum = self // expected-note {{consuming use here}}
        // expected-note @-1 {{consuming use here}}
    } // expected-note {{consuming use here}}
}
