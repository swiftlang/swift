// RUN: %target-swift-frontend -enable-experimental-move-only -verify -emit-sil %s

class Klass {}

var globalMoveOnlyStruct = MoveOnlyStruct()
var globalMoveOnlyEnum = MoveOnlyEnum.lhs(Klass())

@_moveOnly
struct MoveOnlyStruct {
    var k = Klass()

    deinit { // expected-error {{'self' consumed more than once}}
        let x = self // expected-note {{consuming use}}
        _ = x
        var y = MoveOnlyStruct() // expected-error {{'y' consumed more than once}}
        y = self // expected-note {{consuming use}}
        // We get an infinite recursion since we are going to call our own
        // deinit here. We are just testing diagnostics here though.
        _ = y // expected-warning {{function call causes an infinite recursion}}
        // expected-note @-1 {{consuming use}}
        let z = y // expected-note {{consuming use}}
        let _ = z
        globalMoveOnlyStruct = self // expected-note {{consuming use}}
    } // expected-note {{consuming use}}
}

@_moveOnly
enum MoveOnlyEnum {
    case lhs(Klass)
    case rhs(Klass)

    deinit { // expected-error {{'self' consumed more than once}}
        let x = self // expected-note {{consuming use}}
        _ = x
        var y = MoveOnlyEnum.lhs(Klass())
        y = self // expected-note {{consuming use}}
        // We get an infinite recursion since we are going to call our own
        // deinit here. We are just testing diagnostics here though.
        _ = y // expected-warning {{function call causes an infinite recursion}}
        globalMoveOnlyEnum = self // expected-note {{consuming use}}
    } // expected-note {{consuming use}}
}
