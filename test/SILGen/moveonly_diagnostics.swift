// RUN: %target-swift-emit-sil -verify -enable-experimental-move-only %s

//////////////////
// Declarations //
//////////////////

@_moveOnly
public class Klass {
    var intField: Int
    var klsField: Klass?
    init() {
        klsField = Klass()
        intField = 5
    }
}

public func nonConsumingUseKlass(_ k: Klass) {}

@_moveOnly
public struct NonTrivialStruct {
    var k = Klass()
}

public func nonConsumingUseNonTrivialStruct(_ s: NonTrivialStruct) {}

@_moveOnly
public enum NonTrivialEnum {
    case first
    case second(Klass)
    case third(NonTrivialStruct)
}

public func nonConsumingUseNonTrivialEnum(_ e : NonTrivialEnum) {}

///////////
// Tests //
///////////

//===---
// Function Arguments
//

public func useKlass(_ k: Klass) { // expected-error {{'k' has guaranteed ownership but was consumed}}
    nonConsumingUseKlass(k)
    let k2 = k // expected-note {{consuming use}}
    nonConsumingUseKlass(k)
    let _ = k2
}

public func useKlassConsume(_ k: __owned Klass) { // expected-error {{'k' consumed more than once}}
    nonConsumingUseKlass(k)
    let k2 = k // expected-note {{consuming use}}
    // NOTE: We should mark the next line as a lifetime extending use.
    nonConsumingUseKlass(k)
    let _ = k2
}

public func useKlassConsume2(_ k: __owned Klass) { // expected-error {{'k' consumed more than once}}
    nonConsumingUseKlass(k)
    let k2 = k // expected-note {{consuming use}}
    nonConsumingUseKlass(k)
    let k3 = k // expected-note {{consuming use}}
    let _ = k2
    let _ = k3
}

public func useNonTrivialStruct(_ s: NonTrivialStruct) { // expected-error {{'s' has guaranteed ownership but was consumed}}
    nonConsumingUseNonTrivialStruct(s)
    let s2 = s // expected-note {{consuming use}}
    let k = s.k
    let _ = k
    nonConsumingUseNonTrivialStruct(s)
    let _ = s2
}

public func useNonTrivialEnum(_ s: NonTrivialEnum) { // expected-error {{'s' has guaranteed ownership but was consumed}}
    nonConsumingUseNonTrivialEnum(s)
    let s2 = s // expected-note {{consuming use}}
    switch s { // expected-note {{consuming use}}
    case _:
        break
    }
    nonConsumingUseNonTrivialEnum(s)
    let _ = s2
}

//===---
// Self in Init
//

//===---
// Self in Methods
//

extension Klass {
    func testNoUseSelf() { // expected-error {{'self' has guaranteed ownership but was consumed}}
        let x = self // expected-note {{consuming use}}
        let _ = x
    }
}

extension NonTrivialStruct {
    func testNoUseSelf() { // expected-error {{'self' has guaranteed ownership but was consumed}}
        let x = self // expected-note {{consuming use}}
        let _ = x
    }
}

extension NonTrivialEnum {
    func testNoUseSelf() { // expected-error {{'self' has guaranteed ownership but was consumed}}
        let x = self // expected-note {{consuming use}}
        let _ = x
    }
}
