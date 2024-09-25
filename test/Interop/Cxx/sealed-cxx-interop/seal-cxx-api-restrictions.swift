// RUN: %target-swiftxx-frontend -typecheck -seal-cxx-interop-requirement -I %S/Inputs %s -verify

import CxxHeader

public func testCustomRetStruct() -> SimpleStruct { // expected-error {{cannot use struct 'SimpleStruct' here}}
    return SimpleStruct(x: 0, y: 0)
}

public func testCustomString(_ x: SimpleStruct) -> Int { // expected-error {{cannot use struct 'SimpleStruct' here}}
    return 0
}

@_alwaysEmitIntoClient
public func testCustomStringAEIC() -> Int {
    let c = SimpleStruct(x: 0, y: 1) // expected-error {{}} expected-error {{}}
    return Int(c.x) // expected-error {{}}
}

@inlinable
public func testCustomStringInlinable() -> Int {
    let c = SimpleStruct(x: 0, y: 1) // expected-error {{}} expected-error {{}}
    return Int(c.y) // expected-error {{}}
}
