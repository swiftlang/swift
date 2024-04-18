// RUN: %target-swiftxx-frontend -typecheck -cxx-stdlib-path%S/Inputs/c++ -I %S/Inputs -dump-clang-diagnostics %s -verify

import CxxHeader
import custom_std

public func testCustomRetStruct() -> SimpleStruct { // expected-error {{cannot use struct 'SimpleStruct' here}}
    return SimpleStruct(x: 0, y: 0)
}

public func testCustomString(_ x: std.string) -> Int { // expected-error {{cannot use type alias 'string' here}}
    return 0
}

@_alwaysEmitIntoClient
public func testCustomStringAEIC() -> Int {
    let c = std.string("hello") // expected-error {{}} expected-error {{}} expected-error {{}}
    return Int(c.size()) // expected-error {{}}
}

@inlinable
public func testCustomStringInlinable() -> Int {
    let c = std.string("hello") // expected-error {{}} expected-error {{}} expected-error {{}}
    return Int(c.size()) // expected-error {{}}
}
