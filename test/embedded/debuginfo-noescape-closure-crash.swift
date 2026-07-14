// RUN: %target-swift-frontend -g -emit-ir %s -enable-experimental-feature Embedded -enable-experimental-feature Lifetimes -wmo

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_Lifetimes

// Verify IRGenDebugInfo emits debug info for a stored closure of type
// (inout T) -> U where T is ~Escapable, without crashing the round-trip
// type-reconstruction self-check in getMangledName.

public struct B: ~Copyable, ~Escapable { @_lifetime(immortal) init() {} }
public final class C {}

public final class K {
    var fn: ((inout B) -> C)?
    init() { self.fn = self.method }
    func method(b: inout B) -> C { C() }
}

_ = K()
