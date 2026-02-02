// REQUIRES: swift_feature_Lifetimes
// REQUIRES: swift_feature_LifetimeDependence

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-ide-test -enable-experimental-feature Lifetimes -enable-experimental-feature LifetimeDependence -print-swift-file-interface -source-filename %t/test.swift > %t/interface.txt
// RUN: diff %t/interface.txt %t/interface.txt.expected

//--- test.swift
public struct S : ~Escapable {
    @_lifetime(immortal)
    init() {}
}

@_lifetime(foo: copy foo)
public func fooFunc(_ foo: inout S) {}

@_lifetime(&bar)
public func barFunc(_ bar: inout S) -> S {
    return bar
}

@_lifetime(`func`: copy `func`)
public func funcFunc(func: inout S) {}

public struct T : ~Escapable {
    let s: S
    @_lifetime(borrow self)
    func selfFunc() -> S { return s }
    @_lifetime(borrow `self`)
    func selfFunc2(`self`: S) -> S { return `self` }
}

//--- interface.txt.expected

public struct S : ~Escapable {

    @_lifetime(immortal)
    internal init()
}
@_lifetime(foo: copy foo)
public func fooFunc(_ foo: inout S)
@_lifetime(&bar)
public func barFunc(_ bar: inout S) -> S
@_lifetime(`func`: copy `func`)
public func funcFunc(func: inout S)

public struct T : ~Escapable {

    internal let s: S

    @_lifetime(borrow self)
    internal func selfFunc() -> S

    @_lifetime(borrow `self`)
    internal func selfFunc2(self: S) -> S
}
