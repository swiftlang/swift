// RUN: %target-swift-frontend -target %target-cpu-apple-macosx10.9 -typecheck -verify %s
// REQUIRES: OS=macosx

struct Butt {
    var setter_conditionally_available: Int {
        get { fatalError() }

        @available(macOS 10.10, *)
        set { fatalError() }
    }
}

func assertExactType<T>(of _: inout T, is _: T.Type) {}

@available(macOS 10.9, *)
public func unavailableSetterContext() {
    var kp = \Butt.setter_conditionally_available
    assertExactType(of: &kp, is: KeyPath<Butt, Int>.self)
}
@available(macOS 10.10, *)
public func availableSetterContext() {
    var kp = \Butt.setter_conditionally_available
    assertExactType(of: &kp, is: WritableKeyPath<Butt, Int>.self)
}
@available(macOS 10.9, *)
public func conditionalAvailableSetterContext() {
    if #available(macOS 10.10, *) {
        var kp = \Butt.setter_conditionally_available
        assertExactType(of: &kp, is: WritableKeyPath<Butt, Int>.self)
    } else {
        var kp = \Butt.setter_conditionally_available
        assertExactType(of: &kp, is: KeyPath<Butt, Int>.self)
    }
}

public func payloadCaseNotAvailable() { // expected-note {{add @available attribute to enclosing global function}}
    let _ = \Int?.some // expected-error {{key paths to enum cases with associated values are only available in macOS 99.99.0 or newer}}
                       // expected-note@-1 {{add 'if #available' version check}}

    guard #available(macOS 99.99.0, *) else {
        return
    }

    let _ = \Double?.some // ok
}

@available(macOS 99.99.0, *)
public func payloadCaseAvailable() {
    let _ = \Bool?.some // ok
}
