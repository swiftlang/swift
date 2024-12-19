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

// FIXME: Check additional unavailability conditions
