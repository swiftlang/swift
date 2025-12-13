// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@available(*, deprecated)
func dep() -> Bool { return false }

@available(*, deprecated)
struct depS {}

// Function
@warn(DeprecatedDeclaration, as: error)
func foo() {
    let _: Bool = dep() // expected-error {{'dep()' is deprecated}}
    @warn(DeprecatedDeclaration, as: warning)
    func bar() {
        let _: Bool = dep() // expected-warning {{'dep()' is deprecated}}
        @warn(DeprecatedDeclaration, as: ignored)
        @warn(PerformanceHints, as: error)
        func baz() {
            let _: Bool = dep()
        }
        @warn(DeprecatedDeclaration, as: error)
        func qux() {
            let _: Bool = dep() // expected-error {{'dep()' is deprecated}}
            @warn(DeprecatedDeclaration, as: warning)
            func corge() {
                let _: Bool = dep() // expected-warning {{'dep()' is deprecated}}
            }
        }
    }
}

// Nominal types
@warn(DeprecatedDeclaration, as: error)
class Foo {
    let x = dep() // expected-error {{'dep()' is deprecated}}
}
@warn(DeprecatedDeclaration, as: ignored)
struct Bar {
    let x = dep()
}
@warn(DeprecatedDeclaration, as: warning)
enum Baz {
    var x: Bool { dep() } // expected-warning {{'dep()' is deprecated}}
}
@warn(DeprecatedDeclaration, as: error)      
actor Qux {
    let x = dep() // expected-error {{'dep()' is deprecated}}
    @warn(DeprecatedDeclaration, as: ignored)
    struct Quux {
        let x = dep()
    }
}
@warn(DeprecatedDeclaration, as: error)
protocol Proto {
    var x: depS { get } // expected-error {{'depS' is deprecated}}
}

// Initializer
@warn(DeprecatedDeclaration, as: error)
struct Corge : ~Copyable {
    let x = dep() // expected-error {{'dep()' is deprecated}}
    @warn(DeprecatedDeclaration, as: ignored)
    init() {
        let _ = dep() 
    }
    @warn(DeprecatedDeclaration, as: warning) 
    deinit {
        let _ = dep() // expected-warning {{'dep()' is deprecated}}
    }
}

// Extension
@warn(DeprecatedDeclaration, as: error)
extension Foo {
    var y: Bool { dep() } // expected-error {{'dep()' is deprecated}}
}

// Import
@warn(PreconcurrencyImport, as: error)
@preconcurrency import Swift // expected-error {{'@preconcurrency' on module 'Swift' has no effect}}
@warn(PreconcurrencyImport, as: ignored)
@preconcurrency import Swift

// Subscript
struct Hmm {
    @warn(DeprecatedDeclaration, as: error)
    subscript(index: Int) -> Bool {
        return dep() // expected-error {{'dep()' is deprecated}}
    }
} 

// Computed property
@warn(DeprecatedDeclaration, as: ignored)
extension Foo {
    @warn(DeprecatedDeclaration, as: error)
    var property: Bool {
        return dep() // expected-error {{'dep()' is deprecated}}
    }
}

// Accessors
@warn(DeprecatedDeclaration, as: ignored)
extension Foo {
    var b: Bool { dep() }
    var computed_property: Bool {
        @warn(DeprecatedDeclaration, as: error)
        get {
            return dep() // expected-error {{'dep()' is deprecated}}
        }
        @warn(DeprecatedDeclaration, as: warning)
        set {
            let _ = dep() // expected-warning {{'dep()' is deprecated}}
        }
    }
}
