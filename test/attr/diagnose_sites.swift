// REQUIRES: swift_feature_SourceWarningControl
// RUN: %target-typecheck-verify-swift -enable-experimental-feature SourceWarningControl

@available(*, deprecated)
func dep() -> Bool { return false }

@available(*, deprecated)
class depClass {}

// Function
@diagnose(DeprecatedDeclaration, as: error)
func foo() {
    let _: Bool = dep() // expected-error {{'dep()' is deprecated}}
    @diagnose(DeprecatedDeclaration, as: warning)
    func bar() {
        let _: Bool = dep() // expected-warning {{'dep()' is deprecated}}
        @diagnose(DeprecatedDeclaration, as: ignored)
        @diagnose(PerformanceHints, as: error)
        func baz() {
            let _: Bool = dep()
        }
        @diagnose(DeprecatedDeclaration, as: error)
        func qux() {
            let _: Bool = dep() // expected-error {{'dep()' is deprecated}}
            @diagnose(DeprecatedDeclaration, as: warning)
            func corge() {
                let _: Bool = dep() // expected-warning {{'dep()' is deprecated}}
            }
        }
    }
}

// Nominal types
@diagnose(DeprecatedDeclaration, as: error)
class Foo {
    let x = dep() // expected-error {{'dep()' is deprecated}}
}
@diagnose(DeprecatedDeclaration, as: ignored)
struct Bar {
    let x = dep()
}
@diagnose(DeprecatedDeclaration, as: warning)
enum Baz {
    var x: Bool { dep() } // expected-warning {{'dep()' is deprecated}}
}
@diagnose(DeprecatedDeclaration, as: error)      
actor Qux {
    let x = dep() // expected-error {{'dep()' is deprecated}}
    @diagnose(DeprecatedDeclaration, as: ignored)
    struct Quux {
        let x = dep()
    }
}
@diagnose(DeprecatedDeclaration, as: error)
protocol Proto {
    var x: depClass { get } // expected-error {{'depClass' is deprecated}}
}

// Initializer
@diagnose(DeprecatedDeclaration, as: error)
struct Corge : ~Copyable {
    let x = dep() // expected-error {{'dep()' is deprecated}}
    @diagnose(DeprecatedDeclaration, as: ignored)
    init() {
        let _ = dep() 
    }
    @diagnose(DeprecatedDeclaration, as: warning) 
    deinit {
        let _ = dep() // expected-warning {{'dep()' is deprecated}}
    }
}

// Extension
@diagnose(DeprecatedDeclaration, as: error)
extension Foo {
    var y: Bool { dep() } // expected-error {{'dep()' is deprecated}}
}

// Import
@diagnose(PreconcurrencyImport, as: error)
@preconcurrency import Swift // expected-error {{'@preconcurrency' on module 'Swift' has no effect}}
@diagnose(PreconcurrencyImport, as: ignored)
@preconcurrency import Swift

// Subscript
struct Hmm {
    @diagnose(DeprecatedDeclaration, as: error)
    subscript(index: Int) -> Bool {
        return dep() // expected-error {{'dep()' is deprecated}}
    }
} 

// Computed property
@diagnose(DeprecatedDeclaration, as: ignored)
extension Foo {
    @diagnose(DeprecatedDeclaration, as: error)
    var property: Bool {
        return dep() // expected-error {{'dep()' is deprecated}}
    }
}

// Accessors
@diagnose(DeprecatedDeclaration, as: ignored)
extension Foo {
    var b: Bool { dep() }
    var computed_property: Bool {
        @diagnose(DeprecatedDeclaration, as: error)
        get {
            return dep() // expected-error {{'dep()' is deprecated}}
        }
        @diagnose(DeprecatedDeclaration, as: warning)
        set {
            let _ = dep() // expected-warning {{'dep()' is deprecated}}
        }
    }
}

// Observers
@diagnose(DeprecatedDeclaration, as: ignored)
struct Foox {
    var observed_property: Bool {
        @diagnose(DeprecatedDeclaration, as: error)
        willSet {
            let _ = dep() // expected-error {{'dep()' is deprecated}}
        }
        @diagnose(DeprecatedDeclaration, as: warning)
        didSet {
            let _ = dep() // expected-warning {{'dep()' is deprecated}}
        }
    }
}

// Enum case
enum UnEnum {
  @diagnose(DeprecatedDeclaration, as: error)
  case foo(depClass) // expected-error {{'depClass' is deprecated}}
}

// Enum raw value case
enum RawEnum: Int32 {
  @diagnose(DeprecatedDeclaration, as: error)
  case foo = 32
}

// Typealias
@diagnose(DeprecatedDeclaration, as: error)
typealias Sneaky = depClass // expected-error {{'depClass' is deprecated}}

// Associated type
protocol P {
  @diagnose(DeprecatedDeclaration, as: error)
  associatedtype AT: depClass // expected-error {{'depClass' is deprecated}}
}

// Macro declaration
@diagnose(DeprecatedDeclaration, as: error)
@freestanding(declaration, names: named(FooMacro))
macro FooMacro(_ s: depClass) = #externalMacro(module: "Foo", type: "Bar") // expected-error {{'depClass' is deprecated}}
// expected-warning@-1 {{external macro implementation type 'Foo.Bar' could not be found for macro 'FooMacro'; plugin for module 'Foo' not found}}

// With property wrapper
@propertyWrapper
@available(*, deprecated)
struct depIntClamped {
    private var value: Int
    var wrappedValue: Int {
        get { value }
        set { value = min(max(newValue, 0), 42) }
    }
    init(wrappedValue: Int) {
        self.value = min(max(wrappedValue, 0), 42)
    }
}

func baz() {
  @depIntClamped // expected-error {{'depIntClamped' is deprecated}}
  @diagnose(DeprecatedDeclaration, as: error)
  var score1: Int = 5

  @diagnose(DeprecatedDeclaration, as: error)
  @depIntClamped // expected-error {{'depIntClamped' is deprecated}}
  var score2: Int = 5

  @diagnose(DeprecatedDeclaration, as: error)
  @depIntClamped // nothing
  @diagnose(DeprecatedDeclaration, as: ignored)
  var score3: Int = 5
}
