
@available(macOS 200, *)
extension Conformer1: Derived2 {}

@available(macOS 100, *)
extension Conformer2: Derived1 {}
// expected-note@-1 {{update '@available' attribute on enclosing}}
// expected-error@-2 {{conformance of 'Conformer2' to 'Base' is only available in macOS 200 or newer}}

