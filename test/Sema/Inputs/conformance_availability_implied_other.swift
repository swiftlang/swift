
@available(macOS 200, *)
extension Conformer1: Derived2 {}

@available(macOS 100, *)
extension Conformer2: Derived1 {}
// expected-error@-1 {{conformance of 'Conformer2' to 'Base' is only available in macOS 200 or newer}}

