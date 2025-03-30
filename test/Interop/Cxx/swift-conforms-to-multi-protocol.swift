// RUN: %target-typecheck-verify-swift -enable-experimental-cxx-interop

import Foundation

// C++ class declared in a bridging header with multiple conformances:
// `class MyCXXClass {} SWIFT_CONFORMS_TO_PROTOCOL(P1); SWIFT_CONFORMS_TO_PROTOCOL(P2);`

protocol P1 { func foo() }
protocol P2 { func bar() }

extension MyCXXClass: P1 {} // expected-no-error
extension MyCXXClass: P2 {} // expected-no-error

let x = MyCXXClass()
x.foo() // Verify P1 conformance
x.bar() // Verify P2 conformance
