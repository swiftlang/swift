// RUN: %target-swift-frontend \
// RUN:   -enable-experimental-feature LifetimeDependence  \
// RUN:   -typecheck -verify %s

// REQUIRES: swift_feature_LifetimeDependence

struct C_C1<T: ~Copyable>: ~Copyable {}
extension C_C1: Copyable where T: Copyable {}

struct C_C2<T: ~Copyable>: ~Copyable {}
// expected-error @+2 {{conditional conformance to 'Copyable' must explicitly state whether 'T' is required to conform to 'Copyable'}}
// expected-error @+1 {{marked with '~Copyable'}}
extension C_C2: Copyable {}

struct C_CE1<T: ~Copyable & ~Escapable>: ~Copyable {}
extension C_CE1: Copyable where T: Copyable, T: ~Escapable {}

struct C_CE2<T: ~Copyable & ~Escapable>: ~Copyable {}
// expected-error @+1 {{conditional conformance to 'Copyable' must explicitly state whether 'T' is required to conform to 'Escapable'}}
extension C_CE2: Copyable where T: Copyable {}

struct C_CE3<T: ~Copyable & ~Escapable>: ~Copyable {}
// expected-error @+2 {{conditional conformance to 'Copyable' must explicitly state whether 'T' is required to conform to 'Copyable'}}
// expected-error @+1 {{marked with '~Copyable'}}
extension C_CE3: Copyable where T: ~Escapable {}

struct CE_C<T: ~Copyable>: ~Copyable, ~Escapable {}
extension CE_C: Copyable where T: Copyable {}

struct CE_E<T: ~Escapable>: ~Copyable, ~Escapable {}
extension CE_E: Escapable where T: Escapable {}

struct CE_CE1<T: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {}
extension CE_CE1: Copyable where T: Copyable, T: ~Escapable {}
extension CE_CE1: Escapable where T: ~Copyable, T: Escapable {}

struct CE_CE2<T: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {}
// expected-error @+1 {{conditional conformance to 'Copyable' must explicitly state whether 'T' is required to conform to 'Escapable'}}
extension CE_CE2: Copyable where T: Copyable {}
// expected-error @+1 {{conditional conformance to 'Escapable' must explicitly state whether 'T' is required to conform to 'Copyable'}}
extension CE_CE2: Escapable where T: Escapable {}

struct CE_CE3<T: ~Copyable & ~Escapable>: ~Copyable, ~Escapable {}
// expected-error @+2 {{conditional conformance to 'Copyable' must explicitly state whether 'T' is required to conform to 'Copyable'}}
// expected-error @+1 {{marked with '~Copyable'}}
extension CE_CE3: Copyable where T: ~Escapable {}
// expected-error @+2 {{conditional conformance to 'Escapable' must explicitly state whether 'T' is required to conform to 'Escapable'}}
// expected-error @+1 {{marked with '~Escapable'}}
extension CE_CE3: Escapable where T: ~Copyable {}

struct C_C_X<T: ~Copyable, U>: ~Copyable {}
extension C_C_X: Copyable where T: Copyable {}

protocol Floppyable {} // implied Copyable requirement

struct C_C_P<T: ~Copyable, U: Floppyable>: ~Copyable {}
extension C_C_P: Copyable where T: Copyable {}

class Sloppyable {} // implied Copyable requirement since it's a class

struct C_C_K<T: ~Copyable, U: Sloppyable>: ~Copyable {}
extension C_C_K: Copyable where T: Copyable {}

protocol Ploppyable: ~Copyable {}

struct C_C_PC1<T: ~Copyable, U: Ploppyable & ~Copyable>: ~Copyable {}
// expected-error @+1 {{conditional conformance to 'Copyable' must explicitly state whether 'U' is required to conform to 'Copyable'}}
extension C_C_PC1: Copyable where T: Copyable {}

struct C_C_PC2<T: ~Copyable, U: Ploppyable & ~Copyable>: ~Copyable {}
extension C_C_PC2: Copyable where T: Copyable, U: Copyable {}

struct C_C_X2<T: ~Copyable, U: Ploppyable /*& Copyable*/>: ~Copyable {}
extension C_C_X2: Copyable where T: Copyable {}

struct C_C_L<T: ~Copyable, U: AnyObject>: ~Copyable {}
extension C_C_L: Copyable where T: Copyable {}

struct C_C_C<T: ~Copyable, U: ~Copyable> {}
extension C_C_C where T: ~Copyable, U == Int {
    struct SubInt: ~Copyable {}
}

extension C_C_C.SubInt: Copyable where T: Copyable {}

struct NC: ~Copyable {}

extension C_C_C where T: ~Copyable, U: ~Copyable, U == NC {
    struct SubNC: ~Copyable {}
}

extension C_C_C.SubNC: Copyable where T: Copyable {}
