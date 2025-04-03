//--- blessed.swift
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift
import NonPublicInheritance

// Extensions of each class test whether we correctly modeled *which* members
// get inherited, but do not tell us whether they were inherited with the right
// access permissions.
//
// Global functions (i.e., not in extensions) tell us whether those members were
// inherited with the correct Swift access level (i.e., public vs private).

extension Base {
    func ext() {
        publ()
        prot()
        priv()
    }
}
func fBase(v: Base) {
    v.publ()
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPublBase(v: PublBase) {
    v.publ()
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PublPublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPublPublBase(v: PublPublBase) {
        v.publ()
        v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
        v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtPublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fProtPublBase(v: ProtPublBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivPublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPrivPublBase(v: PrivPublBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fProtBase(v: ProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}


extension PublProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPublProtBase(v: PublProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fProtProtBase(v: ProtProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPrivProtBase(v: PrivProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPrivBase(v: PrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PublPrivBase {
    func ext() {
        publ() // expected-error {{'publ()' is unavailable: this base member is not accessible because of private inheritance}}
        prot() // expected-error {{'prot()' is unavailable: this base member is not accessible because of private inheritance}}
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPublPrivBase(v: PublPrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtPrivBase {
    func ext() {
        publ() // expected-error {{'publ()' is unavailable: this base member is not accessible because of private inheritance}}
        prot() // expected-error {{'prot()' is unavailable: this base member is not accessible because of private inheritance}}
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fProtPrivBase(v: ProtPrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivPrivBase {
    func ext() {
        publ() // expected-error {{'publ()' is unavailable: this base member is not accessible because of private inheritance}}
        prot() // expected-error {{'prot()' is unavailable: this base member is not accessible because of private inheritance}}
        priv() // expected-error {{'priv()' is unavailable: this base member is not accessible because it is private}}
    }
}
func fPrivPrivBase(v: PrivPrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}
