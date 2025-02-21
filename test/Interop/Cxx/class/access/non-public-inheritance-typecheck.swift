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
        // TODO: prot()
        // TODO: priv()
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
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
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
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPublPublBase(v: PublPublBase) {
        v.publ()
        v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
        v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtPublBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fProtPublBase(v: ProtPublBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivPublBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPrivPublBase(v: PrivPublBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fProtBase(v: ProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}


extension PublProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPublProtBase(v: PublProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fProtProtBase(v: ProtProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivProtBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPrivProtBase(v: PrivProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivBase {
    func ext() {
        // TODO: publ()
        // TODO: prot()
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPrivBase(v: PrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PublPrivBase {
    func ext() {
        publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
        prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPublPrivBase(v: PublPrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension ProtPrivBase {
    func ext() {
        publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
        prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fProtPrivBase(v: ProtPrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}

extension PrivPrivBase {
    func ext() {
        publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
        prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
        priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
    }
}
func fPrivPrivBase(v: PrivPrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{'priv' is inaccessible due to 'private' protection level}}
}
// UNSUPPORTED: OS=windows-msvc
