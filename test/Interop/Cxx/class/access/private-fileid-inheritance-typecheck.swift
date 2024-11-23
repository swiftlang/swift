// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -I %S/Inputs -cxx-interoperability-mode=default -module-name main %t/blessed.swift
//--- blessed.swift

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
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPublBase(v: PublBase) {
    v.publ()
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'PublBase' has no member 'priv'}}
}

extension PublPublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPublPublBase(v: PublPublBase) {
        v.publ()
        v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
        v.priv() // expected-error {{value of type 'PublPublBase' has no member 'priv'}}
}

extension ProtPublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fProtPublBase(v: ProtPublBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'ProtPublBase' has no member 'priv'}}
}

extension PrivPublBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPrivPublBase(v: PrivPublBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'PrivPublBase' has no member 'priv'}}
}

extension ProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{value of type 'ProtBase' has no member 'priv'}}
    }
}
func fProtBase(v: ProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'ProtBase' has no member 'priv'}}
}


extension PublProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPublProtBase(v: PublProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'PublProtBase' has no member 'priv'}}
}

extension ProtProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fProtProtBase(v: ProtProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
}

extension PrivProtBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPrivProtBase(v: PrivProtBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'PrivProtBase' has no member 'priv'}}
}

extension PrivBase {
    func ext() {
        publ()
        prot()
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPrivBase(v: PrivBase) {
    v.publ() // expected-error {{'publ' is inaccessible due to 'private' protection level}}
    v.prot() // expected-error {{'prot' is inaccessible due to 'private' protection level}}
    v.priv() // expected-error {{value of type 'PrivBase' has no member 'priv'}}
}

extension PublPrivBase {
    func ext() {
        publ() // expected-error {{cannot find 'publ' in scope}}
        prot() // expected-error {{cannot find 'prot' in scope}}
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPublPrivBase(v: PublPrivBase) {
    v.publ() // expected-error {{value of type 'PublPrivBase' has no member 'publ'}}
    v.prot() // expected-error {{value of type 'PublPrivBase' has no member 'prot'}}
    v.priv() // expected-error {{value of type 'PublPrivBase' has no member 'priv'}}
}

extension ProtPrivBase {
    func ext() {
        publ() // expected-error {{cannot find 'publ' in scope}}
        prot() // expected-error {{cannot find 'prot' in scope}}
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fProtPrivBase(v: ProtPrivBase) {
    v.publ() // expected-error {{value of type 'ProtPrivBase' has no member 'publ'}}
    v.prot() // expected-error {{value of type 'ProtPrivBase' has no member 'prot'}}
    v.priv() // expected-error {{value of type 'ProtPrivBase' has no member 'priv'}}
}

extension PrivPrivBase {
    func ext() {
        publ() // expected-error {{cannot find 'publ' in scope}}
        prot() // expected-error {{cannot find 'prot' in scope}}
        priv() // expected-error {{cannot find 'priv' in scope}}
    }
}
func fPrivPrivBase(v: PrivPrivBase) {
    v.publ() // expected-error {{value of type 'PrivPrivBase' has no member 'publ'}}
    v.prot() // expected-error {{value of type 'PrivPrivBase' has no member 'prot'}}
    v.priv() // expected-error {{value of type 'PrivPrivBase' has no member 'priv'}}
}
