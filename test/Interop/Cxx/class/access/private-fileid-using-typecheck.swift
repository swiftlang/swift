// Data members redeclared with a using-declaration in a SWIFT_PRIVATE_FILEID
// class should take on the access of the using-declaration, like methods do.
//
// RUN: split-file %s %t
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -module-name main %t%{fs-sep}blessed.swift
// RUN: %target-swift-frontend -typecheck -verify -suppress-notes -I %S%{fs-sep}Inputs -cxx-interoperability-mode=default -module-name main %t%{fs-sep}cursed.swift

//--- blessed.swift

import PrivateFileIDUsing

extension PublUser {
  mutating func ext() {
    let _ = protField
    protField = 42
    protMethod()
    let _ = protFieldNoUsing
    let _ = protFieldProtUsing
  }
}

extension PrivUser {
  mutating func ext() {
    let _ = protField
    let _ = publField
    protField = 42
    publField = 42
    protMethod()
    publMethod()
    let _ = protFieldNoUsing
    let _ = protFieldProtUsing
  }
}

extension PublPrivUser {
  mutating func ext() {
    let _ = protField
    protField = 42
    let _ = protFieldNoUsing // expected-error {{'protFieldNoUsing' is unavailable: this base member is not accessible because of private inheritance}}
    let _ = protFieldProtUsing
  }
}

//--- cursed.swift

import PrivateFileIDUsing

func f(_ publUser: inout PublUser, _ privUser: inout PrivUser,
       _ publPrivUser: inout PublPrivUser) {
  let _ = publUser.protField
  publUser.protField = 42
  publUser.protMethod()
  let _ = publUser.publField
  let _ = publUser.protFieldNoUsing // expected-error {{'protFieldNoUsing' is inaccessible due to 'private' protection level}}
  let _ = publUser.protFieldProtUsing // expected-error {{'protFieldProtUsing' is inaccessible due to 'private' protection level}}

  let _ = privUser.protField
  let _ = privUser.publField
  privUser.protField = 42
  privUser.publField = 42
  privUser.protMethod()
  privUser.publMethod()
  let _ = privUser.protFieldNoUsing // expected-error {{'protFieldNoUsing' is inaccessible due to 'private' protection level}}
  let _ = privUser.protFieldProtUsing // expected-error {{'protFieldProtUsing' is inaccessible due to 'private' protection level}}

  let _ = publPrivUser.protField
  let _ = publPrivUser.publField
  publPrivUser.protField = 42
  publPrivUser.publField = 42
  publPrivUser.protMethod()
  publPrivUser.publMethod()
  let _ = publPrivUser.protFieldProtUsing // expected-error {{'protFieldProtUsing' is inaccessible due to 'private' protection level}}
}
