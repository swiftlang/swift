// RUN: %target-typecheck-verify-swift -verify-ignore-unknown -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

import UsingNonPublic

let _ = PublUser().publUsingPubl()
let _ = PublUser().protUsingPubl() // expected-error {{'protUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PublUser().omitUsingPubl()
let _ = PublUser().publUsingProt()
let _ = PublUser().protUsingProt() // expected-error {{'protUsingProt' is inaccessible due to 'private' protection level}}
let _ = PublUser().omitUsingProt() // expected-error {{'omitUsingProt' is inaccessible due to 'private' protection level}}

let _ = ProtUser().publUsingPubl()
let _ = ProtUser().protUsingPubl() // expected-error {{'protUsingPubl' is inaccessible due to 'private' protection level}}
let _ = ProtUser().omitUsingPubl() // expected-error {{'omitUsingPubl' is inaccessible due to 'private' protection level}}
let _ = ProtUser().publUsingProt()
let _ = ProtUser().protUsingProt() // expected-error {{'protUsingProt' is inaccessible due to 'private' protection level}}
let _ = ProtUser().omitUsingProt() // expected-error {{'omitUsingProt' is inaccessible due to 'private' protection level}}

let _ = PrivUser().publUsingPubl()
let _ = PrivUser().protUsingPubl() // expected-error {{'protUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PrivUser().omitUsingPubl() // expected-error {{'omitUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PrivUser().publUsingProt()
let _ = PrivUser().protUsingProt() // expected-error {{'protUsingProt' is inaccessible due to 'private' protection level}}
let _ = PrivUser().omitUsingProt() // expected-error {{'omitUsingProt' is inaccessible due to 'private' protection level}}

let _ = PublPrivUser().publUsingPubl()
let _ = PublPrivUser().protUsingPubl() // expected-error {{'protUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PublPrivUser().omitUsingPubl() // expected-error {{'omitUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PublPrivUser().publUsingProt()
let _ = PublPrivUser().protUsingProt() // expected-error {{'protUsingProt' is inaccessible due to 'private' protection level}}
let _ = PublPrivUser().omitUsingProt() // expected-error {{'omitUsingProt' is inaccessible due to 'private' protection level}}

let _ = PrivUserPubl().publUsingPubl()
let _ = PrivUserPubl().protUsingPubl() // expected-error {{'protUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PrivUserPubl().omitUsingPubl() // expected-error {{'omitUsingPubl' is inaccessible due to 'private' protection level}}
let _ = PrivUserPubl().publUsingProt()
let _ = PrivUserPubl().protUsingProt() // expected-error {{'protUsingProt' is inaccessible due to 'private' protection level}}
let _ = PrivUserPubl().omitUsingProt() // expected-error {{'omitUsingProt' is inaccessible due to 'private' protection level}}
