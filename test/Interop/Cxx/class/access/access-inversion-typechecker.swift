// Testing a C++ type whose public members expose private members.
//
// This test effectively documents the expected behavior at this time of writing,
// but does not necessarily specify it (in the deliberate sense). In other words,
// there may be behaviors captured in these tests that deserve amending.

// RUN: %target-typecheck-verify-swift -I %S/Inputs -cxx-interoperability-mode=default -enable-experimental-feature ImportNonPublicCxxMembers
// REQUIRES: swift_feature_ImportNonPublicCxxMembers

import AccessInversion

let _ = Leaky() // A sanity check that just instantiating this doesn't cause trouble

func doSomething() { /* do nothing, actually :-) */ }

func usePrivateAlias(a: inout Leaky.AliasToPrivateAlias) -> Leaky.AliasToPrivateAlias {
    let leaky = Leaky()
    var r = Leaky.RecWithPrivateAlias()

    //
    // Binding (and annotating) PrivateAlias
    //

    let _ = a
    let _: Bool = a
    let _: Leaky.AliasToPrivateAlias = a
    let _: Leaky.PrivateAlias = a
    // expected-error@-1 {{'PrivateAlias' is inaccessible due to 'private' protection level}}

    let _ = r.mem
    let _: Bool = r.mem
    let _: Leaky.AliasToPrivateAlias = r.mem
    let _: Leaky.PrivateAlias = r.mem
    // expected-error@-1 {{'PrivateAlias' is inaccessible due to 'private' protection level}}

    let _ = Leaky.staticReturningPrivateAlias()
    let _: Bool = Leaky.staticReturningPrivateAlias()
    let _: Leaky.AliasToPrivateAlias = Leaky.staticReturningPrivateAlias()
    let _: Leaky.PrivateAlias = Leaky.staticReturningPrivateAlias()
    // expected-error@-1 {{'PrivateAlias' is inaccessible due to 'private' protection level}}

    let _ = leaky.methodReturningPrivateAlias()
    let _: Bool = leaky.methodReturningPrivateAlias()
    let _: Leaky.AliasToPrivateAlias = leaky.methodReturningPrivateAlias()
    let _: Leaky.PrivateAlias = leaky.methodReturningPrivateAlias()
    // expected-error@-1 {{'PrivateAlias' is inaccessible due to 'private' protection level}}

    //
    // Assigning/applying to PrivateAlias
    //

    a = true
    // a = a
    a = r.mem
    a = Leaky.staticReturningPrivateAlias()
    a = leaky.methodReturningPrivateAlias()

    r.mem = true
    r.mem = a
    // r.mem = r.mem
    r.mem = Leaky.staticReturningPrivateAlias()
    r.mem = leaky.methodReturningPrivateAlias()

    Leaky.staticTakingPrivateAlias(true)
    Leaky.staticTakingPrivateAlias(a)
    Leaky.staticTakingPrivateAlias(r.mem)
    Leaky.staticTakingPrivateAlias(Leaky.staticReturningPrivateAlias())
    Leaky.staticTakingPrivateAlias(leaky.methodReturningPrivateAlias())

    leaky.methodTakingPrivateAlias(true)
    leaky.methodTakingPrivateAlias(a)
    leaky.methodTakingPrivateAlias(r.mem)
    leaky.methodTakingPrivateAlias(Leaky.staticReturningPrivateAlias())
    leaky.methodTakingPrivateAlias(leaky.methodReturningPrivateAlias())

    //
    // Using PrivateAlias
    //

    if a { doSomething() }
    if r.mem { doSomething() }
    if Leaky.staticReturningPrivateAlias() { doSomething() }
    if leaky.methodReturningPrivateAlias() { doSomething() }
}

func usePrivateRec(a: inout Leaky.AliasToPrivateRec) -> Leaky.AliasToPrivateRec {
    let leaky = Leaky()
    var r = Leaky.RecWithPrivateRec()

    //
    // Binding (and annotating) PrivateRec
    //

    let _ = a
    let _: Leaky.AliasToPrivateRec = a
    let _: Leaky.PrivateRec = a
    // expected-error@-1 {{'PrivateRec' is inaccessible due to 'private' protection level}}

    let _ = r.mem
    let _: Leaky.AliasToPrivateRec = r.mem
    let _: Leaky.PrivateRec = r.mem
    // expected-error@-1 {{'PrivateRec' is inaccessible due to 'private' protection level}}

    let _ = Leaky.staticReturningPrivateRec()
    let _: Leaky.AliasToPrivateRec = Leaky.staticReturningPrivateRec()
    let _: Leaky.PrivateRec = Leaky.staticReturningPrivateRec()
    // expected-error@-1 {{'PrivateRec' is inaccessible due to 'private' protection level}}

    let _ = leaky.methodReturningPrivateRec()
    let _: Leaky.AliasToPrivateRec = leaky.methodReturningPrivateRec()
    let _: Leaky.PrivateRec = leaky.methodReturningPrivateRec()
    // expected-error@-1 {{'PrivateRec' is inaccessible due to 'private' protection level}}

    //
    // Assigning/applying to PrivateRec
    //

    // a = a
    a = r.mem
    a = Leaky.staticReturningPrivateRec()
    a = leaky.methodReturningPrivateRec()

    r.mem = a
    // r.mem = r.mem
    r.mem = Leaky.staticReturningPrivateRec()
    r.mem = leaky.methodReturningPrivateRec()

    Leaky.staticTakingPrivateRec(a)
    Leaky.staticTakingPrivateRec(r.mem)
    Leaky.staticTakingPrivateRec(Leaky.staticReturningPrivateRec())
    Leaky.staticTakingPrivateRec(leaky.methodReturningPrivateRec())

    leaky.methodTakingPrivateRec(a)
    leaky.methodTakingPrivateRec(r.mem)
    leaky.methodTakingPrivateRec(Leaky.staticReturningPrivateRec())
    leaky.methodTakingPrivateRec(leaky.methodReturningPrivateRec())

    //
    // Using PrivateRec
    //

    // NOTE: privateRecMethod() is not accessible here even though it is
    // a public method of PrivateRec.

    a.privateRecMethod()
    // expected-error@-1 {{'privateRecMethod' is inaccessible due to 'private' protection level}}
    r.mem.privateRecMethod()
    // expected-error@-1 {{'privateRecMethod' is inaccessible due to 'private' protection level}}
    Leaky.staticReturningPrivateRec().privateRecMethod()
    // expected-error@-1 {{'privateRecMethod' is inaccessible due to 'private' protection level}}
    leaky.methodReturningPrivateRec().privateRecMethod()
    // expected-error@-1 {{'privateRecMethod' is inaccessible due to 'private' protection level}}
}

func usePrivateEnum(a: inout Leaky.AliasToPrivateEnum) -> Leaky.AliasToPrivateEnum {
    let leaky = Leaky()
    var r = Leaky.RecWithPrivateEnum()

    //
    // Binding (and annotating) PrivateEnum
    //

    let _ = a
    let _: Leaky.AliasToPrivateEnum = a
    let _: Leaky.PrivateEnum = a
    // expected-error@-1 {{'PrivateEnum' is inaccessible due to 'private' protection level}}

    let _ = r.mem
    let _: Leaky.AliasToPrivateEnum = r.mem
    let _: Leaky.PrivateEnum = r.mem
    // expected-error@-1 {{'PrivateEnum' is inaccessible due to 'private' protection level}}

    let _ = Leaky.staticReturningPrivateEnum()
    let _: Leaky.AliasToPrivateEnum = Leaky.staticReturningPrivateEnum()
    let _: Leaky.PrivateEnum = Leaky.staticReturningPrivateEnum()
    // expected-error@-1 {{'PrivateEnum' is inaccessible due to 'private' protection level}}

    let _ = leaky.methodReturningPrivateEnum()
    let _: Leaky.AliasToPrivateEnum = leaky.methodReturningPrivateEnum()
    let _: Leaky.PrivateEnum = leaky.methodReturningPrivateEnum()
    // expected-error@-1 {{'PrivateEnum' is inaccessible due to 'private' protection level}}

    //
    // Assigning/applying to PrivateEnum
    //

    // a = a
    a = r.mem
    a = Leaky.staticReturningPrivateEnum()
    a = leaky.methodReturningPrivateEnum()

    r.mem = a
    // r.mem = r.mem
    r.mem = Leaky.staticReturningPrivateEnum()
    r.mem = leaky.methodReturningPrivateEnum()

    Leaky.staticTakingPrivateEnum(a)
    Leaky.staticTakingPrivateEnum(r.mem)
    Leaky.staticTakingPrivateEnum(Leaky.staticReturningPrivateEnum())
    Leaky.staticTakingPrivateEnum(leaky.methodReturningPrivateEnum())

    leaky.methodTakingPrivateEnum(a)
    leaky.methodTakingPrivateEnum(r.mem)
    leaky.methodTakingPrivateEnum(Leaky.staticReturningPrivateEnum())
    leaky.methodTakingPrivateEnum(leaky.methodReturningPrivateEnum())

    //
    // Constructing and reading PrivateEnum
    //

    // TODO: nested enum members are not being imported (#54905)
    // let _ = Leaky.privateEnumMember
    let rv0 = Leaky.AliasToPrivateEnum(rawValue: 0)!
    let _ = Leaky.PrivateEnum(rawValue: 0)!
    // expected-error@-1 {{'PrivateEnum' is inaccessible due to 'private' protection level}}

    let _ = rv0.rawValue
    let _: Leaky.AliasToPrivateEnum.RawValue = rv0.rawValue
    let _: Leaky.PrivateEnum.RawValue = rv0.rawValue
    // expected-error@-1 {{'PrivateEnum' is inaccessible due to 'private' protection level}}
}

func usePrivateEnumClass(a: inout Leaky.AliasToPrivateEnumClass) -> Leaky.AliasToPrivateEnumClass {
    let leaky = Leaky()
    var r = Leaky.RecWithPrivateEnumClass()

    //
    // Binding (and annotating) PrivateEnumClass
    //

    let _ = a
    let _: Leaky.AliasToPrivateEnumClass = a
    let _: Leaky.PrivateEnumClass = a
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}

    let _ = r.mem
    let _: Leaky.AliasToPrivateEnumClass = r.mem
    let _: Leaky.PrivateEnumClass = r.mem
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}

    let _ = Leaky.staticReturningPrivateEnumClass()
    let _: Leaky.AliasToPrivateEnumClass = Leaky.staticReturningPrivateEnumClass()
    let _: Leaky.PrivateEnumClass = Leaky.staticReturningPrivateEnumClass()
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}

    let _ = leaky.methodReturningPrivateEnumClass()
    let _: Leaky.AliasToPrivateEnumClass = leaky.methodReturningPrivateEnumClass()
    let _: Leaky.PrivateEnumClass = leaky.methodReturningPrivateEnumClass()
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}

    //
    // Assigning/applying to PrivateEnumClass
    //

    // a = a
    a = r.mem
    a = Leaky.staticReturningPrivateEnumClass()
    a = leaky.methodReturningPrivateEnumClass()

    r.mem = a
    // r.mem = r.mem
    r.mem = Leaky.staticReturningPrivateEnumClass()
    r.mem = leaky.methodReturningPrivateEnumClass()

    Leaky.staticTakingPrivateEnumClass(a)
    Leaky.staticTakingPrivateEnumClass(r.mem)
    Leaky.staticTakingPrivateEnumClass(Leaky.staticReturningPrivateEnumClass())
    Leaky.staticTakingPrivateEnumClass(leaky.methodReturningPrivateEnumClass())

    leaky.methodTakingPrivateEnumClass(a)
    leaky.methodTakingPrivateEnumClass(r.mem)
    leaky.methodTakingPrivateEnumClass(Leaky.staticReturningPrivateEnumClass())
    leaky.methodTakingPrivateEnumClass(leaky.methodReturningPrivateEnumClass())

    //
    // Constructing and reading PrivateEnumClass
    //

    // NOTE: private enum class members are not accessible even if we can access
    // instances of the private enum class via

    let _ = Leaky.AliasToPrivateEnumClass.privateEnumClassMember
    // expected-error@-1 {{'privateEnumClassMember' is inaccessible due to 'private' protection level}}
    let _ = Leaky.PrivateEnumClass.privateEnumClassMember
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}
    let _: Leaky.AliasToPrivateEnum = .privateEnumClassMember
    // expected-error@-1 {{type 'Leaky.AliasToPrivateEnum' (aka 'Leaky.PrivateEnum') has no member 'privateEnumClassMember'}}
    // TODO: ^this is not really the right error message

    let rv0 = Leaky.AliasToPrivateEnumClass(rawValue: 0)!
    let _ = Leaky.PrivateEnumClass(rawValue: 0)!
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}

    let _ = rv0.rawValue
    let _: Leaky.AliasToPrivateEnumClass.RawValue = rv0.rawValue
    let _: Leaky.PrivateEnumClass.RawValue = rv0.rawValue
    // expected-error@-1 {{'PrivateEnumClass' is inaccessible due to 'private' protection level}}

    switch rv0 {
    case .privateEnumClassMember:
    // expected-error@-1 {{'privateEnumClassMember' is inaccessible due to 'private' protection level}}
      doSomething()
    default:
      doSomething()
    }
}

func usePrivateDefaultArgs(leaky: Leaky) {
  leaky.defaultArgOfPrivateRec()
  leaky.defaultArgOfPrivateEnum()
  leaky.defaultArgOfPrivateEnumClass()
  leaky.defaultArgOfPrivateConst()
  leaky.defaultArgOfPrivateRecConst()
}

let privateAlias = Leaky.staticReturningPrivateAlias()
// expected-error@-1 {{constant must be declared private or fileprivate because its type 'Leaky.PrivateAlias' (aka 'Bool') uses a private type}}
let privateRec = Leaky.staticReturningPrivateRec()
// expected-error@-1 {{constant must be declared private or fileprivate because its type 'Leaky.PrivateRec' uses a private type}}
let privateEnum = Leaky.staticReturningPrivateEnum()
// expected-error@-1 {{constant must be declared private or fileprivate because its type 'Leaky.PrivateEnum' uses a private type}}
let privateEnumClass = Leaky.staticReturningPrivateEnumClass()
// expected-error@-1 {{constant must be declared private or fileprivate because its type 'Leaky.PrivateEnumClass' uses a private type}}

let aliasedPrivateAlias: Leaky.AliasToPrivateAlias = Leaky.staticReturningPrivateAlias()
let aliasedPrivateRec: Leaky.AliasToPrivateRec = Leaky.staticReturningPrivateRec()
let aliasedPrivateEnum: Leaky.AliasToPrivateEnum = Leaky.staticReturningPrivateEnum()
let aliasedPrivateEnumClass: Leaky.AliasToPrivateEnumClass = Leaky.staticReturningPrivateEnumClass()
