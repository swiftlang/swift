// RUN: %target-typecheck-verify-swift -swift-version 5

struct ImplicitMembers: Equatable {
    struct Other {
        var implicit: ImplicitMembers { ImplicitMembers() }
    }

    static var other = Other()
    static func createOther() -> Other {
        Other()
    }
    var anotherOther: Other { Other() }
    func getAnotherOther() -> Other {
        Other()
    }

    static var implicit = ImplicitMembers()
    static func createImplicit() -> ImplicitMembers {
        ImplicitMembers()
    }

    static var optional: ImplicitMembers? = ImplicitMembers()
    static func createOptional() -> ImplicitMembers? {
        ImplicitMembers()
    }
    static var superOptional: ImplicitMembers??? = ImplicitMembers()

    var another: ImplicitMembers { ImplicitMembers() }

    func getAnother() -> ImplicitMembers {
        ImplicitMembers()
    }

    func getAnother(arg: Int) -> ImplicitMembers {
        ImplicitMembers()
    }

    var anotherOptional: ImplicitMembers? { ImplicitMembers() }

    func getAnotherOptional() -> ImplicitMembers? {
        ImplicitMembers()
    }

    func getAnotherOptional(arg: Int) -> ImplicitMembers? {
        ImplicitMembers()
    }
    
    subscript(arg: Void) -> ImplicitMembers { ImplicitMembers() }
    subscript(optional arg: Void) -> ImplicitMembers? { ImplicitMembers() }
    subscript(func arg: Void) -> (() -> ImplicitMembers) { { ImplicitMembers() } }
    subscript(funcOptional arg: Void) -> (() -> ImplicitMembers?) { { ImplicitMembers() } }
    subscript(optionalFunc arg: Void) -> (() -> ImplicitMembers)? { { ImplicitMembers() } }
    subscript(other arg: Void) -> Other { Other() }
}

let _: ImplicitMembers = .implicit
let _: ImplicitMembers? = .implicit
let _: ImplicitMembers? = .optional

let _: ImplicitMembers = .implicit.another.another
let _: ImplicitMembers = .createImplicit().another.another
let _: ImplicitMembers = .init().another.another

let _: ImplicitMembers = .implicit.getAnother().another
let _: ImplicitMembers = .createImplicit().getAnother().another
let _: ImplicitMembers = .init().getAnother().another

let _: ImplicitMembers = .implicit.getAnother(arg: 0).another
let _: ImplicitMembers = .createImplicit().getAnother(arg: 0).another
let _: ImplicitMembers = .init().getAnother(arg: 0).another

let _: ImplicitMembers = .implicit.another.getAnother()
let _: ImplicitMembers = .createImplicit().another.getAnother()
let _: ImplicitMembers = .init().another.getAnother()

let _: ImplicitMembers = .implicit.another.getAnother(arg: 0)
let _: ImplicitMembers = .createImplicit().another.getAnother(arg: 0)
let _: ImplicitMembers = .init().another.getAnother(arg: 0)

let _: ImplicitMembers = .implicit.getAnother().getAnother(arg: 0)
let _: ImplicitMembers = .createImplicit().getAnother().getAnother(arg: 0)
let _: ImplicitMembers = .init().getAnother().getAnother(arg: 0)

let _: ImplicitMembers = .implicit.getAnother().getAnother(arg: 0).another
let _: ImplicitMembers = .createImplicit().getAnother().getAnother(arg: 0).another
let _: ImplicitMembers = .init().getAnother().getAnother(arg: 0).another

let _: ImplicitMembers = .implicit.another.getAnother().getAnother(arg: 0)
let _: ImplicitMembers = .createImplicit().another.getAnother().getAnother(arg: 0)
let _: ImplicitMembers = .init().another.getAnother().getAnother(arg: 0)

let _: ImplicitMembers = .implicit.another.another.another.another.another
let _: ImplicitMembers = .implicit.getAnother().getAnother().getAnother().getAnother().getAnother()
let _: ImplicitMembers = .implicit.getAnother(arg: 0).getAnother(arg: 0).getAnother(arg: 0).getAnother(arg: 0).getAnother(arg: 0)

let _: ImplicitMembers = .optional!
let _: ImplicitMembers = .optional!.another
let _: ImplicitMembers = .createOptional()!.another
let _: ImplicitMembers = .optional!.anotherOptional!
let _: ImplicitMembers = .createOptional()!.anotherOptional!
let _: ImplicitMembers = .optional!.getAnotherOptional()!
let _: ImplicitMembers = .createOptional()!.getAnotherOptional()!

let _: ImplicitMembers = .optional // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{35-35= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{35-35=!}}
let _: ImplicitMembers = .implicit.anotherOptional // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{51-51= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{51-51=!}}
let _: ImplicitMembers = .createOptional() // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{43-43= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{43-43=!}}
let _: ImplicitMembers = .implicit.getAnotherOptional() // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{56-56= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{56-56=!}}
let _: ImplicitMembers = .implicit[optional: ()] // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{49-49= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{49-49=!}}
let _: ImplicitMembers = .implicit[funcOptional: ()]() // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{55-55= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{55-55=!}}

// FIXME: Improve these diagnostics (should probably offer unwrapping, as above)
let _: ImplicitMembers = .implicit.anotherOptional?.another // expected-error{{cannot infer contextual base in reference to member 'implicit'}} expected-error{{cannot convert value of type 'Optional<_>' to specified type 'ImplicitMembers'}}
let _: ImplicitMembers = .implicit[optionalFunc: ()]?() // expected-error{{cannot infer contextual base in reference to member 'implicit'}} expected-error{{cannot convert value of type 'Optional<_>' to specified type 'ImplicitMembers'}}


let _: ImplicitMembers = .other.implicit
let _: ImplicitMembers = .implicit.anotherOther.implicit
let _: ImplicitMembers = .createOther().implicit
let _: ImplicitMembers = .implicit.getAnotherOther().implicit
let _: ImplicitMembers = .implicit[other: ()].implicit

let _: ImplicitMembers = .other // expected-error {{member 'other' in 'ImplicitMembers' produces result of type 'ImplicitMembers.Other', but context expects 'ImplicitMembers'}}
let _: ImplicitMembers = .implicit.anotherOther // expected-error {{member 'anotherOther' in 'ImplicitMembers' produces result of type 'ImplicitMembers.Other', but context expects 'ImplicitMembers'}}
let _: ImplicitMembers = .implicit.anotherOther.nonDeclaredMember // expected-error {{value of type 'ImplicitMembers.Other' has no member 'nonDeclaredMember'}}
let _: ImplicitMembers = .implicit.anotherOther.nonDeclaredMethod() // expected-error {{value of type 'ImplicitMembers.Other' has no member 'nonDeclaredMethod'}}
let _: ImplicitMembers = .implicit.anotherOther.nonDeclaredMember.another // expected-error {{value of type 'ImplicitMembers.Other' has no member 'nonDeclaredMember'}}
let _: ImplicitMembers = .implicit.anotherOther.nonDeclaredMethod().another // expected-error {{value of type 'ImplicitMembers.Other' has no member 'nonDeclaredMethod'}}
let _: ImplicitMembers = .implicit.getAnotherOther() // expected-error {{member 'getAnotherOther()' in 'ImplicitMembers' produces result of type 'ImplicitMembers.Other', but context expects 'ImplicitMembers'}}
let _: ImplicitMembers = .implicit[other: ()] // expected-error {{member 'subscript(other:)' in 'ImplicitMembers' produces result of type 'ImplicitMembers.Other', but context expects 'ImplicitMembers'}}

let _: ImplicitMembers? = .implicit.another
let _: ImplicitMembers? = .implicit.anotherOptional

let _: ImplicitMembers? = .optional
let _: ImplicitMembers? = .optional?.another
let _: ImplicitMembers? = .optional?.anotherOptional
let _: ImplicitMembers? = .optional?.getAnother()
let _: ImplicitMembers? = .optional?.getAnotherOptional()
let _: ImplicitMembers? = .optional?.anotherOptional?.another
let _: ImplicitMembers? = .optional?.getAnotherOptional()?.another
let _: ImplicitMembers? = .createOptional()
let _: ImplicitMembers? = .createOptional()?.another
let _: ImplicitMembers? = .createOptional()?.anotherOptional
let _: ImplicitMembers? = .createOptional()?.getAnother()
let _: ImplicitMembers? = .createOptional()?.getAnotherOptional()
let _: ImplicitMembers? = .createOptional()?.anotherOptional?.another
let _: ImplicitMembers? = .createOptional()?.getAnotherOptional()?.another
// FIXME: This should be allowed
// let _: ImplicitMembers? = .superOptional???.another

let _: ImplicitMembers = .implicit[()]
let _: ImplicitMembers = .implicit[optional: ()]!
let _: ImplicitMembers? = .implicit[optional: ()]
let _: ImplicitMembers = .implicit[func: ()]()
let _: ImplicitMembers = .implicit[funcOptional: ()]()!
let _: ImplicitMembers? = .implicit[funcOptional: ()]()
let _: ImplicitMembers = .implicit[optionalFunc: ()]!()
let _: ImplicitMembers? = .implicit[optionalFunc: ()]?()
let _: ImplicitMembers = .implicit.another[()]
let _: ImplicitMembers = .implicit.another[optional: ()]!
let _: ImplicitMembers? = .implicit.another[optional: ()]
let _: ImplicitMembers = .implicit.another[func: ()]()
let _: ImplicitMembers = .implicit.another[funcOptional: ()]()!
let _: ImplicitMembers? = .implicit.another[funcOptional: ()]()
let _: ImplicitMembers = .implicit.another[optionalFunc: ()]!()
let _: ImplicitMembers? = .implicit.another[optionalFunc: ()]?()
let _: ImplicitMembers = .implicit[()].another
let _: ImplicitMembers = .implicit[optional: ()]!.another
let _: ImplicitMembers? = .implicit[optional: ()]?.another
let _: ImplicitMembers = .implicit[func: ()]().another
let _: ImplicitMembers = .implicit[funcOptional: ()]()!.another
let _: ImplicitMembers? = .implicit[funcOptional: ()]()?.another
let _: ImplicitMembers = .implicit[optionalFunc: ()]!().another
let _: ImplicitMembers? = .implicit[optionalFunc: ()]?().another

func implicit(_ i: inout ImplicitMembers) {
    if i == .implicit {}
    if i == .implicit.another {}
    if i == .implicit.getAnother() {}
    if i == .optional?.another {}
    if i == .optional!.another {}
    if i == .createOptional()?.another {}
}

struct ImplicitGeneric<T> {
    static var implicit: ImplicitGeneric<T> { ImplicitGeneric<T>() }
    var another: ImplicitGeneric<T> { ImplicitGeneric<T>() }
    func getAnother() -> ImplicitGeneric<T> {
        ImplicitGeneric<T>()
    }
}

extension ImplicitGeneric where T == Int {
    static var implicitInt: ImplicitGeneric<Int> { ImplicitGeneric<Int>() }
    static var implicitString: ImplicitGeneric<String> { ImplicitGeneric<String>() }
    var anotherInt: ImplicitGeneric<Int> { ImplicitGeneric<Int>() }
    var anotherIntString: ImplicitGeneric<String> { ImplicitGeneric<String>() }
    func getAnotherInt() -> ImplicitGeneric<Int> {
        ImplicitGeneric<Int>()
    }
}

extension ImplicitGeneric where T == String {
    static var implicitString: ImplicitGeneric<String> { ImplicitGeneric<String>() }
    var anotherString: ImplicitGeneric<String> { ImplicitGeneric<String>() }
    var anotherStringInt: ImplicitGeneric<Int> { ImplicitGeneric<Int>() }
    func getAnotherString() -> ImplicitGeneric<String> {
        ImplicitGeneric<String>()
    }
    func getAnotherStringInt() -> ImplicitGeneric<Int> {
        ImplicitGeneric<Int>()
    }
}

func implicit<T>(_ arg: ImplicitGeneric<T>) {}

implicit(.implicitInt)
implicit(.implicit.anotherInt)
implicit(.implicit.anotherInt.another)
implicit(.implicit.another.anotherInt)
implicit(.implicit.getAnotherInt())
implicit(.implicit.another.getAnotherInt())
implicit(.implicit.getAnother().anotherInt)
implicit(.implicit.getAnotherInt())
implicit(.implicit.getAnother().getAnotherInt())
implicit(.implicitString.anotherStringInt)
// Member types along the chain can have different generic arguments
implicit(.implicit.anotherIntString.anotherStringInt)

implicit(.implicit.anotherString.anotherStringInt) // expected-error {{type of expression is ambiguous without more context}}
implicit(.implicit.getAnotherString().anotherStringInt) // expected-error {{type of expression is ambiguous without more context}}
implicit(.implicit.anotherString.getAnotherStringInt()) // expected-error {{type of expression is ambiguous without more context}}
implicit(.implicit.getAnotherString().getAnotherStringInt()) // expected-error {{type of expression is ambiguous without more context}}
