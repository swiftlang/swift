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
    static let implicitLet = ImplicitMembers() // expected-note2 {{change 'let' to 'var' to make it mutable}}
    static var implicitImmutable: ImplicitMembers { ImplicitMembers() }
    static func createImplicit() -> ImplicitMembers {
        ImplicitMembers()
    }

    static var optional: ImplicitMembers? = ImplicitMembers()
    static func createOptional() -> ImplicitMembers? {
        ImplicitMembers()
    }
    static var superOptional: ImplicitMembers??? = ImplicitMembers()

    static func createIUOArg(_: Int) -> ImplicitMembers { ImplicitMembers() }
    var anotherIUO: ImplicitMembers! { ImplicitMembers() }
    func getAnotherIUO() -> ImplicitMembers! { ImplicitMembers() }

    var another: ImplicitMembers { ImplicitMembers() }
    var anotherMutable: ImplicitMembers {
        get { ImplicitMembers() }
        set {}
    }

    func getAnother() -> ImplicitMembers {
        ImplicitMembers()
    }

    func getAnother(arg: Int) -> ImplicitMembers {
        ImplicitMembers()
    }

    var anotherOptional: ImplicitMembers? { ImplicitMembers() }
    var anotherOptionalMutable: ImplicitMembers? {
        get { ImplicitMembers() }
        set {}
    }

    func getAnotherOptional() -> ImplicitMembers? {
        ImplicitMembers()
    }

    func getAnotherOptional(arg: Int) -> ImplicitMembers? {
        ImplicitMembers()
    }

    static func takesClosure(_: (Int) -> Void) -> ImplicitMembers { ImplicitMembers() }
    static func takesArgClosure(_: Int, _: (Int) -> Void) -> ImplicitMembers { ImplicitMembers() }
    func methodTakesClosure(_: (Int) -> Void) -> ImplicitMembers { ImplicitMembers() }
    func methodTakesArgClosure(_: Int, _: (Int) -> Void) -> ImplicitMembers { ImplicitMembers() }
    
    subscript(arg: Void) -> ImplicitMembers {
        get { ImplicitMembers() }
        set {}
    }
    subscript(optional arg: Void) -> ImplicitMembers? {
        get { ImplicitMembers() }
        set {}
    }
    subscript(immutable arg: Void) -> ImplicitMembers { ImplicitMembers() }
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

let _: ImplicitMembers = .implicit.another.getAnother().getAnother(arg: 0).anotherIUO
let _: ImplicitMembers = .createImplicit().another.getAnother().getAnother(arg: 0).anotherIUO
let _: ImplicitMembers = .init().another.getAnother().getAnother(arg: 0).anotherIUO

let _: ImplicitMembers = .implicit.another.getAnother().getAnother(arg: 0).getAnotherIUO()
let _: ImplicitMembers = .createImplicit().another.getAnother().getAnother(arg: 0).getAnotherIUO()
let _: ImplicitMembers = .init().another.getAnother().getAnother(arg: 0).getAnotherIUO()

let _: ImplicitMembers = .createIUOArg(_:)(0)

let _: ImplicitMembers = .optional!
let _: ImplicitMembers = .optional!.another
let _: ImplicitMembers = .createOptional()!.another
let _: ImplicitMembers = .optional!.anotherOptional!
let _: ImplicitMembers = .createOptional()!.anotherOptional!
let _: ImplicitMembers = .optional!.getAnotherOptional()!
let _: ImplicitMembers = .createOptional()!.getAnotherOptional()!
let _: ImplicitMembers = .implicit.getAnotherIUO()
let _: ImplicitMembers = .createImplicit().anotherIUO
let _: ImplicitMembers = .implicit.anotherIUO
let _: ImplicitMembers = .createImplicit().anotherIUO

let _: ImplicitMembers = .optional // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{35-35= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{35-35=!}}
let _: ImplicitMembers = .implicit.anotherOptional // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{51-51= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{51-51=!}}
let _: ImplicitMembers = .createOptional() // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{43-43= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{43-43=!}}
let _: ImplicitMembers = .implicit.getAnotherOptional() // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{56-56= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{56-56=!}}
let _: ImplicitMembers = .implicit[optional: ()] // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{49-49= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{49-49=!}}
let _: ImplicitMembers = .implicit[funcOptional: ()]() // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}} expected-note {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{55-55= ?? <#default value#>}} expected-note {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{55-55=!}}

let _: ImplicitMembers = .implicit.anotherOptional?.another // expected-error {{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}}
// expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{60-60= ?? <#default value#>}}
// expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{26-26=(}} {{60-60=)!}}
let _: ImplicitMembers = .implicit[optionalFunc: ()]?() // expected-error{{value of optional type 'ImplicitMembers?' must be unwrapped to a value of type 'ImplicitMembers'}}
// expected-note@-1 {{coalesce using '??' to provide a default when the optional value contains 'nil'}} {{56-56= ?? <#default value#>}}
// expected-note@-2 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}} {{26-26=(}} {{56-56=)!}}

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
let _: ImplicitMembers? = .createOptional()?.getAnotherOptional()?.anotherIUO
let _: ImplicitMembers? = .createOptional()?.getAnotherOptional()?.getAnotherIUO()
// FIXME: This should be allowed
// let _: ImplicitMembers? = .superOptional???.another

let _: ImplicitMembers = .takesClosure { _ in }
let _: ImplicitMembers = .takesArgClosure(0) { _ in }
let _: ImplicitMembers = .implicit.methodTakesClosure { _ in }
let _: ImplicitMembers = .implicit.methodTakesArgClosure(0) { _ in }
let _: ImplicitMembers? = .optional?.methodTakesClosure { _ in }
let _: ImplicitMembers? = .optional?.methodTakesArgClosure(0) { _ in }

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
let _: ImplicitMembers = .implicit.self

func implicit(_ i: inout ImplicitMembers) {
    if i == .implicit {}
    if i == .implicit.another {}
    if i == .implicit.getAnother() {}
    if i == .optional?.another {}
    if i == .optional!.another {}
    if i == .createOptional()?.another {}
}

func testLValues() {
    let local = ImplicitMembers();

    .implicit = local;
    .implicit.anotherMutable = local;
    .implicit.anotherOptionalMutable? = local;
    .implicit.anotherOptionalMutable! = local;
    .implicit[()] = local;
    .implicit[()].anotherMutable = local;
    .optional?[optional: ()]?.anotherOptionalMutable! = local;

    .implicitLet = local; // expected-error {{cannot assign to property: 'implicitLet' is a 'let' constant}}
    .implicitImmutable = local; // expected-error {{cannot assign to property: 'implicitImmutable' is a get-only property}}
    .createImplicit() = local; // expected-error {{expression is not assignable: function call returns immutable value}}
    .implicit.another = local; // expected-error {{cannot assign to property: 'another' is a get-only property}}
    .implicit[immutable: ()] = local; // expected-error {{cannot assign through subscript: subscript is get-only}}
    .implicit.getAnother() = local; // expected-error {{expression is not assignable: function call returns immutable value}}

    .implicitLet.anotherMutable = local; // expected-error {{cannot assign to property: 'implicitLet' is a 'let' constant}}
    .implicitImmutable.anotherMutable = local; // expected-error {{cannot assign to property: 'implicitImmutable' is a get-only property}}
    .createImplicit().anotherMutable = local; // expected-error {{cannot assign to property: function call returns immutable value}}
    .implicit.another.anotherMutable = local; // expected-error {{cannot assign to property: 'another' is a get-only property}}
    .implicit[immutable: ()].anotherMutable = local; // expected-error {{cannot assign to property: subscript is get-only}}
    .implicit.getAnother().anotherMutable = local; // expected-error {{cannot assign to property: function call returns immutable value}}


    // FIXME: These should probably be allowed
    //.implicit.anotherOptionalMutable = local;
    //.optional = local;
}

struct ImplicitGeneric<T> { // expected-note4 {{arguments to generic parameter 'T' ('Int' and 'String') are expected to be equal}}
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

implicit(.implicit.anotherString.anotherStringInt) // expected-error {{member chain produces result of type 'ImplicitGeneric<Int>' but contextual base was inferred as 'ImplicitGeneric<String>'}}
implicit(.implicit.getAnotherString().anotherStringInt) // expected-error {{member chain produces result of type 'ImplicitGeneric<Int>' but contextual base was inferred as 'ImplicitGeneric<String>'}}
implicit(.implicit.anotherString.getAnotherStringInt()) // expected-error {{member chain produces result of type 'ImplicitGeneric<Int>' but contextual base was inferred as 'ImplicitGeneric<String>'}}
implicit(.implicit.getAnotherString().getAnotherStringInt()) // expected-error {{member chain produces result of type 'ImplicitGeneric<Int>' but contextual base was inferred as 'ImplicitGeneric<String>'}}

// Implicit member syntax can be used to apply curried instance methods:
struct Curried {
    func method() -> Curried { Curried() }
    func method(with arg: Int) -> Curried { Curried() }
    func method(with arg1: Int, and arg2: String) -> Curried { Curried() }
    func takesClosure(_: (Int) -> Void) -> Curried { Curried() }
    func takesArgClosure(_: Int, _: (Int) -> Void) -> Curried { Curried() }
    static func curried(_ _self: Curried) -> () -> Curried{ return { _self } }
    static func curriedWithArgs(_ _self: Curried) -> (Int, String) -> Curried { return { _, _ in _self } }
}

let _: Curried = .method(Curried())()
let _: Curried = .method(Curried())(with: 0)
let _: Curried = .method(Curried())(with: 0, and: "")
let _: Curried = .takesClosure(Curried())() { _ in }
let _: Curried = .takesArgClosure(Curried())(0) { _ in }
let _: Curried = .curried(Curried())()
let _: Curried = .curriedWithArgs(Curried())(0, "")


struct CurriedGeneric<T> {
    func create<U>(_: U.Type) -> CurriedGeneric<U> { return CurriedGeneric<U>() }
}

extension CurriedGeneric where T == Int {
    func createInt() -> Self {
        return self
    }
}

let _: CurriedGeneric = .createInt(CurriedGeneric())()
let _: CurriedGeneric = .create(CurriedGeneric())(Int.self)

// rdar://problem/68094328 - failed to compile unresolved member with implicit optional promotion
func rdar68094328() {
  struct S {
    init(string: String) {}

    var value: S {
      get { S(string: "") }
    }

    func baz(str: String) -> S {
      S(string: str)
    }
  }

  class C {
    func bar(_: S) {}
  }

  func foo<T>(_: (C) -> (T) -> Void, _: T?) {}

  func test(str: String) {
    foo(C.bar, .init(string: str)) // Ok
    foo(C.bar, .init(string: str).value) // Ok
    foo(C.bar, .init(string: str).baz(str: "")) // Ok
  }
}

// Ensure postfix operator is not a part of implicit member chain.
postfix operator ^
postfix func ^ (_ lhs: ImplicitMembers) -> Int { 0 }
func acceptInt(_ x: Int) {}
func postfixOpIsNotAMemberChain() {
  acceptInt(.implicit.another^)
}

// Ensure that base type doesn't get bound to a protocol type too eagerly
do {
  struct V : Hashable {
    static let v1: V = V()
    static let v2: V = V()
  }

  let _: Set = [V.v1, .v2] // Ok

  struct Elements : RandomAccessCollection {
    init() {}
    init(_ elements: [Int]) {}

    var startIndex: Int { 0 }
    var endIndex: Int { 0 }
    subscript(index: Int) -> Int { 0 }
  }

  struct TestNilCoalescing {
    var data: Elements?

    func test() {
      for _ in self.data ?? .init() {} // Ok
    }
  }
}
