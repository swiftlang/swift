// RUN: %target-typecheck-verify-swift -swift-version 5

protocol P {}

struct S : P {
  var other: S { S() }
}


struct G<T> : P {
  var other: G<T> { fatalError() }
}

extension P where Self == S {
  static var property: S { S() }

  static var iuoProp: S! { S() }
  static var optProp: S? { S() }

  static var fnProp: () -> S {
    { S() }
  }

  static func method() -> S {
    return S()
  }

  static subscript(_: Int) -> S {
    get { S() }
  }
}

extension P {
  static func genericFn<T>(_: T) -> G<T> where Self == G<T> {
    return G<T>()
  }

  static subscript<T>(t t: T) -> G<T> where Self == G<T> {
    get { G<T>() }
  }
}

// References on protocol metatype are only allowed through a leading dot syntax

_ = P.property // expected-error {{static member 'property' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.property.other // expected-error {{static member 'property' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.iuoProp // expected-error {{static member 'iuoProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.iuoProp.other // expected-error {{static member 'iuoProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.optProp // expected-error {{static member 'optProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.optProp?.other // expected-error {{static member 'optProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.fnProp // expected-error {{static member 'fnProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.fnProp() // expected-error {{static member 'fnProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.fnProp().other // expected-error {{static member 'fnProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.method() // expected-error {{static member 'method' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.method   // expected-error {{static member 'method' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.method().other // expected-error {{static member 'method' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.genericFn(42) // expected-error {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.genericFn(42).other // expected-error {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
_ = P[42] // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
_ = P[42].other // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
_ = P[t: 42] // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
_ = P[t: 42].other // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}

let _: S = P.property // expected-error {{static member 'property' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P.property.other // expected-error {{static member 'property' cannot be used on protocol metatype 'P.Protocol'}}
let _: () -> S = P.fnProp // expected-error {{static member 'fnProp' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P.fnProp() // expected-error {{static member 'fnProp' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P.fnProp().other // expected-error {{static member 'fnProp' cannot be used on protocol metatype 'P.Protocol'}}
let _: () -> S = P.method // expected-error {{static member 'method' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P.method() // expected-error {{static member 'method' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P.method().other // expected-error {{static member 'method' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<Int> = P.genericFn(42) // expected-error {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
let _: G = P.genericFn(42) // expected-error {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<String> = P.genericFn(42) // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
// expected-error@-1 {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<Int> = P.genericFn(42).other // expected-error {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<String> = P.genericFn(42).other // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
// expected-error@-1 {{static member 'genericFn' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P[42] // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
let _: S = P[42].other // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<Int> = P[t: 42] // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
let _: G = P[t: 42] // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<String> = P[t: 42] // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
// expected-error@-1 {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<Int> = P[t: 42].other // expected-error {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
let _: G<String> = P[t: 42].other // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
// expected-error@-1 {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}

func test<T: P>(_: T) {}

test(.property) // Ok, base is inferred as Style.Type
test(.property.other) // Ok
test(.iuoProp) // Ok
test(.iuoProp.other) // Ok
test(.optProp!) // Ok
test(.optProp)
// expected-error@-1 {{value of optional type 'S?' must be unwrapped to a value of type 'S'}}
// expected-note@-2 {{coalesce using '??' to provide a default when the optional value contains 'nil'}}
// expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
test(.optProp!.other) // Ok
test(.fnProp()) // Ok
test(.fnProp().other) // Ok
test(.method()) // Ok, static method call on the metatype
test(.method().other) // Ok
test(.genericFn(42)) // Ok
test(.genericFn(42).other) // Ok

protocol Q {}

func test_combo<T: P & Q>(_: T) {} // expected-note 2 {{where 'T' = 'G<Int>'}}

extension Q where Self == S {
  static var otherProperty: S { S() }

  static func otherMethod() -> S {
    return S()
  }

  static func otherGeneric<T>(_: T) -> S {
    return S()
  }
}

extension S : Q {
}

test_combo(.property) // Ok
test_combo(.method()) // Ok
test_combo(.otherProperty) // Ok
test_combo(.otherProperty.other) // Ok
test_combo(.otherProperty.property) // expected-error {{static member 'property' cannot be used on instance of type 'S'}}
test_combo(.otherMethod()) // Ok
test_combo(.otherMethod().method()) // expected-error {{static member 'method' cannot be used on instance of type 'S'}}
test_combo(.otherGeneric(42)) // Ok

test_combo(.genericFn(42)) // expected-error {{global function 'test_combo' requires that 'G<Int>' conform to 'Q'}}

/* Invalid result types */

extension P {
  static func generic<T>(_: T) -> T where Self == T { fatalError() } // expected-note 3 {{'generic' declared here}}
  static func genericWithReqs<T: Collection, Q>(_: T) -> Q where T.Element == Q, Self == Q { // expected-note {{in call to function 'genericWithReqs'}} expected-note 2 {{'genericWithReqs' declared here}} expected-note 3 {{required by static method 'genericWithReqs' where 'T' = '()'}}
    fatalError()
  }
}

extension P {
  static var invalidProp: Int { 42 } // expected-note 3 {{'invalidProp' declared here}}
  static var selfProp: Self { fatalError() }
  static func invalidMethod() -> Int { 42 } // expected-note 3 {{'invalidMethod()' declared here}}
  static subscript(q q: String) -> Int { get { 42 } }
}

_ = P.doesntExist // expected-error {{type 'P' has no member 'doesntExist'}}
_ = P.selfProp // expected-error {{static member 'selfProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.invalidProp
// expected-error@-1 {{static member 'invalidProp' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.invalidProp.other
// expected-error@-1 {{static member 'invalidProp' cannot be used on protocol metatype 'P.Protocol'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
_ = P.invalidMethod // Partial application with an invalid base type
// expected-error@-1 {{static member 'invalidMethod' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.invalidMethod()
// expected-error@-1 {{static member 'invalidMethod' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.invalidMethod().other
// expected-error@-1 {{static member 'invalidMethod' cannot be used on protocol metatype 'P.Protocol'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
_ = P.generic(42)
// expected-error@-1 {{static member 'generic' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.generic(42).other
// expected-error@-1 {{static member 'generic' cannot be used on protocol metatype 'P.Protocol'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
_ = P.generic(S()) // expected-error {{static member 'generic' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.generic(S()).other // expected-error {{static member 'generic' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.generic(G<Int>()) // expected-error {{static member 'generic' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.genericWithReqs([S()]) // expected-error {{static member 'genericWithReqs' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.genericWithReqs([42])
// expected-error@-1 {{static member 'genericWithReqs' cannot be used on protocol metatype 'P.Protocol'}}
_ = P.genericWithReqs(())
// expected-error@-1 {{type '()' cannot conform to 'Collection'}} expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}
// expected-error@-2 {{static member 'genericWithReqs' cannot be used on protocol metatype 'P.Protocol'}}
// expected-error@-3 {{generic parameter 'Q' could not be inferred}}
_ = P[q: ""]
// expected-error@-1 {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
_ = P[q: ""].other
// expected-error@-1 {{static member 'subscript' cannot be used on protocol metatype 'P.Protocol'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}

test(.doesntExist) // expected-error {{type 'P' has no member 'doesntExist'}}
test(.doesnt.exist()) // expected-error {{type 'P' has no member 'doesnt'}}
test(.invalidProp)
// expected-error@-1 {{cannot reference static property 'invalidProp' on 'P.Protocol' with non-conforming result type 'Int'}}
test(.invalidProp.other)
// expected-error@-1 {{cannot reference static property 'invalidProp' on 'P.Protocol' with non-conforming result type 'Int'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
test(.invalidMethod())
// expected-error@-1 {{cannot reference static method 'invalidMethod()' on 'P.Protocol' with non-conforming result type 'Int'}}
test(.invalidMethod().other)
// expected-error@-1 {{cannot reference static method 'invalidMethod()' on 'P.Protocol' with non-conforming result type 'Int'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
test(.generic(42))
// expected-error@-1 {{cannot reference static method 'generic' on 'P.Protocol' with non-conforming result type 'Int'}}
test(.generic(42).other)
// expected-error@-1 {{cannot reference static method 'generic' on 'P.Protocol' with non-conforming result type 'Int'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
test(.generic(S())) // Ok
test(.generic(G<Int>())) // Ok
test(.genericWithReqs([S()])) // Ok
test(.genericWithReqs([42]))
// expected-error@-1 {{cannot reference static method 'genericWithReqs' on 'P.Protocol' with non-conforming result type 'Int'}}
test(.genericWithReqs(()))
// expected-error@-1 {{type '()' cannot conform to 'Collection'}} expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}

test_combo(.doesntExist) // expected-error {{reference to member 'doesntExist' cannot be resolved without a contextual type}}
test_combo(.doesnt.exist()) // expected-error {{reference to member 'doesnt' cannot be resolved without a contextual type}}
test_combo(.invalidProp)
// expected-error@-1 {{cannot reference static property 'invalidProp' on 'P.Protocol' with non-conforming result type 'Int'}}
test_combo(.invalidMethod())
// expected-error@-1 {{cannot reference static method 'invalidMethod()' on 'P.Protocol' with non-conforming result type 'Int'}}
test_combo(.generic(42))
// expected-error@-1 {{cannot reference static method 'generic' on 'P.Protocol' with non-conforming result type 'Int'}}
test_combo(.generic(S())) // Ok
test_combo(.generic(G<Int>())) // expected-error {{global function 'test_combo' requires that 'G<Int>' conform to 'Q'}}
test_combo(.genericWithReqs([S()])) // Ok
test_combo(.genericWithReqs([42]))
// expected-error@-1 {{cannot reference static method 'genericWithReqs' on 'P.Protocol' with non-conforming result type 'Int'}}
test_combo(.genericWithReqs(()))
// expected-error@-1 {{type '()' cannot conform to 'Collection'}} expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}

protocol Z {
  associatedtype T = Int

  static var prop: T { get }
}

extension Z {
  static func method() -> T { fatalError() }
}

_ = Z.prop
// expected-error@-1 {{member 'prop' cannot be used on value of protocol type 'Z.Protocol'; use a generic constraint instead}}
// expected-error@-2 {{protocol 'Z' can only be used as a generic constraint because it has Self or associated type requirements}}

_ = Z.method()
// expected-error@-1 {{member 'method' cannot be used on value of protocol type 'Z.Protocol'; use a generic constraint instead}}
// expected-error@-2 {{protocol 'Z' can only be used as a generic constraint because it has Self or associated type requirements}}
