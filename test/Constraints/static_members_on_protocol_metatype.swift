// RUN: %target-typecheck-verify-swift -swift-version 5

protocol P {}

struct S : P {
  var other: S { S() }
}


struct G<T> : P {
  var other: G<T> { fatalError() }
}

extension P {
  static var property: S { S() }

  static var fnProp: () -> S {
    { S() }
  }

  static func method() -> S {
    return S()
  }

  static func genericFn<T>(_: T) -> G<T> {
    return G<T>()
  }

  static subscript(_: Int) -> S {
    get { S() }
  }

  static subscript<T>(t t: T) -> G<T> {
    get { G<T>() }
  }
}

_ = P.property // Ok
_ = P.property.other // Ok
_ = P.fnProp // Ok
_ = P.fnProp() // Ok
_ = P.fnProp().other // Ok
_ = P.method() // Ok
_ = P.method   // Ok (partial application)
_ = P.method().other // Ok
_ = P.genericFn(42) // Ok
_ = P.genericFn(42).other // Ok
_ = P[42] // Ok
_ = P[42].other // OK
_ = P[t: 42] // Ok
_ = P[t: 42].other // Ok

let _: S = P.property // Ok
let _: S = P.property.other // Ok
let _: () -> S = P.fnProp
let _: S = P.fnProp() // Ok
let _: S = P.fnProp().other // Ok
let _: () -> S = P.method // Ok
let _: S = P.method() // Ok
let _: S = P.method().other // Ok
let _: G<Int> = P.genericFn(42) // Ok
let _: G = P.genericFn(42) // Ok
let _: G<String> = P.genericFn(42) // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
let _: G<Int> = P.genericFn(42).other // Ok
let _: G<String> = P.genericFn(42).other // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
let _: S = P[42] // Ok
let _: S = P[42].other // OK
let _: G<Int> = P[t: 42] // Ok
let _: G = P[t: 42] // Ok
let _: G<String> = P[t: 42] // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}
let _: G<Int> = P[t: 42].other // Ok
let _: G<String> = P[t: 42].other // expected-error {{cannot convert value of type 'Int' to expected argument type 'String'}}

func test<T: P>(_: T) {}

test(.property) // Ok, base is inferred as Style.Type
test(.property.other) // Ok
test(.fnProp()) // Ok
test(.fnProp().other) // Ok
test(.method()) // Ok, static method call on the metatype
test(.method().other) // Ok
test(.genericFn(42)) // Ok
test(.genericFn(42).other) // Ok

protocol Q {}

func test_combo<T: P & Q>(_: T) {} // expected-note 2 {{where 'T' = 'G<Int>'}}

extension Q {
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
  static var invalidProp: Int { 42 } // expected-note 5 {{'invalidProp' declared here}}
  static var selfProp: Self { fatalError() }
  static func invalidMethod() -> Int { 42 } // expected-note 6 {{'invalidMethod()' declared here}}
  static func generic<T>(_: T) -> T { fatalError() } // expected-note 5 {{'generic' declared here}}
  static func genericWithReqs<T: Collection, Q>(_: T) -> Q where T.Element == Q { // expected-note 3 {{'genericWithReqs' declared here}} expected-note 3 {{required by static method 'genericWithReqs' where 'T' = '()'}}
    fatalError()
  }

  static subscript(q q: String) -> Int { get { 42 } } // expected-note 2 {{'subscript(q:)' declared here}}
}

_ = P.doesntExist // expected-error {{type 'P' has no member 'doesntExist'}}
_ = P.selfProp // expected-error {{generic parameter 'Self' could not be inferred}}
_ = P.invalidProp
// expected-error@-1 {{cannot reference static property 'invalidProp' on 'P.Protocol' with non-conforming result type 'Int'}}
_ = P.invalidProp.other
// expected-error@-1 {{cannot reference static property 'invalidProp' on 'P.Protocol' with non-conforming result type 'Int'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
_ = P.invalidMethod // Partial application with an invalid base type
// expected-error@-1 {{cannot reference static method 'invalidMethod()' on 'P.Protocol' with non-conforming result type 'Int'}}
_ = P.invalidMethod()
// expected-error@-1 {{cannot reference static method 'invalidMethod()' on 'P.Protocol' with non-conforming result type 'Int'}}
_ = P.invalidMethod().other
// expected-error@-1 {{cannot reference static method 'invalidMethod()' on 'P.Protocol' with non-conforming result type 'Int'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
_ = P.generic(42)
// expected-error@-1 {{cannot reference static method 'generic' on 'P.Protocol' with non-conforming result type 'Int'}}
_ = P.generic(42).other
// expected-error@-1 {{cannot reference static method 'generic' on 'P.Protocol' with non-conforming result type 'Int'}}
// expected-error@-2 {{value of type 'Int' has no member 'other'}}
_ = P.generic(S()) // Ok
_ = P.generic(S()).other // Ok
_ = P.generic(G<Int>()) // Ok
_ = P.genericWithReqs([S()]) // Ok
_ = P.genericWithReqs([42])
// expected-error@-1 {{cannot reference static method 'genericWithReqs' on 'P.Protocol' with non-conforming result type 'Int'}}
_ = P.genericWithReqs(())
// expected-error@-1 {{type '()' cannot conform to 'Collection'}} expected-note@-1 {{only concrete types such as structs, enums and classes can conform to protocols}}
// expected-error@-2 {{generic parameter 'Self' could not be inferred}}
_ = P[q: ""]
// expected-error@-1 {{cannot reference static subscript 'subscript(q:)' on 'P.Protocol' with non-conforming result type 'Int'}}
_ = P[q: ""].other
// expected-error@-1 {{cannot reference static subscript 'subscript(q:)' on 'P.Protocol' with non-conforming result type 'Int'}}
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
  static func method() -> T { fatalError() } // expected-note {{'method()' declared here}}
}

_ = Z.prop
// expected-error@-1 {{member 'prop' cannot be used on value of protocol type 'Z.Protocol'; use a generic constraint instead}}
// expected-error@-2 {{protocol 'Z' can only be used as a generic constraint because it has Self or associated type requirements}}

_ = Z.method()
// expected-error@-1 {{cannot reference static method 'method()' on 'Z.Protocol' with non-conforming result type 'Z.T'}}
// expected-error@-2 {{member 'method' cannot be used on value of protocol type 'Z.Protocol'; use a generic constraint instead}}
// expected-error@-3 {{protocol 'Z' can only be used as a generic constraint because it has Self or associated type requirements}}
