// RUN: %target-typecheck-verify-swift -target %target-swift-5.1-abi-triple

/////
// This test is focused on checking protocol conformance and constraint checking

protocol None {
  associatedtype V
  // expected-note@+2 {{protocol requires property 'someProp' with type 'CA_N.V' (aka 'Int')}}
  // expected-note@+1 2 {{protocol requires property 'someProp' with type 'Self.V'}}
  var someProp : V { get }
}

protocol T {
  associatedtype V
  // expected-note@+2 {{protocol requires property 'someProp' with type 'CAT_T.V' (aka 'Int')}}
  // expected-note@+1 {{protocol requires property 'someProp' with type 'Self.V'}}
  var someProp : V { get throws }
}

protocol A {
  associatedtype V
  // expected-note@+1 2 {{protocol requires property 'someProp' with type 'Self.V'}}
  var someProp : V { get async }
}

protocol AT {
  associatedtype V
  var someProp : V { get async throws }
}

/////
// exercise the space of conformances to a property with effects
// The naming scheme here is:
//     CN_K
// where
//     C = "conformer"
//     N = candidate witness's effect abbreviation
//     K = protocol requirement's effect abbreviation
//     "effect abbreviation" = [
//        N -> <none>, T -> throws, A -> async, AT -> async throws
//     ]

// First group here also demonstrates that stored or mutable properties can
// witness a protocol requirement that allows for effects.
class CN_N : None { typealias V = Int; var someProp : Int { get {0} } }
class CN_T : T    { typealias V = Int; var someProp : Int = 0 }
class CN_T_v2 : T { typealias V = Int; var someProp : Int { get {0} } }
class CN_A : A    { typealias V = Int; var someProp : Int { get {0} set {} } }
class CN_AT : AT   { typealias V = Int; var someProp : Int { _read {yield 0} } }
// ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

// Remaining conformances test the limit check for kinds of effects allowed

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 3 {{candidate throws, but protocol does not allow it}}
// expected-error@+1 {{type 'CT_N' does not conform to protocol 'None'}}
class CT_N : None { typealias V = Int; var someProp : Int { get throws {0} } }
class CT_T : T    { typealias V = Int; var someProp : Int { get throws {0} } }

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 {{candidate throws, but protocol does not allow it}}
// expected-error@+1{{type 'CT_A' does not conform to protocol 'A'}}
class CT_A : A    { typealias V = Int; var someProp : Int { get throws {0} } }
class CT_AT : AT   { typealias V = Int; var someProp : Int { get throws {0} } }
// ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

// expected-note@+3 {{add stubs for conformance}} 
// expected-note@+2 3 {{candidate is 'async', but protocol requirement is not}}
// expected-error@+1 {{type 'CA_N' does not conform to protocol 'None'}}
struct CA_N : None { typealias V = Int; var someProp : Int { get async {0} } }

// expected-note@+3 {{add stubs for conformance}}
// expected-note@+2 {{candidate is 'async', but protocol requirement is not}}
// expected-error@+1 {{type 'CA_T' does not conform to protocol 'T'}}
class CA_T : T    { typealias V = Int; var someProp : Int { get async {0} } }

struct CA_A : A  { typealias V = Int; var someProp : Int { get async {0} } }
enum CA_AT : AT   { typealias V = Int; var someProp : Int { get async {0} } }
// ------ ------ ------ ------ ------ ------ ------ ------ ------ ------

// I put these on separate lines to ensure diagnostics point to the right thing.

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CAT_N' does not conform to protocol 'None'}}
class CAT_N : None { typealias V = Int;
  // expected-note@+2 {{candidate throws, but protocol does not allow it}}
  // expected-note@+1 2 {{candidate is 'async', but protocol requirement is not}}
  var someProp : Int { get async throws {0} }
}
// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CAT_T' does not conform to protocol 'T'}}
enum CAT_T : T    { typealias V = Int;
  // expected-note@+2 {{candidate throws, but protocol does not allow it}}
  // expected-note@+1 2 {{candidate is 'async', but protocol requirement is not}}
  var someProp : Int { get async throws {0} }
}
// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CAT_A' does not conform to protocol 'A'}}
class CAT_A : A    { typealias V = Int;
  // expected-note@+1 {{candidate throws, but protocol does not allow it}}
  var someProp : Int { get async throws {0} }
}
class CAT_AT : AT   { typealias V = Int;
  var someProp : Int { get async throws {0} }
}

// because the protocols above are generic over a type (i.e.,
// have an associatedtype), we can't express the constraints
// below with just 'as'.

func asNone<U : None>(u : U) async throws {
  _ = u.someProp
}

func asAsync<U : A>(u : U) async {
  // expected-error@+1 {{expression is 'async' but is not marked with 'await'}}{{7-7=await }}
  _ = u.someProp // expected-note@:7{{property access is 'async'}}

  _ = await u.someProp
}

func asThrows<U : T>(u : U) throws {
// expected-note@+3 {{did you mean to handle error as optional value?}}
// expected-note@+2 {{did you mean to disable error propagation?}}
// expected-note@+1 {{did you mean to use 'try'?}}
  _ = u.someProp // expected-error {{property access can throw but is not marked with 'try'}}

  _ = try u.someProp
}

func asAsyncThrows<U : AT>(u : U) async throws {
  // expected-note@+6 {{did you mean to handle error as optional value?}}
  // expected-note@+5 {{did you mean to disable error propagation?}}
  // expected-note@+4 {{did you mean to use 'try'?}}
  // expected-error@+3 {{expression is 'async' but is not marked with 'await'}}{{9-9=await }}
  // expected-note@+2 {{property access is 'async'}}
  // expected-error@+1 {{property access can throw but is not marked with 'try'}}
    _ = u.someProp

    _ = try await u.someProp
}


////////
// specific conformance coverage for subscripts

protocol NoneSub {
  // expected-note@+1 3 {{protocol requires subscript with type '(Int) -> Bool'}}
  subscript(_ i : Int) -> Bool { get }
}

protocol AsyncSub {
  // expected-note@+1 2 {{protocol requires subscript with type '(Int) -> Bool'}}
  subscript(_ i : Int) -> Bool { get async }
}

protocol ThrowsSub {
  // expected-note@+1 2 {{protocol requires subscript with type '(Int) -> Bool'}}
  subscript(_ i : Int) -> Bool { get throws }
}

protocol AsyncThrowsSub {
  subscript(_ i : Int) -> Bool { get async throws }
}

// "S" stands for "subscript", but otherwise the convention above applies:
// Sx_y ==> witness x trying to conform to y. A = async, T = throws, AT = async throws
// I peppered some enums and structs in there for flavor.

struct SN_N : NoneSub
  { subscript(_ i : Int) -> Bool { get { true } }}
class SA_N : NoneSub // expected-error{{type 'SA_N' does not conform to protocol 'NoneSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get async { true } }} // expected-note{{candidate is 'async', but protocol requirement is not}}
class ST_N : NoneSub // expected-error{{type 'ST_N' does not conform to protocol 'NoneSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get throws { true } }} // expected-note{{candidate throws, but protocol does not allow it}}
class SAT_N : NoneSub // expected-error{{type 'SAT_N' does not conform to protocol 'NoneSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get async throws { true } }} // expected-note{{candidate is 'async', but protocol requirement is not}}

class SN_A : AsyncSub
  { subscript(_ i : Int) -> Bool { get { true } }}
struct SA_A : AsyncSub
  { subscript(_ i : Int) -> Bool { get async { true } }}
enum ST_A : AsyncSub // expected-error{{type 'ST_A' does not conform to protocol 'AsyncSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get throws { true } }} // expected-note{{candidate throws, but protocol does not allow it}}
class SAT_A : AsyncSub // expected-error{{type 'SAT_A' does not conform to protocol 'AsyncSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get async throws { true } }} // expected-note{{candidate throws, but protocol does not allow it}}

class SN_T : ThrowsSub
  { subscript(_ i : Int) -> Bool { get { true } }}
class SA_T : ThrowsSub // expected-error{{type 'SA_T' does not conform to protocol 'ThrowsSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get async { true } }} // expected-note{{candidate is 'async', but protocol requirement is not}}
struct ST_T : ThrowsSub
  { subscript(_ i : Int) -> Bool { get throws { true } }}
struct SAT_T : ThrowsSub // expected-error{{type 'SAT_T' does not conform to protocol 'ThrowsSub'}} expected-note {{add stubs for conformance}}
  { subscript(_ i : Int) -> Bool { get async throws { true } }} // expected-note{{candidate is 'async', but protocol requirement is not}}

class SN_AT : AsyncThrowsSub
  { subscript(_ i : Int) -> Bool { get { true } }}
enum SA_AT : AsyncThrowsSub
  { subscript(_ i : Int) -> Bool { get async { true } }}
class ST_AT : AsyncThrowsSub
  { subscript(_ i : Int) -> Bool { get throws { true } }}
struct SAT_AT : AsyncThrowsSub
  { subscript(_ i : Int) -> Bool { get async throws { true } }}


////////
// protocol composition & inheritance

func composed1<U : A & T >(u : U) async throws {
  // expected-note@+4 {{did you mean to handle error as optional value?}}
  // expected-note@+3 {{did you mean to disable error propagation?}}
  // expected-note@+2 {{did you mean to use 'try'?}}
  // expected-error@+1 {{property access can throw but is not marked with 'try'}}
  _ = u.someProp

  // FIXME: this ^ should raise property access is 'async' but is not marked with 'await'

  _ = try u.someProp
}

func composed2<U : None & A >(u : U) async {
  _ = u.someProp
  // FIXME: this ^ should raise "property access is 'async' but is not marked with 'await'""

  _ = await u.someProp // expected-warning {{no 'async' operations occur within 'await' expression}}
}

func composed3<U : T & None >(u : U) throws {
  // expected-note@+3 {{did you mean to handle error as optional value?}}
  // expected-note@+2 {{did you mean to disable error propagation?}}
  // expected-note@+1 {{did you mean to use 'try'?}}
  _ = u.someProp // expected-error {{property access can throw but is not marked with 'try'}}

  _ = try u.someProp
}

func composed4<U : T & None >(u : U) {
  _ = u.someProp // expected-error {{property access can throw, but it is not marked with 'try' and the error is not handled}}

  _ = try! u.someProp
}


/////////////////
// redefining the protocols to make sure the fix-its are matched

protocol NoEffects {
// expected-note@+1 2 {{protocol requires property 'someProp' with type 'Int'}}
  var someProp : Int { get }
}

protocol Throws {
  // expected-note@+1 2 {{protocol requires property 'someProp' with type 'Int'}}
  var someProp : Int { get throws }
}

protocol Async {
  // expected-note@+1 3 {{protocol requires property 'someProp' with type 'Int'}}
  var someProp : Int { get async }
}

protocol AsyncThrowsByInheritance : Async, Throws {}
protocol AsyncByInheritance : Async, NoEffects {}
protocol ThrowsByInheritance : Throws, NoEffects {}

extension CN_N : AsyncByInheritance, ThrowsByInheritance, AsyncThrowsByInheritance {}
extension CN_T : AsyncByInheritance, ThrowsByInheritance, AsyncThrowsByInheritance {}
extension CN_A : AsyncByInheritance, ThrowsByInheritance, AsyncThrowsByInheritance {}
extension CN_AT : AsyncByInheritance, ThrowsByInheritance, AsyncThrowsByInheritance {}

// ----- -----

extension CT_N : Throws {}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CT_N' does not conform to protocol 'NoEffects'}}
extension CT_N : ThrowsByInheritance {}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CT_N' does not conform to protocol 'Async'}}
extension CT_N : AsyncByInheritance {}

// ----- -----

extension CA_N : Async {}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CA_N' does not conform to protocol 'NoEffects'}}
extension CA_N : AsyncByInheritance {}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CA_N' does not conform to protocol 'Throws'}}
extension CA_N : ThrowsByInheritance {}

// ----- -----

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CAT_N' does not conform to protocol 'Async'}}
extension CAT_N : Async {}

// expected-note@+2 {{add stubs for conformance}}
// expected-error@+1 {{type 'CAT_N' does not conform to protocol 'Throws'}}
extension CAT_N : Throws {}

// expected-note@+3 {{add stubs for conformance}}
// expected-error@+2 {{type 'CAT_T' does not conform to protocol 'Async'}}
// expected-error@+1 {{type 'CAT_T' does not conform to protocol 'Throws'}}
extension CAT_T : AsyncThrowsByInheritance {}


struct S : Async, Throws {
  var someProp : Int { 3 }
}

func play(s : S) async throws {
  _ = s.someProp
  _ = await (s as Async).someProp
  _ = try (s as Throws).someProp
}

//////////
/// Check protocol overrides. Cannot override with more effects.

protocol HammeredDulcimer {
  subscript(_ note : Int) -> Int { get }
  var bridges : Int { get async throws }
}

protocol Santur : HammeredDulcimer {
  override subscript(_ note : Int) -> Int { get throws } // expected-error{{cannot override non-throwing subscript with throwing subscript}}
  override var bridges : Int { get throws }
}

protocol Santoor : Santur {
  override var bridges : Int { get async throws } // expected-error{{cannot override non-async property with async property}}
}

protocol Yangqin : HammeredDulcimer {
  override var bridges : Int { get async throws } // same effects are OK
}

protocol Hackbrett : HammeredDulcimer {
 override var bridges : Int { get } // no effects are OK
 override subscript(_ note : Int) -> Int { get async throws } // expected-error {{cannot override non-async subscript with async subscript}}
}
