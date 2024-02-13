// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics -enable-experimental-feature NonescapableTypes

// REQUIRES: noncopyable_generics

// Check support for explicit conditional conformance
public struct ExplicitCond<T: ~Copyable>: ~Copyable {}
extension ExplicitCond: Copyable where T: Copyable {}
// expected-note@-1 {{requirement from conditional conformance}}
// expected-note@-2 {{requirement from conditional conformance of 'ExplicitCondAlias<NC>' (aka 'ExplicitCond<NC>') to 'Copyable'}}

public typealias ExplicitCondAlias<T> = ExplicitCond<T> where T: ~Copyable
public typealias AlwaysCopyable<T> = ExplicitCond<T>

func checkCopyable<T>(_ t: T) {} // expected-note {{generic parameter 'T' has an implicit Copyable requirement}}

func test<C, NC: ~Copyable>(
  _ a1: ExplicitCond<C>, _ b1: borrowing ExplicitCond<NC>,
  _ a2: ExplicitCondAlias<C>, _ b2: borrowing ExplicitCondAlias<NC>
  ) {
  checkCopyable(a1)
  checkCopyable(b1) // expected-error {{global function 'checkCopyable' requires that 'NC' conform to 'Copyable'}}
  checkCopyable(a2)
  checkCopyable(b2) // expected-error {{global function 'checkCopyable' requires that 'NC' conform to 'Copyable'}}
}

func checkAliases<C, NC: ~Copyable>(_ a: AlwaysCopyable<C>, _ b: AlwaysCopyable<NC>) {
// expected-error@-1 {{'NC' required to be 'Copyable' but is marked with '~Copyable'}}
  checkCopyable(a)
  checkCopyable(b)
}

struct TryInferCopyable: ~Copyable, NeedsCopyable {}
// expected-error@-1 {{type 'TryInferCopyable' does not conform to protocol 'NeedsCopyable'}}
// expected-error@-2 {{type 'TryInferCopyable' does not conform to protocol 'Copyable'}}

protocol Removed: ~Copyable {
  func requiresCopyableSelf(_ t: AlwaysCopyable<Self>)
  // expected-error@-1 {{type 'Self' does not conform to protocol 'Copyable'}}
}
protocol Plain<T> {
  associatedtype T: ~Copyable
  func requiresCopyableSelf(_ t: AlwaysCopyable<Self>)
  func requiresCopyableT(_ t: AlwaysCopyable<T>)
  // expected-error@-1 {{type 'Self.T' does not conform to protocol 'Copyable'}}
}

protocol RemovedAgain where Self: ~Copyable {
    func requiresCopyableSelf(_ t: AlwaysCopyable<Self>) // expected-error {{type 'Self' does not conform to protocol 'Copyable'}}
}

struct StructContainment<T: ~Copyable> : Copyable {
    var storage: Maybe<T>
    // expected-error@-1 {{stored property 'storage' of 'Copyable'-conforming generic struct 'StructContainment' has non-Copyable type 'Maybe<T>'}}
}

enum EnumContainment<T: ~Copyable> : Copyable {
    // expected-note@-1 {{'T' has '~Copyable' constraint preventing implicit 'Copyable' conformance}}

    case some(T) // expected-error {{associated value 'some' of 'Copyable'-conforming generic enum 'EnumContainment' has non-Copyable type 'T'}}
    case other(Int)
    case none
}

class ClassContainment<T: ~Copyable> {
    var storage: T
    init(_ t: consuming T) {
        storage = t
        checkCopyable(t) // expected-error {{noncopyable type 'T' cannot be substituted for copyable generic parameter 'T' in 'checkCopyable'}}
    }

    deinit {}
}

// expected-note@+2 {{generic struct 'ConditionalContainment' has '~Copyable' constraint on a generic parameter, making its 'Copyable' conformance conditional}}
// expected-note@+1 {{consider adding '~Copyable' to generic struct 'ConditionalContainment'}}{{45-45=: ~Copyable}}
struct ConditionalContainment<T: ~Copyable> {
  var x: T
  var y: NC // expected-error {{stored property 'y' of 'Copyable'-conforming generic struct 'ConditionalContainment' has non-Copyable type 'NC'}}
}

func chk(_ T: RequireCopyable<ConditionalContainment<Int>>) {}

// expected-note@+2 3{{add}}
// expected-error@+1 {{parameter of noncopyable type 'some Escapable & ~Copyable' must specify ownership}}
func dogDays(_ t: some Escapable & ~Copyable) {}

/// ----------------

struct AlwaysCopyableDeinit<T: ~Copyable> : Copyable {
  let nc: NC // expected-error {{stored property 'nc' of 'Copyable'-conforming generic struct 'AlwaysCopyableDeinit' has non-Copyable type 'NC'}}
  deinit {} // expected-error {{deinitializer cannot be declared in generic struct 'AlwaysCopyableDeinit' that conforms to 'Copyable'}}
}

struct SometimesCopyableDeinit<T: ~Copyable> : ~Copyable {
  deinit {} // expected-error {{deinitializer cannot be declared in generic struct 'SometimesCopyableDeinit' that conforms to 'Copyable'}}
}
extension SometimesCopyableDeinit: Copyable where T: Copyable {}

struct NeverCopyableDeinit<T: ~Copyable>: ~Copyable {
  deinit {}
}

protocol Test: ~Copyable {
  init?() // expected-error {{noncopyable types cannot have failable initializers yet}}
}

struct NoncopyableAndSendable: ~Copyable, Sendable {}

/// ---------------

// expected-note@+2 {{consider adding '~Copyable' to generic enum 'Maybe'}}
// expected-note@+1 2{{generic enum 'Maybe' has '~Copyable' constraint on a generic parameter, making its 'Copyable' conformance conditional}}
enum Maybe<Wrapped: ~Copyable> {
  case just(Wrapped)
  case none

  deinit {} // expected-error {{deinitializer cannot be declared in generic enum 'Maybe' that conforms to 'Copyable'}}
}

// expected-note@+4{{requirement specified as 'NC' : 'Copyable'}}
// expected-note@+3{{requirement from conditional conformance of 'Maybe<NC>' to 'Copyable'}}
// expected-note@+2{{requirement specified as 'Wrapped' : 'Copyable'}}
// expected-note@+1{{requirement from conditional conformance of 'Maybe<Wrapped>' to 'Copyable'}}
struct RequireCopyable<T> {
  // expected-note@-1 {{consider adding '~Copyable' to generic struct 'RequireCopyable'}}{{27-27=: ~Copyable}}
  deinit {} // expected-error {{deinitializer cannot be declared in generic struct 'RequireCopyable' that conforms to 'Copyable'}}
}

struct NC: ~Copyable {
// expected-note@-1 4{{struct 'NC' has '~Copyable' constraint preventing 'Copyable' conformance}}
  deinit {}
}

typealias ok1 = RequireCopyable<Int>
typealias ok2 = RequireCopyable<Maybe<Int>>

typealias err1 = RequireCopyable<Maybe<NC>>
// expected-error@-1{{type 'NC' does not conform to protocol 'Copyable'}}
// expected-error@-2{{'RequireCopyable' requires that 'NC' conform to 'Copyable'}}

typealias err2 = RequireCopyable<NC>
// expected-error@-1{{type 'NC' does not conform to protocol 'Copyable'}}

extension Maybe where Wrapped: ~Copyable {
  func check1(_ t: RequireCopyable<Self>) {}
  // expected-error@-1 {{type 'Wrapped' does not conform to protocol 'Copyable'}}
  // expected-error@-2 {{'RequireCopyable' requires that 'Wrapped' conform to 'Copyable'}}
}

extension Maybe {
  func check2(_ t: RequireCopyable<Self>) {}
}

// expected-note@+2 {{generic struct 'CornerCase' has '~Copyable' constraint on a generic parameter, making its 'Copyable' conformance conditional}}
// expected-note@+1 {{consider adding '~Copyable' to generic struct 'CornerCase'}}{{33-33=: ~Copyable}}
struct CornerCase<T: ~Copyable> {
  let t: T
  let nc: NC // expected-error {{stored property 'nc' of 'Copyable'-conforming generic struct 'CornerCase' has non-Copyable type 'NC'}}
}

func chk(_ t: CornerCase<NC>) {}
// expected-error@-1 {{parameter of noncopyable type 'CornerCase<NC>' must specify ownership}}
// expected-note@-2 3{{add}}


/// MARK: tests that we diagnose ~Copyable that became invalid because it's required to be copyable

protocol NeedsCopyable {}

struct Silly: ~Copyable, Copyable {} // expected-error {{struct 'Silly' required to be 'Copyable' but is marked with '~Copyable'}}
enum Sally: Copyable, ~Copyable, NeedsCopyable {} // expected-error {{enum 'Sally' required to be 'Copyable' but is marked with '~Copyable'}}

class NiceTry: ~Copyable, Copyable {} // expected-error {{classes cannot be '~Copyable'}}
                                      // expected-error@-1 {{class 'NiceTry' required to be 'Copyable' but is marked with '~Copyable}}

@_moveOnly class NiceTry2: Copyable {} // expected-error {{'@_moveOnly' attribute is only valid on structs or enums}}
                                       // expected-error@-1 {{class 'NiceTry2' required to be 'Copyable' but is marked with '~Copyable'}}

struct OopsConformance1: ~Copyable, NeedsCopyable {}
// expected-error@-1 {{type 'OopsConformance1' does not conform to protocol 'NeedsCopyable'}}
// expected-error@-2 {{type 'OopsConformance1' does not conform to protocol 'Copyable'}}


struct Extendo: ~Copyable {}
extension Extendo: Copyable, ~Copyable {} // expected-error {{cannot apply inverse '~Copyable' to extension}}

enum EnumExtendo {}
extension EnumExtendo: ~Copyable {} // expected-error {{cannot apply inverse '~Copyable' to extension}}

extension NeedsCopyable where Self: ~Copyable {}
// expected-error@-1 {{'Self' required to be 'Copyable' but is marked with '~Copyable'}}

protocol NoCopyP: ~Copyable {}

func needsCopyable<T>(_ t: T) {} // expected-note 2{{generic parameter 'T' has an implicit Copyable requirement}}
func noCopyable(_ t: borrowing some ~Copyable) {}
func noCopyableAndP(_ t: borrowing some NoCopyP & ~Copyable) {}

func openingExistentials(_ a: borrowing any NoCopyP & ~Copyable,
                         _ b: any NoCopyP,
                         _ nc: borrowing any ~Copyable) {
  needsCopyable(a) // expected-error {{noncopyable type 'any NoCopyP & ~Copyable' cannot be substituted for copyable generic parameter 'T' in 'needsCopyable'}}
  noCopyable(a)
  noCopyableAndP(a)

  needsCopyable(b)
  noCopyable(b)
  noCopyableAndP(b)

  needsCopyable(nc) // expected-error {{noncopyable type 'any ~Copyable' cannot be substituted for copyable generic parameter 'T' in 'needsCopyable'}}
  noCopyable(nc)
  noCopyableAndP(nc) // expected-error {{global function 'noCopyableAndP' requires that 'some NoCopyP & ~Copyable' conform to 'NoCopyP'}}
}

func project<CurValue>(_ base: CurValue) { }
func testSpecial(_ a: Any) {
  _openExistential(a, do: project)
}

/// MARK: non-Escapable types

func requireEscape<T: ~Copyable>(_ t: borrowing T) {} // expected-note {{generic parameters are always considered '@escaping'}}
// expected-note@-1 {{where 'T' = 'MutableBuggerView<NC>'}}
// expected-note@-2 {{where 'T' = 'BuggerView<NC>'}}
// expected-note@-3 {{where 'T' = 'MutableBuggerView<Int>'}}
// expected-note@-4 {{where 'T' = 'BuggerView<Int>'}}

func genericNoEscape<T: ~Escapable>(_ t: borrowing T) {} // expected-note {{generic parameters are always considered '@escaping'}}
// expected-note@-1 2{{generic parameter 'T' has an implicit Copyable requirement}}

func genericNoEscapeOrCopy<T: ~Escapable & ~Copyable>(_ t: borrowing T) {}

func checkFunctions(_ f: @autoclosure () -> Int) {
  requireEscape(f) // expected-error {{converting non-escaping parameter 'f' to generic parameter 'T' may allow it to escape}}

  // FIXME: rdar://119410346 (nonescaping function is not permitted as arg to ~Escapable generic function)
  genericNoEscape(f) // expected-error {{converting non-escaping parameter 'f' to generic parameter 'T' may allow it to escape}}
}

struct BuggerView<T: ~Copyable>: ~Escapable, Copyable {}

struct MutableBuggerView<T: ~Copyable>: ~Copyable, ~Escapable {}

func checkNominals(_ mutRef: inout MutableBuggerView<NC>,
                   _ ref: BuggerView<NC>,
                   _ intMutRef: borrowing MutableBuggerView<Int>,
                   _ intRef: BuggerView<Int>) {

  genericNoEscape(mutRef) // expected-error {{noncopyable type 'MutableBuggerView<NC>' cannot be substituted for copyable generic parameter 'T' in 'genericNoEscape'}}
  genericNoEscape(ref)
  genericNoEscape(intMutRef) // expected-error {{noncopyable type 'MutableBuggerView<Int>' cannot be substituted for copyable generic parameter 'T' in 'genericNoEscape'}}
  genericNoEscape(intRef)

  genericNoEscapeOrCopy(mutRef)
  genericNoEscapeOrCopy(ref)
  genericNoEscapeOrCopy(intMutRef)
  genericNoEscapeOrCopy(intRef)

  requireEscape(mutRef) // expected-error {{global function 'requireEscape' requires that 'MutableBuggerView<NC>' conform to 'Escapable'}}
  requireEscape(ref) // expected-error {{global function 'requireEscape' requires that 'BuggerView<NC>' conform to 'Escapable'}}
  requireEscape(intMutRef) // expected-error {{global function 'requireEscape' requires that 'MutableBuggerView<Int>' conform to 'Escapable'}}
  requireEscape(intRef) // expected-error {{global function 'requireEscape' requires that 'BuggerView<Int>' conform to 'Escapable'}}
}

struct NonescapingType: ~Escapable {}

struct Wraps: ~Escapable {
  let x: MaybeEscapes<NonescapingType>
}

struct NonescapeDoesNotAllowNoncopyable: ~Escapable { // expected-note {{consider adding '~Copyable' to struct 'NonescapeDoesNotAllowNoncopyable'}}
  let x: NC // expected-error {{stored property 'x' of 'Copyable'-conforming struct 'NonescapeDoesNotAllowNoncopyable' has non-Copyable type 'NC'}}
}

enum MaybeEscapes<T: ~Escapable> { // expected-note {{generic enum 'MaybeEscapes' has '~Escapable' constraint on a generic parameter, making its 'Escapable' conformance conditional}}
  case just(T)
  case none
}

struct Escapes { // expected-note {{consider adding '~Escapable' to struct 'Escapes'}}
  let t: MaybeEscapes<NonescapingType> // expected-error {{stored property 't' of 'Escapable'-conforming struct 'Escapes' has non-Escapable type 'MaybeEscapes<NonescapingType>'}}
}

enum Boring {
  case thing(MaybeEscapes<Int>)
}

struct NonEscapingHasNoDeinit: ~Escapable { // expected-note {{consider adding '~Copyable' to struct 'NonEscapingHasNoDeinit'}}
  deinit {} // expected-error {{deinitializer cannot be declared in struct 'NonEscapingHasNoDeinit' that conforms to 'Copyable'}}
}

/// MARK: requirement conflict tests

func conflict1<T>(_ t: T) where T: NeedsCopyable, T: ~Copyable {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

func conflict2<T: ~Copyable>(_ t: AlwaysCopyable<T>) {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

func conflict3a<T: NeedsCopyable & ~Copyable>(_ t: T) {}
// expected-error@-1 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}

func conflict3b<T>(_ t: T) where T: NeedsCopyable, T: ~Copyable {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

func conflict4a(_ t: some NeedsCopyable & ~Copyable) {}
// expected-error@-1 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}

protocol Conflict5: ~Copyable {
  borrowing func whatever() -> AlwaysCopyable<Self> // expected-error {{type 'Self' does not conform to protocol 'Copyable'}}
}

// expected-warning@+1 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}
func conflict6<T: ~Copyable, U>(_ t: T, _ u: U) // expected-error {{'T' required to be 'Copyable' but is marked with '~Copyable'}}
 where U : NeedsCopyable, T == U {}

protocol Conflict7 {
  associatedtype Element
}

func conflict7<T, U>(_ t: T, _ u: U)
  where
    U: ~Copyable,  // expected-error {{'U' required to be 'Copyable' but is marked with '~Copyable'}}
    T: Conflict7,
    U == T.Element
  {}

protocol Conflict8: ~Copyable, NeedsCopyable {}
// expected-error@-1 {{'Self' required to be 'Copyable' but is marked with '~Copyable'}}

struct Conflict9<T: NeedsCopyable> {}
func conflict9<U: ~Copyable>(_ u: Conflict9<U>) {}
// expected-error@-1 {{'U' required to be 'Copyable' but is marked with '~Copyable'}}

func conflict10<T>(_ t: T, _ u: some ~Copyable & Copyable)
// expected-error@-1 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
  where T: Copyable,
        T: ~Copyable {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

// FIXME: this is bogus (rdar://119345796)
protocol Conflict11: ~Copyable, Copyable {}

struct Conflict12: ~Copyable, Copyable {}
// expected-error@-1 {{struct 'Conflict12' required to be 'Copyable' but is marked with '~Copyable'}}

// FIXME: this is bogus (rdar://119346022)
protocol Conflict13 {
  associatedtype A
  associatedtype B: ~Copyable
}
func conflict13<T>(_ t: T)
  where T: Conflict13,
        T.A == T.B
        {}

// expected-warning@+1 {{same-type requirement makes generic parameters 'U' and 'T' equivalent}}
func conflict14<T, U>(_ t: T, _ u: U)
  where T: ~Copyable, // expected-error {{'T' required to be 'Copyable' but is marked with '~Copyable'}}
        U: ~Escapable, // expected-error {{'U' required to be 'Escapable' but is marked with '~Escapable'}}
        T == U {}

protocol Conflict15 {
  associatedtype HasE: ~Copyable
  associatedtype HasC: ~Escapable
}
func conflict15<T, C, E>(_ t: T, _ c: C, _ e: borrowing E)
  where
    T: Conflict15,
    E: ~Copyable,  // expected-error {{'E' required to be 'Copyable' but is marked with '~Copyable'}}
    E == T.HasC,
    C: ~Escapable,  // expected-error {{'C' required to be 'Escapable' but is marked with '~Escapable'}}
    C == T.HasE
  {}


// Class bounds and AnyObject

class Soup {}
func checkClassBound1<T>(_ t: T) where T: ~Copyable, T: Soup {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

// expected-note@+2 3{{add}}
// expected-error@+1 {{parameter of noncopyable type 'T' must specify ownership}}
func checkClassBound2<T>(_ t: T) where T: ~Escapable, T: AnyObject, T: ~Copyable {}
// expected-error@-1 {{'T' required to be 'Escapable' but is marked with '~Escapable'}}
// expected-error@-2 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}

func checkClassBound3<T>(_ t: T) where T: Soup & ~Copyable & ~Escapable {}
// expected-error@-1 {{composition involving class requirement 'Soup' cannot contain '~Copyable'}}

func checkClassBound4<T>(_ t: T) where T: Soup, T: ~Copyable & ~Escapable {}
// expected-error@-1 {{'T' required to be 'Copyable' but is marked with '~Copyable'}}
// expected-error@-2 {{'T' required to be 'Escapable' but is marked with '~Escapable'}}

public func checkAnyObjInv1<Result: AnyObject>(_ t: borrowing Result) where Result: ~Copyable {}
// expected-error@-1 {{'Result' required to be 'Copyable' but is marked with '~Copyable'}}

public func checkAnyObjInv2<Result: AnyObject>(_ t: borrowing Result) where Result: ~Escapable {}
// expected-error@-1 {{'Result' required to be 'Escapable' but is marked with '~Escapable'}}

public func checkAnyObject<Result>(_ t: Result) where Result: AnyObject {
    checkCopyable(t)
}

func checkExistentialAndClasses(
    _ a: any AnyObject & ~Copyable, // expected-error {{composition involving 'AnyObject' cannot contain '~Copyable'}}
    _ b: any Soup & Copyable & ~Escapable & ~Copyable,
    // expected-error@-1 {{composition involving class requirement 'Soup' cannot contain '~Copyable'}}
    // expected-error@-2 {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
    _ c: some (~Escapable & Removed) & Soup // expected-error {{composition cannot contain '~Escapable' when another member requires 'Escapable'}}
    ) {}

protocol HasNCBuddy: ~Copyable {
  associatedtype NCBuddy: HasNCBuddy, ~Copyable

  associatedtype Buddy: HasMember
}

protocol HasMember : HasNCBuddy {
  associatedtype Member: HasMember

  associatedtype NCMember: ~Copyable
}

func checkOwnership<T: HasMember>(_ t: T,
                                  _ l: T.NCBuddy.Buddy.Member.Buddy,
                                  _ m: T.Member.Member,
                                  _ n: T.Member.NCMember,
// expected-error@-1 {{parameter of noncopyable type 'T.Member.NCMember' must specify ownership}} // expected-note@-1 3{{add}}

                                  _ o: T.Member.NCBuddy.NCBuddy
// expected-error@-1 {{parameter of noncopyable type 'T.Member.NCBuddy.NCBuddy' must specify ownership}} // expected-note@-1 3{{add}}
) {}

// Covers an issue when building Combine from its interface.
public struct Record<Output> {
  public init(recording: Record<Output>) {}
}
protocol P {}
extension Record : Decodable where Output : P {}

// Expect that if Copyable is an inherited protocol, then conditional
// conformances carry that through, making the Blahaj conditionally Copyable.
struct Blahaj<Value>: ~Copyable {
  deinit {} // expected-error {{deinitializer cannot be declared in generic struct 'Blahaj' that conforms to 'Copyable'}}
}
extension Blahaj: Q where Value: Q {}
protocol Q: Copyable {}

// expected-note@+2 3{{add}}
// expected-error@+1 {{parameter of noncopyable type 'Blahaj<T>' must specify ownership}}
func testBlahaj<T, U: Q>(_ x: Blahaj<T>,
                         _ y: Blahaj<U>) {}

extension Int: NeedsCopyable {}

func checkExistentials() {
    let _: any ~Escapable & (NeedsCopyable & ~Copyable) // expected-error {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
    let _: any NeedsCopyable & ~Copyable = 1 // expected-error {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
    let _: any NeedsCopyable & ~Escapable = 1 // expected-error {{composition cannot contain '~Escapable' when another member requires 'Escapable'}}
    let _: any Copyable & ~Copyable = 1 // expected-error {{composition cannot contain '~Copyable' when another member requires 'Copyable'}}
    let _: any Escapable & ~Escapable = 1 // expected-error {{composition cannot contain '~Escapable' when another member requires 'Escapable'}}
}
