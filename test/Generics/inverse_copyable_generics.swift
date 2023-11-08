// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

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

// FIXME: we should expect a warning/error that NC actually did require Copyable
// despite the annotation due to the implied requirement!
func checkAliases<C, NC: ~Copyable>(_ a: AlwaysCopyable<C>, _ b: AlwaysCopyable<NC>) {
  checkCopyable(a)
  checkCopyable(b)
}

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
    // expected-error@-1 {{stored property 'storage' of 'Copyable'-conforming generic struct 'StructContainment' has noncopyable type 'Maybe<T>'}}
}

enum EnumContainment<T: ~Copyable> : Copyable {
    // expected-note@-1 {{'T' has '~Copyable' constraint preventing implicit 'Copyable' conformance}}

    case some(T) // expected-error {{associated value 'some' of 'Copyable'-conforming generic enum 'EnumContainment' has noncopyable type 'T'}}
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
  var y: NC // expected-error {{stored property 'y' of 'Copyable'-conforming generic struct 'ConditionalContainment' has noncopyable type 'NC'}}
}

func chk(_ T: RequireCopyable<ConditionalContainment<Int>>) {}


/// ----------------

struct AlwaysCopyableDeinit<T: ~Copyable> : Copyable {
  let nc: NC // expected-error {{stored property 'nc' of 'Copyable'-conforming generic struct 'AlwaysCopyableDeinit' has noncopyable type 'NC'}}
  deinit {} // expected-error {{deinitializer cannot be declared in generic struct 'AlwaysCopyableDeinit' that conforms to 'Copyable'}}
}

struct SometimesCopyableDeinit<T: ~Copyable> : ~Copyable {
  deinit {} // expected-error {{deinitializer cannot be declared in generic struct 'SometimesCopyableDeinit' that conforms to 'Copyable'}}
}
extension SometimesCopyableDeinit: Copyable where T: Copyable {}

struct NeverCopyableDeinit<T: ~Copyable>: ~Copyable {
  deinit {}
}

/// ---------------

// expected-note@+2 {{consider adding '~Copyable' to generic enum 'Maybe'}}
// expected-note@+1 2{{generic enum 'Maybe' has '~Copyable' constraint on a generic parameter, making its 'Copyable' conformance conditional}}
enum Maybe<Wrapped: ~Copyable> {
  case just(Wrapped)
  case none

  deinit {} // expected-error {{deinitializer cannot be declared in generic enum 'Maybe' that conforms to 'Copyable'}}
  // expected-error@-1 {{deinitializers are not yet supported on noncopyable enums}}
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
// expected-note@-1 3{{struct 'NC' has '~Copyable' constraint preventing 'Copyable' conformance}}
  deinit {}
}

typealias ok1 = RequireCopyable<Int>
typealias ok2 = RequireCopyable<Maybe<Int>>

typealias err1 = RequireCopyable<Maybe<NC>>
// expected-error@-1{{type 'NC' does not conform to protocol 'Copyable'}}
// expected-error@-2{{'RequireCopyable' requires that 'NC' conform to 'Copyable'}}

typealias err2 = RequireCopyable<NC>
// expected-error@-1{{type 'NC' does not conform to protocol 'Copyable'}}

// plain extension doesn't treat Self as Copyable
extension Maybe {
  func check1(_ t: RequireCopyable<Self>) {}
  // expected-error@-1 {{type 'Wrapped' does not conform to protocol 'Copyable'}}
  // expected-error@-2 {{'RequireCopyable' requires that 'Wrapped' conform to 'Copyable'}}
}

extension Maybe where Self: Copyable {
  func check2(_ t: RequireCopyable<Self>) {}
}

// expected-note@+2 {{generic struct 'CornerCase' has '~Copyable' constraint on a generic parameter, making its 'Copyable' conformance conditional}}
// expected-note@+1 {{consider adding '~Copyable' to generic struct 'CornerCase'}}{{33-33=: ~Copyable}}
struct CornerCase<T: ~Copyable> {
  let t: T
  let nc: NC // expected-error {{stored property 'nc' of 'Copyable'-conforming generic struct 'CornerCase' has noncopyable type 'NC'}}
}

func chk(_ t: CornerCase<NC>) {}
// expected-error@-1 {{parameter of noncopyable type 'CornerCase<NC>' must specify ownership}}
// expected-note@-2 3{{add}}


/// MARK: tests that we diagnose ~Copyable that became invalid because it's required to be copyable

protocol NeedsCopyable {}

struct Silly: ~Copyable, Copyable {} // expected-error {{struct 'Silly' required to be 'Copyable' but is marked with '~Copyable'}}
enum Sally: Copyable, ~Copyable, NeedsCopyable {} // expected-error {{enum 'Sally' required to be 'Copyable' but is marked with '~Copyable'}}
class NiceTry: ~Copyable, Copyable {} // expected-error {{classes cannot be noncopyable}}

struct OopsConformance1: ~Copyable, NeedsCopyable {}
// expected-error@-1 {{type 'OopsConformance1' does not conform to protocol 'NeedsCopyable'}}
// expected-error@-2 {{type 'OopsConformance1' does not conform to protocol 'Copyable'}}

protocol Nothing: ~Copyable, NeedsCopyable {}
// expected-warning@-1 {{protocol 'Nothing' should be declared to refine 'Copyable' due to a same-type constraint on 'Self'}}


struct Extendo: ~Copyable {}
extension Extendo: Copyable, ~Copyable {} // expected-error {{cannot apply inverse '~Copyable' to extension}}

enum EnumExtendo {}
extension EnumExtendo: ~Copyable {} // expected-error {{cannot apply inverse '~Copyable' to extension}}

extension NeedsCopyable where Self: ~Copyable {}
// expected-error@-1 {{cannot add inverse constraint 'Self: ~Copyable' on generic parameter 'Self' defined in outer scope}}
