// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

public struct Cond<T: ~Copyable>: ~Copyable {}
extension Cond: Copyable where T: Copyable {} // expected-note {{requirement from conditional conformance}}

public typealias AlwaysCopyable<T> = Cond<T>
public typealias CondAlias<T> = Cond<T> where T: ~Copyable

func checkCopyable<T>(_ t: T) {} // expected-note {{generic parameter 'T' has an implicit Copyable requirement}}

func test<C, NC: ~Copyable>(_ a: Cond<C>, _ b: borrowing Cond<NC>) {
  checkCopyable(a)
  checkCopyable(b) // expected-error {{global function 'checkCopyable' requires that 'NC' conform to 'Copyable'}}
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
protocol Plain {
  func requiresCopyableSelf(_ t: AlwaysCopyable<Self>)
}

protocol RemovedAgain where Self: ~Copyable {
    func requiresCopyableSelf(_ t: AlwaysCopyable<Self>) // expected-error {{type 'Self' does not conform to protocol 'Copyable'}}
}

struct StructContainment<T: ~Copyable> {
    // expected-note@-1 {{consider removing '~Copyable' from generic parameter 'T' so it conforms to the 'Copyable' protocol}}
    // expected-note@-2 {{consider removing implicit 'Copyable' conformance from generic struct 'StructContainment'}}

    var storage: T
    // expected-error@-1 {{stored property 'storage' of 'Copyable'-conforming generic struct 'StructContainment' has noncopyable type 'T'}}
}

enum EnumContainment<T: ~Copyable> {
    // expected-note@-1{{consider removing '~Copyable' from generic parameter 'T' so it conforms to the 'Copyable' protocol}}
    // expected-note@-2{{consider removing implicit 'Copyable' conformance from generic enum 'EnumContainment'}}

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
}


/// ----------------

enum Maybe<Wrapped: ~Copyable>: ~Copyable {
  case just(Wrapped)
  case none
}
extension Maybe: Copyable where Wrapped: Copyable {}

struct RequireCopyable<T> {}
// expected-note@-1{{requirement specified as 'NC' : 'Copyable'}}
// expected-note@-2{{requirement from conditional conformance of 'Maybe<NC>' to 'Copyable'}}
// expected-note@-3{{requirement specified as 'Wrapped' : 'Copyable'}}
// expected-note@-4{{requirement from conditional conformance of 'Maybe<Wrapped>' to 'Copyable'}}

struct NC: ~Copyable {}

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
