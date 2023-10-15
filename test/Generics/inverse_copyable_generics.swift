// RUN: %target-typecheck-verify-swift -enable-experimental-feature NoncopyableGenerics

// REQUIRES: asserts

public struct Cond<T: ~Copyable>: ~Copyable {}
extension Cond: Copyable where T: Copyable {} // expected-note {{requirement from conditional conformance}}

public typealias AlwaysCopyable<T> = Cond<T>
public typealias CondAlias<T> = Cond<T> where T: ~Copyable

func checkCopyable<T>(_ t: T) {}

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
