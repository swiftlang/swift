// rdar://163324789 (https://github.com/swiftlang/swift/issues/85110)
// RUN: %target-typecheck-verify-swift

struct CasePath<Root, Value> {}

protocol CasePathable {
  associatedtype AllCasePaths
}

@dynamicMemberLookup
struct Case<Value> {
  subscript<Member>(dynamicMember keyPath: KeyPath<Value.AllCasePaths, CasePath<Value, Member>>) -> Case<Member> where Value: CasePathable {
    fatalError()
  }
}

protocol P<A> {
  associatedtype A
}

struct S<A>: P {}

extension P {
  func forEach(_: KeyPath<Case<A>, Case<[Int]>>) -> S<A> { S() }
}

enum E: CasePathable {
  case a(Int)

  struct AllCasePaths {
    var a: CasePath<E, Int> { fatalError() }
  }
}

func f<R: P<E>>(_: R) {}

f(S().forEach(\.a)) // expected-error {{failed to produce diagnostic for expression; please submit a bug report}}
