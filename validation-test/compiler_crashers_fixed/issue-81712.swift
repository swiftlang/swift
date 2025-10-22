// RUN: %target-typecheck-verify-swift

protocol Id {
  associatedtype ID
}

protocol P<Id1> {
  associatedtype Id1: Id
  associatedtype Id2: Id where Id2.ID == Id1.ID

  func foo(_: Id2.ID)
}

struct MyId: Id {
  typealias ID = Int
}

func f(id: Int, cache: any P<MyId>) {
  cache.foo(id)
}
