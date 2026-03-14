// RUN: %target-typecheck-verify-swift

struct S<A> {
  let v:any P<A>
  func map<B> (_ f:(A) -> B) -> S<B> {
    v.map(f).k()
  }
}

protocol P<A> {
  associatedtype A
  func map<D> (_ g:(A) -> D) -> any P<D>
  func k() -> S<A>
}
