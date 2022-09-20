// RUN: %target-typecheck-verify-swift -parse-as-library

protocol BaseProto {
  associatedtype AssocTy
}
var a: BaseProto.AssocTy = 4
// expected-error@-1{{cannot access associated type 'AssocTy' from 'BaseProto'; use a concrete type or generic parameter base instead}}

var a = BaseProto.AssocTy.self
// expected-error@-1{{cannot access associated type 'AssocTy' from 'BaseProto'; use a concrete type or generic parameter base instead}}

protocol DerivedProto : BaseProto {
  func associated() -> AssocTy // no-warning

  func existential() -> BaseProto.AssocTy
  // expected-error@-1{{cannot access associated type 'AssocTy' from 'BaseProto'; use a concrete type or generic parameter base instead}}
}


func generic<T: BaseProto>(_: T, _ assoc: T.AssocTy) {} // no-warning

