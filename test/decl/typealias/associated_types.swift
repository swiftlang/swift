// RUN: %target-typecheck-verify-swift -parse-as-library

protocol BaseProto {
  associatedtype AssocTy
}
var a: BaseProto.AssocTy = 4
// expected-error@-1{{associated type 'AssocTy' can only be used with a concrete type or generic parameter base}}

var a = BaseProto.AssocTy.self
// expected-error@-1{{associated type 'AssocTy' can only be used with a concrete type or generic parameter base}}

protocol DerivedProto : BaseProto {
  func associated() -> AssocTy // no-warning

  func existential() -> BaseProto.AssocTy
  // expected-error@-1{{associated type 'AssocTy' can only be used with a concrete type or generic parameter base}}
}


func generic<T: BaseProto>(_: T, _ assoc: T.AssocTy) {} // no-warning

