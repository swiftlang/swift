// RUN: %swift -parse -parse-as-library %s -verify

protocol BaseProto {
  typealias AssocTy
}
var a: BaseProto.AssocTy // expected-error{{cannot use associated type 'AssocTy' outside of its protocol}}


protocol DerivedProto : BaseProto {
  func associated() -> AssocTy {} // no-warning

  func existential() -> BaseProto.AssocTy {} // expected-error{{cannot use associated type 'AssocTy' outside of its protocol}}
}


func generic<T: BaseProto>(assoc: T.AssocTy) {} // no-warning

