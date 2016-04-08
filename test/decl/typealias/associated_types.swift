// RUN: %target-parse-verify-swift -parse-as-library

protocol BaseProto {
  associatedtype AssocTy
}
var a: BaseProto.AssocTy = 4 // expected-error{{cannot use associated type 'AssocTy' outside of its protocol}}


protocol DerivedProto : BaseProto {
  func associated() -> AssocTy // no-warning

  func existential() -> BaseProto.AssocTy // expected-error{{cannot use associated type 'AssocTy' outside of its protocol}}
}


func generic<T: BaseProto>(_ assoc: T.AssocTy) {} // no-warning

