// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

struct S {}
struct R {}

protocol ReturnsMetatype {
  associatedtype Assoc
  mutating
  func getAssocMetatype() -> Assoc.Type
}

struct ConformsToReturnsMetatype : ReturnsMetatype {
  // CHECK-LABEL: sil private [transparent] [thunk] @_T018result_abstraction25ConformsToReturnsMetatypeVAA0eF0A2aDP08getAssocF0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: ReturnsMetatype) (@inout ConformsToReturnsMetatype) -> @thick S.Type
  // CHECK:         function_ref @_T018result_abstraction25ConformsToReturnsMetatypeV08getAssocF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsMetatype) -> @thin S.Type
  mutating
  func getAssocMetatype() -> S.Type {
    return S.self
  }
}

protocol ReturnsFunction {
  associatedtype Arg
  associatedtype Result
  func getFunc() -> (Arg) -> Result
}

struct ConformsToReturnsFunction : ReturnsFunction {
  // CHECK-LABEL: sil private [transparent] [thunk] @_T018result_abstraction25ConformsToReturnsFunctionVAA0eF0A2aDP7getFunc{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: ReturnsFunction) (@in_guaranteed ConformsToReturnsFunction) -> @owned @callee_guaranteed (@in S) -> @out R
  // CHECK:         function_ref @_T018result_abstraction1SVAA1RVIegyd_AcEIegir_TR : $@convention(thin) (@in S, @guaranteed @callee_guaranteed (S) -> R) -> @out R
  func getFunc() -> (S) -> R {
    return {s in R()}
  }
}

protocol ReturnsAssoc {
  associatedtype Assoc
  mutating
  func getAssoc() -> Assoc
}

struct ConformsToReturnsAssocWithMetatype : ReturnsAssoc {
  typealias Assoc = S.Type
  // CHECK-LABEL: sil private [transparent] [thunk] @_T018result_abstraction34ConformsToReturnsAssocWithMetatypeVAA0eF0A2aDP03getF0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: ReturnsAssoc) (@inout ConformsToReturnsAssocWithMetatype) -> @out @thick S.Type
  // CHECK:         function_ref @_T018result_abstraction34ConformsToReturnsAssocWithMetatypeV03getF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsAssocWithMetatype) -> @thin S.Type
  mutating
  func getAssoc() -> S.Type {
    return S.self
  }
}

struct ConformsToReturnsAssocWithFunction : ReturnsAssoc {
  typealias Assoc = (S) -> R
  // CHECK-LABEL: sil private [transparent] [thunk] @_T018result_abstraction34ConformsToReturnsAssocWithFunctionVAA0eF0A2aDP03getF0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method: ReturnsAssoc) (@inout ConformsToReturnsAssocWithFunction) -> @out @callee_guaranteed (@in S) -> @out R
  // CHECK:         function_ref @_T018result_abstraction34ConformsToReturnsAssocWithFunctionV03getF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsAssocWithFunction) -> @owned @callee_guaranteed (S) -> R
  mutating
  func getAssoc() -> (S) -> R {
    return {s in R()}
  }
}
