// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

struct S {}
struct R {}

protocol ReturnsMetatype {
  associatedtype Assoc
  mutating
  func getAssocMetatype() -> Assoc.Type
}

struct ConformsToReturnsMetatype : ReturnsMetatype {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_T018result_abstraction25ConformsToReturnsMetatypeVAA0eF0AaaDP08getAssocF0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method) (@inout ConformsToReturnsMetatype) -> @thick S.Type
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_T018result_abstraction25ConformsToReturnsFunctionVAA0eF0AaaDP7getFunc{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method) (@in_guaranteed ConformsToReturnsFunction) -> @owned @callee_owned (@in S) -> @out R
  // CHECK:         function_ref @_T018result_abstraction1SVAA1RVIxyd_AcEIxir_TR : $@convention(thin) (@in S, @owned @callee_owned (S) -> R) -> @out R
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_T018result_abstraction34ConformsToReturnsAssocWithMetatypeVAA0eF0AaaDP03getF0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method) (@inout ConformsToReturnsAssocWithMetatype) -> @out @thick S.Type
  // CHECK:         function_ref @_T018result_abstraction34ConformsToReturnsAssocWithMetatypeV03getF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsAssocWithMetatype) -> @thin S.Type
  mutating
  func getAssoc() -> S.Type {
    return S.self
  }
}

struct ConformsToReturnsAssocWithFunction : ReturnsAssoc {
  typealias Assoc = (S) -> R
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_T018result_abstraction34ConformsToReturnsAssocWithFunctionVAA0eF0AaaDP03getF0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method) (@inout ConformsToReturnsAssocWithFunction) -> @out @callee_owned (@in S) -> @out R
  // CHECK:         function_ref @_T018result_abstraction34ConformsToReturnsAssocWithFunctionV03getF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsAssocWithFunction) -> @owned @callee_owned (S) -> R
  mutating
  func getAssoc() -> (S) -> R {
    return {s in R()}
  }
}
