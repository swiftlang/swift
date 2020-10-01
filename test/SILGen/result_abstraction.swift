
// RUN: %target-swift-emit-silgen -module-name result_abstraction %s | %FileCheck %s

struct S {}
struct R {}

protocol ReturnsMetatype {
  associatedtype Assoc
  mutating
  func getAssocMetatype() -> Assoc.Type
}

struct ConformsToReturnsMetatype : ReturnsMetatype {
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s18result_abstraction25ConformsToReturnsMetatypeVAA0eF0A2aDP08getAssocF0{{[_0-9a-zA-Z]*}}FTW :
  // CHECK:         function_ref @$s18result_abstraction25ConformsToReturnsMetatypeV08getAssocF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsMetatype) -> @thin S.Type
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
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s18result_abstraction25ConformsToReturnsFunctionVAA0eF0A2aDP7getFunc{{[_0-9a-zA-Z]*}}FTW :
  // CHECK:         function_ref @$s18result_abstraction1SVAA1RVIegyd_AcEIegnr_TR : $@convention(thin) (@in_guaranteed S, @guaranteed @callee_guaranteed (S) -> R) -> @out R
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
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s18result_abstraction34ConformsToReturnsAssocWithMetatypeVAA0eF0A2aDP03getF0{{[_0-9a-zA-Z]*}}FTW :
  // CHECK:         function_ref @$s18result_abstraction34ConformsToReturnsAssocWithMetatypeV03getF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsAssocWithMetatype) -> @thin S.Type
  mutating
  func getAssoc() -> S.Type {
    return S.self
  }
}

struct ConformsToReturnsAssocWithFunction : ReturnsAssoc {
  typealias Assoc = (S) -> R
  // CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s18result_abstraction34ConformsToReturnsAssocWithFunctionVAA0eF0A2aDP03getF0{{[_0-9a-zA-Z]*}}FTW :
  // CHECK:         function_ref @$s18result_abstraction34ConformsToReturnsAssocWithFunctionV03getF0{{[_0-9a-zA-Z]*}}F : $@convention(method) (@inout ConformsToReturnsAssocWithFunction) -> @owned @callee_guaranteed (S) -> R
  mutating
  func getAssoc() -> (S) -> R {
    return {s in R()}
  }
}
