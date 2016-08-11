// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct S {}
struct R {}

protocol ReturnsMetatype {
  associatedtype Assoc
  mutating
  func getAssocMetatype() -> Assoc.Type
}

struct ConformsToReturnsMetatype : ReturnsMetatype {
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV18result_abstraction25ConformsToReturnsMetatypeS_15ReturnsMetatypeS_FS1_16getAssocMetatype{{.*}} : $@convention(witness_method) (@inout ConformsToReturnsMetatype) -> @thick S.Type
  // CHECK:         function_ref @_TFV18result_abstraction25ConformsToReturnsMetatype16getAssocMetatype{{.*}} : $@convention(method) (@inout ConformsToReturnsMetatype) -> @thin S.Type
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV18result_abstraction25ConformsToReturnsFunctionS_15ReturnsFunctionS_FS1_7getFunc{{.*}} : $@convention(witness_method) (@in_guaranteed ConformsToReturnsFunction) -> @owned @callee_owned (@in S) -> @out R
  // CHECK:         function_ref @_TTRXFo_dV18result_abstraction1S_dVS_1R_XFo_iS0__iS1__ : $@convention(thin) (@in S, @owned @callee_owned (S) -> R) -> @out R
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
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV18result_abstraction34ConformsToReturnsAssocWithMetatypeS_12ReturnsAssocS_FS1_8getAssoc{{.*}} : $@convention(witness_method) (@inout ConformsToReturnsAssocWithMetatype) -> @out @thick S.Type
  // CHECK:         function_ref @_TFV18result_abstraction34ConformsToReturnsAssocWithMetatype8getAssoc{{.*}} : $@convention(method) (@inout ConformsToReturnsAssocWithMetatype) -> @thin S.Type
  mutating
  func getAssoc() -> S.Type {
    return S.self
  }
}

struct ConformsToReturnsAssocWithFunction : ReturnsAssoc {
  typealias Assoc = (S) -> R
  // CHECK-LABEL: sil hidden [transparent] [thunk] @_TTWV18result_abstraction34ConformsToReturnsAssocWithFunctionS_12ReturnsAssocS_FS1_8getAssoc{{.*}} : $@convention(witness_method) (@inout ConformsToReturnsAssocWithFunction) -> @out @callee_owned (@in S) -> @out R
  // CHECK:         function_ref @_TFV18result_abstraction34ConformsToReturnsAssocWithFunction8getAssoc{{.*}} : $@convention(method) (@inout ConformsToReturnsAssocWithFunction) -> @owned @callee_owned (S) -> R
  mutating
  func getAssoc() -> (S) -> R {
    return {s in R()}
  }
}
