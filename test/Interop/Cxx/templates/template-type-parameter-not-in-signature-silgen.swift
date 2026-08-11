// RUN: %target-swift-emit-silgen %s -cxx-interoperability-mode=default -I %S/Inputs \
// RUN: | %FileCheck %s
//
// RUN: %target-swift-emit-silgen %s -cxx-interoperability-mode=default -I %S/Inputs \
// RUN:   -enable-experimental-feature ImportCxxMembersLazily \
// RUN: | %FileCheck %s
//
// REQUIRES: swift_feature_ImportCxxMembersLazily

import TemplateTypeParameterNotInSignature

_ = templateTypeParamNotUsedInSignature(T: Int.self)
_ = templateTypeParamNotUsedInSignature(T: Bool.self)


// CHECK: sil [transparent] [serialized] [ossa] @$sSC35templateTypeParamNotUsedInSignatureySbSimF : $@convention(thin) (@thin Int.Type) -> Bool {
// CHECK: bb0(%0 : $@thin Int.Type):
// CHECK:   {{.*}} = function_ref @$sSo69__swift_specializedThunk__Z35templateTypeParamNotUsedInSignatureI{{l|x}}EbvSbyFTo : $@convention(c) () -> Bool
// CHECK: } // end sil function '$sSC35templateTypeParamNotUsedInSignatureySbSimF'

// CHECK: sil [transparent] [serialized] [ossa] @$sSC35templateTypeParamNotUsedInSignatureyS2bmF : $@convention(thin) (@thin Bool.Type) -> Bool {
// CHECK: bb0(%0 : $@thin Bool.Type):
// CHECK:   {{.*}} = function_ref @$sSo69__swift_specializedThunk__Z35templateTypeParamNotUsedInSignatureIbEbvSbyFTo : $@convention(c) () -> Bool
// CHECK: } // end sil function '$sSC35templateTypeParamNotUsedInSignatureyS2bmF'

_ = unnamedParamNotUsedInSignature(4, T: Int.self)

// CHECK: sil [transparent] [serialized] [ossa] @$sSC30unnamedParamNotUsedInSignatureys5Int32VAC_SimtF : $@convention(thin) (Int32, @thin Int.Type) -> Int32 {
// CHECK: bb0([[ARG:%.*]] : $Int32, %{{.*}} : $@thin Int.Type):
// CHECK:   debug_value [[ARG]], let, name "__argument0"
// CHECK:   [[FN:%.*]] = function_ref @$sSo{{.*}}__swift_specializedThunk{{.*}} : $@convention(c) (Int32) -> Int32
// CHECK:   {{.*}} = apply [[FN]]([[ARG]])
// CHECK: } // end sil function '$sSC30unnamedParamNotUsedInSignatureys5Int32VAC_SimtF'

_ = mixedNamedParamsNotUsedInSignature(1, 2, T: Int.self)

// CHECK: sil [transparent] [serialized] [ossa] @$sSC34mixedNamedParamsNotUsedInSignatureys5Int32VAC_ACSimtF : $@convention(thin) (Int32, Int32, @thin Int.Type) -> Int32 {
// CHECK: bb0([[X:%.*]] : $Int32, [[UNNAMED:%.*]] : $Int32, %{{.*}} : $@thin Int.Type):
// CHECK-DAG:   debug_value [[X]], let, name "x"
// CHECK-DAG:   debug_value [[UNNAMED]], let, name "__argument1"
// CHECK:   [[FN2:%.*]] = function_ref @$sSo{{.*}}__swift_specializedThunk{{.*}} : $@convention(c) (Int32, Int32) -> Int32
// CHECK:   {{.*}} = apply [[FN2]]([[X]], [[UNNAMED]])
// CHECK: } // end sil function '$sSC34mixedNamedParamsNotUsedInSignatureys5Int32VAC_ACSimtF'

_ = unnamedAndDeducedParamsNotUsedInSignature(1, 2 as CInt, T: Int.self)

// CHECK: sil [transparent] [serialized] [ossa] @$sSC41unnamedAndDeducedParamsNotUsedInSignatureys5Int32VAC_ACSimtF : $@convention(thin) (Int32, Int32, @thin Int.Type) -> Int32 {
// CHECK: bb0([[U0:%.*]] : $Int32, [[U1:%.*]] : $Int32, %{{.*}} : $@thin Int.Type):
// CHECK-DAG:   debug_value [[U0]], let, name "__argument0"
// CHECK-DAG:   debug_value [[U1]], let, name "u"
// CHECK: } // end sil function '$sSC41unnamedAndDeducedParamsNotUsedInSignatureys5Int32VAC_ACSimtF'

let s = Struct()
_ = s.unnamedParamNotUsedInSignature(4, T: Int.self)
_ = Struct.unnamedParamNotUsedInSignatureStatic(4, T: Int.self)

// CHECK: sil shared [transparent] [serialized] [ossa] @$sSo6StructV30unnamedParamNotUsedInSignatureys5Int32VAE_SimtF : $@convention(method) (Int32, @thin Int.Type, Struct) -> Int32 {
// CHECK: bb0([[MARG:%.*]] : $Int32, %{{.*}} : $@thin Int.Type, %{{.*}} : $Struct):
// CHECK:   debug_value [[MARG]], let, name "__argument0"
// CHECK: } // end sil function '$sSo6StructV30unnamedParamNotUsedInSignatureys5Int32VAE_SimtF'

// CHECK: sil shared [transparent] [serialized] [ossa] @$sSo6StructV36unnamedParamNotUsedInSignatureStaticys5Int32VAE_SimtFZ : $@convention(method) (Int32, @thin Int.Type, @thin Struct.Type) -> Int32 {
// CHECK: bb0([[SARG:%.*]] : $Int32, %{{.*}} : $@thin Int.Type, %{{.*}} : $@thin Struct.Type):
// CHECK:   debug_value [[SARG]], let, name "__argument0"
// CHECK: } // end sil function '$sSo6StructV36unnamedParamNotUsedInSignatureStaticys5Int32VAE_SimtFZ'
