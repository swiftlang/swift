// RUN: %target-swift-emit-silgen %s -cxx-interoperability-mode=default -I %S/Inputs | %FileCheck %s

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
