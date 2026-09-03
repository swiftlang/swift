// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-silgen -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces %s \
// RUN:   > %t/legacy.sil
// RUN: %target-swift-frontend -emit-silgen -parse-as-library \
// RUN:   -module-name main -enable-experimental-feature Namespaces \
// RUN:   -enable-experimental-feature ParserASTGen %s > %t/astgen.sil
// RUN: %diff -u %t/legacy.sil %t/astgen.sil
// RUN: %FileCheck %s --check-prefix=SIL \
// RUN:   --implicit-check-not='$s4main10BuildProofyXZ6answerSiyFZ' \
// RUN:   --implicit-check-not='namespace<BuildProof>' < %t/legacy.sil

// REQUIRES: swift_swift_parser
// REQUIRES: swift_feature_ParserASTGen
// REQUIRES: swift_feature_Namespaces
// UNSUPPORTED: asan

namespace BuildProof {
  static func answer() -> Int { 42 }
}

// SIL-LABEL: sil hidden [ossa] @$s4main10BuildProofyXZ6answerSiyF : $@convention(thin) () -> Int {
// SIL: return

// SIL-LABEL: sil hidden [ossa] @$s4main10callAnswerSiyF : $@convention(thin) () -> Int {
// SIL: function_ref @$s4main10BuildProofyXZ6answerSiyF : $@convention(thin) () -> Int
// SIL: apply {{%.*}}() : $@convention(thin) () -> Int
func callAnswer() -> Int {
  BuildProof.answer()
}

// SIL-LABEL: sil hidden [ossa] @$s4main28callNamespaceFunctionPointerSiyF : $@convention(thin) () -> Int {
// SIL: function_ref @$s4main10BuildProofyXZ6answerSiyF : $@convention(thin) () -> Int
func callNamespaceFunctionPointer() -> Int {
  let function: @convention(c) () -> Int = BuildProof.answer
  return function()
}
