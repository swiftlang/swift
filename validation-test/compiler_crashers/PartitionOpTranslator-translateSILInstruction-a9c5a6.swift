// {"extraArgs":["-enable-experimental-feature","BuiltinModule","-language-mode","6"],"kind":"emit-sil","signature":"swift::regionanalysisimpl::PartitionOpTranslator::translateSILInstruction(swift::SILInstruction*)::'lambda0'()::operator()() const","signatureAssert":"Assertion failed: ((TranslationSemantics::LookThrough != TranslationSemantics::LookThrough || isStaticallyLookThroughInst(inst)) && \"Out of sync?!\"), function visitMakeBorrowInst","signatureNext":"PartitionOpTranslator::translateSILBasicBlock"}
// RUN: not --crash %target-swift-frontend -emit-sil -enable-experimental-feature BuiltinModule -language-mode 6 %s
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_BuiltinModule
import Builtin
func f(_ r: Int) {
  _ = Builtin.makeBorrow(r)
}
