// {"extraArgs":["-enable-library-evolution"],"kind":"typecheck","original":"2f0860f3","signature":"swift::TypeChecker::inferDefaultWitnesses(swift::ProtocolDecl*)","signatureNext":"DeclChecker::visit"}
// RUN: not --crash %target-swift-frontend -typecheck -enable-library-evolution %s
public
  protocol a: Collection where SubSequence: a
{
}
