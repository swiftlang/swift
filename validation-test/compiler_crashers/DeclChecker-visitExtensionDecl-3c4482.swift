// {"extraArgs":["-enable-experimental-feature","TupleConformances"],"kind":"emit-ir","signature":"(anonymous namespace)::DeclChecker::visitExtensionDecl(swift::ExtensionDecl*)","signatureAssert":"Assertion failed: (false && \"Huh?\"), function isValidExtendedTypeForTupleExtension","signatureNext":"DeclChecker::visit"}
// RUN: not --crash %target-swift-frontend -emit-ir -enable-experimental-feature TupleConformances %s
// REQUIRES: swift_feature_TupleConformances
typealias a = (repeat b)
extension a {
}
