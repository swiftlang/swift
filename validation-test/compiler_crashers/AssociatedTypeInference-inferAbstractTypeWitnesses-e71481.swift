// {"kind":"typecheck","signature":"(anonymous namespace)::AssociatedTypeInference::inferAbstractTypeWitnesses(llvm::ArrayRef<swift::AssociatedTypeDecl*>, unsigned int)","signatureAssert":"Assertion failed: (!tyWitness.DefaultedAssocType && \"already recorded a default type witness\"), function addDefaultTypeWitness","signatureNext":"AssociatedTypeInference::findSolutionsRec"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a : b protocol b {
  typealias c typealias c : = c
