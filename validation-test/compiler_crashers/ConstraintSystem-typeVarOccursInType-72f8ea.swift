// {"kind":"typecheck","original":"5743d696","signature":"swift::constraints::ConstraintSystem::typeVarOccursInType(swift::TypeVariableType*, swift::Type, bool*)","signatureAssert":"Assertion failed: ((!typeVariables.empty() || hasError()) && \"Did not find type variables!\"), function getTypeVariables","signatureNext":"isBindable"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
struct a < each b {
  @globalActor struct d {
    static shared = @d
    func c {
      c
