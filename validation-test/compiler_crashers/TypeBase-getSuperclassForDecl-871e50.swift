// {"kind":"typecheck","original":"08496a0b","signature":"swift::TypeBase::getSuperclassForDecl(swift::ClassDecl const*, bool)","signatureAssert":"Assertion failed: (baseClass->isSuperclassOf(currentClass) && \"no inheritance relationship between given classes\"), function getSuperclassForDecl","signatureNext":"TypeBase::getContextSubstitutions"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
class a < b {
  @globalActor actor d {
    static shared = @d
    class c
      class e : c
