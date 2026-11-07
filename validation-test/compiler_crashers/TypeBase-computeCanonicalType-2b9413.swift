// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"bf67dfa3","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"Serializer::DeclSerializer::visitFuncDecl"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
protocol a where b == Self
}
c {
  protocol d {
      e<each f: a>(repeat each f)
