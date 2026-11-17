// {"extraArgs":["-experimental-allow-module-with-compiler-errors","-emit-module-path","/dev/null"],"kind":"emit-sil","original":"bf67dfa3","signature":"swift::TypeBase::computeCanonicalType()","signatureAssert":"Assertion failed: (Result->isCanonical()), function computeCanonicalType","signatureNext":"Serializer::DeclSerializer::visitFuncDecl"}
// RUN: not --crash %target-swift-frontend -emit-sil -experimental-allow-module-with-compiler-errors -emit-module-path /dev/null %s
protocol a where b == Self
}
c {
  protocol d {
      e<each f: a>(repeat each f)
