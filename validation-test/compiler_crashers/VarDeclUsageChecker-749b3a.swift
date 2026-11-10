// {"kind":"typecheck","original":"f7236a3b","signature":"(anonymous namespace)::VarDeclUsageChecker::~VarDeclUsageChecker()","signatureAssert":"Assertion failed: (isa<To>(Val) && \"cast<Ty>() argument of incompatible type!\"), function cast","signatureNext":"performAbstractFuncDeclDiagnostics"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
protocol a {
  class b {
    c { guard let d = c as? Any is a
      }
  }
