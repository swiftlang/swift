// {"kind":"complete","original":"459eeccf","signature":"swift::diagnoseMissingOwnership(swift::ParamSpecifier, swift::TypeRepr*, swift::Type, swift::TypeResolution const&)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoEnvironment","signatureNext":"validateParameterType"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
macro a<b>() =
  {
    class c {
      #^^#      init(d: b)
    }
  }
