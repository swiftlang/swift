// {"kind":"complete","original":"1bd67425","signature":"swift::diagnoseMissingOwnership(swift::ParamSpecifier, swift::TypeRepr*, swift::Type, swift::TypeResolution const&)","signatureAssert":"Assertion failed: (!type->hasTypeParameter() && \"no generic environment provided for type with type parameters\"), function mapTypeIntoContext"}
// RUN: not --crash %target-swift-ide-test -code-completion -batch-code-completion -skip-filecheck -code-completion-diagnostics -source-filename %s
@a(0#^^#-> b->func > <b
