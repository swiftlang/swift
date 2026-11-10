// {"kind":"typecheck","signature":"isParamListRepresentableInLanguage(swift::AbstractFunctionDecl const*, swift::ParameterList const*, swift::ObjCReason)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->","signatureNext":"isRepresentableInLanguage"}
// The crash depends on the C++ parser's handling of this input, which ASTGen
// handles differently. Expect it to fail, so that we notice if it crashes again.
// XFAIL: parser_astgen
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a { @objc b(@e & d Int) @propertyWrapper enum e
