// {"kind":"typecheck","signature":"isParamListRepresentableInLanguage(swift::AbstractFunctionDecl const*, swift::ParameterList const*, swift::ObjCReason)","signatureAssert":"Assertion failed: (Ptr && \"Cannot dereference a null Type!\"), function operator->"}
// RUN: not --crash %target-swift-frontend -typecheck %s
// REQUIRES: objc_interop
class a { @objc b(@c & d Int) @propertyWrapper enum c
